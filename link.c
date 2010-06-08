#define _GNU_SOURCE
#define GL_GLEXT_PROTOTYPES
#include <err.h>
#include <errno.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <pthread.h>
#include <sys/mman.h>
#include <sys/poll.h>
#include <byteswap.h>

#include <GL/gl.h>
#include <GL/glext.h>
#include <GL/glx.h>

#include <caml/fail.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/unixsupport.h>

#include "fitz/fitz.h"
#include "mupdf/mupdf.h"

#include <sys/time.h>

static long pagesize;

struct tex {
    int index;
    int h;
};

struct page {
    int pagenum;
    fz_pixmap *pixmap;
    GLuint texid;
    struct page2 *page2;
    struct page *prev;
    struct tex texs[];
};

struct page2 {
    fz_bbox bbox;
    fz_matrix ctm;
    fz_pixmap pixmap;
    int pagenum;
};

struct {
    int sock;
    pthread_t thread;
    struct page *pages;
    struct page2 *pages2;
    int page;
    int pagecount;
    pdf_xref *xref;
    fz_glyphcache *cache;
    int w, h;
    Display *dpy;
    GLXContext ctx;
    GLXDrawable drawable;

    int texid;
    int texcount;
    int slicecount;
    GLuint *texids;
    struct tex **texowners;
} state;

static double now (void)
{
    struct timeval tv;

    if (gettimeofday (&tv, NULL)) {
        err (1, "gettimeofday");
    }
    return tv.tv_sec + tv.tv_usec*1e-6;
}

static void readdata (int fd, char *p, int size)
{
    ssize_t n;

    n = read (fd, p, size);
    if (n - size) {
        err (1, "read (req %d, ret %zd)", size, n);
    }
}

static void writedata (int fd, char *p, int size)
{
    char buf[4];
    ssize_t n;

    buf[0] = (size >> 24) & 0xff;
    buf[1] = (size >> 16) & 0xff;
    buf[2] = (size >>  8) & 0xff;
    buf[3] = (size >>  0) & 0xff;

    n = write (fd, buf, 4);
    if (n != 4) {
        err (1, "write %zd", n);
    }

    n = write (fd, p, size);
    if (n - size) {
        err (1, "write (req %d, ret %zd)", size, n);
    }
}

static void __attribute__ ((format (printf, 2, 3)))
    printd (int fd, const char *fmt, ...)
{
    int len;
    va_list ap;
    char buf[200];

    va_start (ap, fmt);
    len = vsnprintf (buf, sizeof (buf), fmt, ap);
    va_end (ap);
    writedata (fd, buf, len);
}

static void closexref (void);

static void createmmap (struct page *page)
{
    int fd, ret;
    size_t size;

    size = page->pixmap->w * page->pixmap->h * 4;

    fd = open ("pdfmap", O_CREAT|O_TRUNC|O_RDWR);
    if (fd == -1) {
        err (1, "open");
    }
    ret = unlink ("pdfmap");
    if (ret) {
        err (1, "unlink");
    }
    size = (size + pagesize - 1) & ~(pagesize - 1);
    ret = ftruncate (fd, size);
    if (ret) {
        err (1, "ftruncate");
    }
    page->pixmap->samples = mmap (NULL, size, PROT_READ|PROT_WRITE,
                                  MAP_PRIVATE, fd, 0);
    if (page->pixmap->samples == MAP_FAILED) {
        err (1, "mmap");
    }
}

static void die(fz_error error)
{
    fz_catch(error, "aborting");
    closexref();
    exit(1);
}

void openxref(char *filename, char *password, int dieonbadpass)
{
    fz_stream *file;
    int okay;
    int fd;
    char *basename;

    basename = strrchr(filename, '/');
    if (!basename)
        basename = filename;
    else
        basename++;

    fd = open(filename, O_BINARY | O_RDONLY, 0666);
    if (fd < 0)
        die(fz_throw("cannot open file '%s'", filename));

    file = fz_openfile(fd);
    state.xref = pdf_openxref(file);
    if (!state.xref)
        die(fz_throw("cannot open PDF file '%s'", basename));
    fz_dropstream(file);

    if (pdf_needspassword(state.xref))
    {
        okay = pdf_authenticatepassword(state.xref, password);
        if (!okay && !dieonbadpass)
            fz_warn("invalid password, attempting to continue.");
        else if (!okay && dieonbadpass)
            die(fz_throw("invalid password"));
    }

    state.pagecount = pdf_getpagecount(state.xref);
    printd (state.sock, "C %d", state.pagecount);
}

static void flushxref(void)
{
    if (state.xref)
    {
        pdf_flushxref(state.xref, 0);
    }
}

static void closexref(void)
{
    if (state.xref)
    {
        pdf_closexref(state.xref);
        state.xref = nil;
    }
}

static int readlen (int fd)
{
    ssize_t n;
    char p[4];

    n = read (fd, p, 4);
    if (n != 4) {
        err (1, "read %zd", n);
    }

    return (p[0] << 24) | (p[1] << 16) | (p[2] << 8) | p[3];
}

static void freepage (struct page *page)
{
    int i;
    struct page *p;

    fz_droppixmap (page->pixmap);
    for (p = state.pages; p; p = p->prev) {
        if (p->prev == page) {
            p->prev = page->prev;
            break;
        }
    }
    for (i = 0; i < state.slicecount; ++i) {
        struct tex *t = &p->texs[i];
        state.texowners[t->index] = NULL;
    }
    free (page);
}

static void subdivide (struct page *p)
{
    int i;
    int h = p->pixmap->h;

    if (state.slicecount == 1) {
        struct tex *t = &p->texs[0];
        t->index = -1;
        t->h = h;
    }
    else {
        int th = h / (state.slicecount - 1);

        for (i = 0; i < state.slicecount; ++i) {
            struct tex *t = &p->texs[i];
            t->index = -1;
            t->h = MIN (th, h);
            h -= th;
        }
    }
}

static void *render (int pagenum, int pindex)
{
    fz_error error;
    fz_obj *pageobj;
    int w, h;
    float zoom;
    struct page *page;
    struct page2 *page2;
    fz_device *idev, *tdev, *mdev;
    fz_displaylist *list;
    pdf_page *drawpage;

    printf ("render %d %d\n", pagenum, pindex);
    pdf_flushxref (state.xref, 0);
    page = calloc (sizeof (*page) + (state.slicecount * sizeof (struct tex)), 1);
    if (!page) {
        err (1, "malloc page %d\n", pagenum);
    }
    page->prev = state.pages;
    state.pages = page;

    page2 = &state.pages2[pindex];

    pageobj = pdf_getpageobject(state.xref, pagenum);
    if (!pageobj)
        die (fz_throw ("cannot retrieve info from page %d", pagenum));

    error = pdf_loadpage(&drawpage, state.xref, pageobj);
    if (error)
        die(error);

    page->pixmap = fz_newpixmapwithrect (pdf_devicergb, page2->bbox);
    if (error)
        die (error);
#if 0
    fz_free (page->pixmap->samples);
    createmmap (page);
#endif
    fz_clearpixmap(page->pixmap, 0xFF);

    list = fz_newdisplaylist ();
    if (!list)
        die (fz_throw ("fz_newdisplaylist failed"));

    mdev = fz_newlistdevice(list);
    error = pdf_runcontentstream(mdev, fz_identity(), state.xref,
                                 drawpage->resources,
                                 drawpage->contents);
    if (error)
        die (error);

    fz_freedevice(mdev);

    idev = fz_newdrawdevice (state.cache, page->pixmap);
    if (!idev)
        die (fz_throw ("fz_newdrawdevice failed"));

    fz_executedisplaylist(list, idev, page2->ctm);
    fz_freedevice(idev);
    fz_freedisplaylist(list);

    /* fz_debugpixmap (page->pixmap, "haha"); */
    pdf_droppage (drawpage);
    page->page2 = page2;
    page->pagenum = pagenum;
    subdivide (page);
    return page;
}

static void layout1 (void)
{
    int pagenum;
    double a, b;
    int prevrotate;
    fz_rect prevbox;
    int i, pindex;
    asize_t size;
    int64 mapsize;
    struct page2 *p;

    size = 0;
    pindex = 0;
    mapsize = 0;
    a = now ();
    for (pagenum = 1; pagenum <= state.pagecount; ++pagenum) {
        float w;
        float zoom;
        int rotate;
        fz_obj *obj;
        fz_rect box;
        fz_rect box2;
        fz_matrix ctm;
        fz_bbox bbox;
        fz_error error;
        fz_obj *pageobj;

        pageobj = pdf_getpageobject (state.xref, pagenum);
        if (!pageobj)
            die (fz_throw ("cannot retrieve info from page %d", pagenum));

        obj = fz_dictgets (pageobj, "CropBox");
        if (!fz_isarray(obj)) {
            obj = fz_dictgets (pageobj, "MediaBox");
            if (!fz_isarray (obj))
                die (fz_throw ("cannot find page bounds %d (%d R)",
                               fz_tonum (obj), fz_togen (obj)));
        }
        box = pdf_torect (obj);

        obj = fz_dictgets (pageobj, "Rotate");
        if (fz_isint (obj))
            rotate = fz_toint (obj);
        else
            rotate = 0;

        if (pagenum != 1
            && (prevrotate == rotate
                && !memcmp (&prevbox, &box, sizeof (box)))) {
            continue;
        }

        memcpy (&prevbox, &box, sizeof (box));
        prevrotate = rotate;

        box.x0 = MIN (prevbox.x0, prevbox.x1);
        box.y0 = MIN (prevbox.y0, prevbox.y1);
        box.x1 = MAX (prevbox.x0, prevbox.x1);
        box.y1 = MAX (prevbox.y0, prevbox.y1);

        ctm = fz_identity ();
        ctm = fz_concat (ctm, fz_translate (0, -box.y1));
        ctm = fz_concat (ctm, fz_rotate (rotate));
        box2 = fz_transformrect (ctm, box);
        w = box2.x1 - box2.x0;

        zoom = state.w / w;
        ctm = fz_identity ();
        ctm = fz_concat (ctm, fz_translate (0, -box.y1));
        ctm = fz_concat (ctm, fz_scale (zoom, -zoom));
        ctm = fz_concat (ctm, fz_rotate (rotate));
        bbox = fz_roundrect (fz_transformrect (ctm, box));

        size += sizeof (*state.pages2);
        state.pages2 = caml_stat_resize (state.pages2, size);

        p = &state.pages2[pindex++];
        memcpy (&p->bbox, &bbox, sizeof (bbox));
        memcpy (&p->ctm, &ctm, sizeof (ctm));

        p->pagenum = pagenum - 1;
        p->pixmap.x = bbox.x0;
        p->pixmap.y = bbox.y0;
        p->pixmap.w = bbox.x1 - bbox.x0;
        p->pixmap.h = bbox.y1 - bbox.y0;
        p->pixmap.n = 4;
    }

    for (i = pindex - 1; i >= 0; --i) {
        p = &state.pages2[i];
        printd (state.sock, "l %d %d %d",
                p->pagenum, p->pixmap.w, p->pixmap.h);
    }

    b = now ();
    printf ("layout1 took %f sec\n", b - a);
    printd (state.sock, "C %d", state.pagecount);
}

static void *mainloop (void *unused)
{
    char *p = NULL;
    int len, ret, oldlen = 0;

    for (;;) {
        len = readlen (state.sock);
        if (len == 0) {
            errx (1, "readlen returned 0");
        }

        if (oldlen < len + 1) {
            p = realloc (p, len + 1);
            if (!p) {
                err (1, "realloc %d failed", len + 1);
            }
            oldlen = len + 1;
        }
        readdata (state.sock, p, len);
        p[len] = 0;

        if (!strncmp ("open", p, 4)) {
            char *filename = p + 5;

            openxref (filename, NULL, 1);
        }
        else if (!strncmp ("free", p, 4)) {
            void *ptr;

            ret = sscanf(p + 4, " %p", &ptr);
            freepage (ptr);
        }
        else if (!strncmp ("layout", p, 6)) {
            int y;

            ret = sscanf (p + 6, " %d", &y);
            if (ret != 1) {
                errx (1, "malformed layout `%.*s' ret=%d", len, p, ret);
            }
        }
        else if (!strncmp ("geometry", p, 8)) {
            int w, h;
            struct page *page;

            ret = sscanf (p + 8, " %d %d", &w, &h);
            if (ret != 2) {
                errx (1, "malformed geometry `%.*s' ret=%d", len, p, ret);
            }
            state.h = h;
            if (w != state.w) {
                state.w = w;
                for (page = state.pages; page; page = page->prev) {
                    page->texid = 0;
                }
            }
            layout1 ();
        }
        else if (!strncmp ("render", p, 6)) {
            int pagenum, pindex, w, h, ret;
            struct page *page;
            unsigned char *pix;

            ret = sscanf (p + 6, " %d %d %d %d", &pagenum, &pindex, &w, &h);

            if (ret != 4) {
                errx (1, "bad render line `%.*s' ret=%d", len, p, ret);
            }

            page = render (pagenum, pindex);
            printd (state.sock, "r %d %d %d %p\n",
                    pagenum,
                    state.w,
                    state.h,
                    page);
        }
        else {
            errx (1, "unknown command %.*s", len, p);
        }
    }
    return NULL;
}

static void upload2 (struct page *page, int texnum, const char *cap)
{
    int i;
    int w, h;
    double start, end;
    struct tex *tex = &page->texs[texnum];

    w = page->pixmap->w;
    h = page->pixmap->h;

    glPixelStorei(GL_UNPACK_ALIGNMENT, 4);
    glPixelStorei(GL_UNPACK_ROW_LENGTH, w);

    if (tex->index != -1 && state.texowners[tex->index] == tex) {
        glBindTexture (GL_TEXTURE_RECTANGLE_ARB, state.texids[tex->index]);
    }
    else  {
        int subimage;
        int index = (state.texid++ % state.texcount);
        size_t offset = 0;

        for (i = 0; i < texnum; ++i) {
            offset += w * page->texs[i].h * 4;
        }
        subimage = 0 || state.texowners[index]
            ? state.texowners[index]->h == tex->h
            : 0
            ;

        state.texowners[index] = tex;
        tex->index = index;

        glBindTexture (GL_TEXTURE_RECTANGLE_ARB, state.texids[tex->index]);

        start = now ();
        if (subimage) {
            glTexSubImage2D (GL_TEXTURE_RECTANGLE_ARB,
                             0,
                             0,
                             0,
                             w,
                             tex->h,
#ifndef _ARCH_PPC
                             GL_BGRA_EXT,
                             GL_UNSIGNED_INT_8_8_8_8,
#else
                             GL_ABGR_EXT,
                             GL_UNSIGNED_BYTE, /* INT_8_8_8_8, */
#endif
                             page->pixmap->samples + offset
                );
        }
        else {
            glTexImage2D (GL_TEXTURE_RECTANGLE_ARB,
                          0,
                          GL_RGBA8,
                          w,
                          tex->h,
                          0,
#ifndef _ARCH_PPC
                          GL_BGRA_EXT,
                          GL_UNSIGNED_INT_8_8_8_8,
#else
                          GL_ABGR_EXT,
                          GL_UNSIGNED_BYTE, /* INT_8_8_8_8, */
#endif
                          page->pixmap->samples + offset
                );
        }

        end = now ();
        printf ("%s(%s) %d(%d) took %f sec\n", cap,
                subimage ? "sub" : "img",
                page->pagenum, texnum, end - start);
    }
}

CAMLprim value ml_preload (value ptr_v)
{
    int i;
    int ret;
    void *ptr;
    CAMLparam1 (ptr_v);
    char *s = String_val (ptr_v);

    ret = sscanf (s, "%p", &ptr);
    if (ret != 1) {
        errx (1, "cannot parse pointer `%s'", s);
    }
    for (i = 0; i < state.slicecount; ++i)
        upload2 (ptr, i, "preload");
    CAMLreturn (Val_unit);
}

#if 0
#define lprintf printf
#else
#define lprintf(...)
#endif

CAMLprim value ml_draw (value dispy_v, value w_v, value h_v,
                        value py_v, value ptr_v)
{
    CAMLparam5 (dispy_v, w_v, h_v, py_v, ptr_v);
    int dispy = Int_val (dispy_v);
    int w = Int_val (w_v);
    int h = Int_val (h_v);
    int py = Int_val (py_v);
    char *s = String_val (ptr_v);
    int ret;
    const char *r;
    void *ptr;
    struct page *page;
    int texnum = 0;

    ret = sscanf (s, "%p", &ptr);
    if (ret != 1) {
        errx (1, "cannot parse pointer `%s'", s);
    }
    page = ptr;

    w = page->pixmap->w;
    printf ("draw[%d] %dx%d %dx%d\n",
            page->pagenum,
            page->pixmap->w,
            page->pixmap->h,
            w,
            h);

    glEnable (GL_TEXTURE_RECTANGLE_ARB);
#ifdef _ARCH_PPC
    glEnable (GL_FRAGMENT_SHADER_ATI);
#endif

    for (texnum = 0; texnum != state.slicecount; ++texnum) {
        struct tex *tex = &page->texs[texnum];
        if (tex->h > py) {
            break;
        }
        py -= tex->h;
    }

    while (h) {
        int th;
        struct tex *tex = &page->texs[texnum];

        if (texnum >= state.slicecount) {
            abort ();
        }
        th = MIN (h, tex->h - py);
        lprintf ("draw[%d, %d], h=%d th=%d py=%d dispy=%d\n",
                 page->pagenum, texnum, h, th, py, dispy);

        upload2 (page, texnum, "upload");

        glBegin (GL_QUADS);
        {
            glTexCoord2i (0, py);
            glVertex2i (0, dispy);

            glTexCoord2i (w, py);
            glVertex2i (w, dispy);

            glTexCoord2i (w, py+th);
            glVertex2i (w, dispy + th);

            glTexCoord2i (0, py+th);
            glVertex2i (0, dispy + th);
        }
        glEnd ();

        h -= th;
        py = 0;
        dispy += th;
        texnum += 1;
    }

    glDisable (GL_TEXTURE_RECTANGLE_ARB);
#ifdef _ARCH_PPC
    glDisable (GL_FRAGMENT_SHADER_ATI);
#endif
    CAMLreturn (Val_unit);
}

static void initgl (void)
{
    state.dpy = glXGetCurrentDisplay ();
    if (!state.dpy) {
        die (fz_throw ("glXGetCurrentDisplay"));
    }
    state.ctx = glXGetCurrentContext ();
    if (!state.ctx) {
        die (fz_throw ("glXGetCurrentContext"));
    }
    state.drawable = glXGetCurrentDrawable ();
    if (!state.drawable) {
        die (fz_throw ("glXGetCurrentDrawable"));
    }

#ifdef _ARCH_PPC
    glBindFragmentShaderATI (1);
    glBeginFragmentShaderATI ();
    {
        glSampleMapATI (GL_REG_0_ATI, GL_TEXTURE0_ARB, GL_SWIZZLE_STR_ATI);

        glColorFragmentOp1ATI (GL_MOV_ATI,
                               GL_REG_1_ATI, GL_RED_BIT_ATI, GL_NONE,
                               GL_REG_0_ATI, GL_BLUE, GL_NONE);
        glColorFragmentOp1ATI (GL_MOV_ATI,
                               GL_REG_1_ATI, GL_BLUE_BIT_ATI, GL_NONE,
                               GL_REG_0_ATI, GL_RED, GL_NONE);
        glColorFragmentOp1ATI (
            GL_MOV_ATI,
            GL_REG_0_ATI, GL_RED_BIT_ATI | GL_BLUE_BIT_ATI, GL_NONE,
            GL_REG_1_ATI, GL_NONE, GL_NONE
            );
    }
    glEndFragmentShaderATI ();
#endif
}

CAMLprim value ml_init (value sock_v)
{
    int ret;
    fz_error error;
    CAMLparam1 (sock_v);

    pagesize = sysconf (_SC_PAGESIZE);
    if (pagesize == -1) {
        err (1, "sysconf");
    }

    state.texcount = 32;
    state.slicecount = 16;

    state.texids = calloc (state.texcount * sizeof (*state.texids), 1);
    if (!state.texids) {
        err (1, "calloc texids %zu", state.texcount * sizeof (*state.texids));
    }

    state.texowners = calloc (state.texcount * sizeof (*state.texowners), 1);
    if (!state.texowners) {
        err (1, "calloc texowners %zu",
             state.texcount * sizeof (*state.texowners));
    }

    glGenTextures (state.texcount, state.texids);

    state.sock = Int_val (sock_v);
    initgl ();

    state.cache = fz_newglyphcache ();

    ret = pthread_create (&state.thread, NULL, mainloop, NULL);
    if (ret) {
        unix_error (ret, "pthread_create", Nothing);
    }

    CAMLreturn (Val_unit);
}

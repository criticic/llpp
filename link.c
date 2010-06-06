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

static const char *parsepointer (const char *s, int len, void **ptr)
{
    int i;
    intptr_t val = 0;

    if (len < 3) {
        return s;
    }

    for (i = 0; i < len; ++i) {
        char c = s[i];
        if (c >= '0' && c <= '9') {
            val = (val << 4) | (c - '0');
        }
        else if (c >= 'a' && c <= 'f')  {
            val = (val << 4) | ((c - 'a') + 10);
        }
        else {
            fprintf (stderr, "parsepointer(%.*s) i=%d len=%d invalid char=%c\n",
                     len, s, i, len, c);
            break;
        }
    }
    *(intptr_t *) ptr = val;
    return s + i;
}

struct page {
    int pagenum;
    pdf_page *drawpage;
    fz_pixmap *pixmap;
    GLuint texid;
    struct page2 *page2;
    struct page *prev;
};

struct page2 {
    fz_irect bbox;
    fz_matrix ctm;
    fz_pixmap pixmap;
    int pagenum;
};

struct {
    int sock;
    int texid;
    pthread_t thread;
    pthread_cond_t cond;
    pthread_mutex_t mutex;
    struct page *pages;
    struct page2 *pages2;
    int page;
    int pagecount;
    pdf_xref *xref;
    fz_renderer *drawgc;
    pdf_page *drawpage;
    size_t mapsize;
    void *map;
    int w, h;
    Display *dpy;
    GLXContext ctx;
    GLXDrawable drawable;
} state = {
    .cond = PTHREAD_COND_INITIALIZER,
    .mutex = PTHREAD_MUTEX_INITIALIZER
};

static char *basename1;

static void closexref (void);

static void lock (void)
{
    int ret;

    ret = pthread_mutex_lock (&state.mutex);
    if (ret) {
        errx (1, "pthread_mutex_lock: %s\n", strerror (ret));
    }
}
static void unlock (void)
{
    int ret;

    ret = pthread_mutex_unlock (&state.mutex);
    if (ret) {
        errx (1, "pthread_mutex_unlock: %s\n", strerror (ret));
    }
}

static void condsignal (void)
{
    int ret;

    ret = pthread_cond_signal (&state.cond);
    if (ret) {
        errx (1, "pthread_cond_signal: %s\n", strerror (ret));
    }
}

static void condwait (void)
{
    int ret;

    ret = pthread_cond_wait (&state.cond, &state.mutex);
    if (ret) {
        errx (1, "pthread_cond_wait: %s\n", strerror (ret));
    }
}


static void die(fz_error error)
{
    fz_catch(error, "aborting");
    closexref();
    exit(1);
}

static void drawloadpage(int pagenum)
{
    fz_error error;
    fz_obj *pageobj;
    long start;
    long end;
    long elapsed;

    pageobj = pdf_getpageobject(state.xref, pagenum);
    error = pdf_loadpage(&state.drawpage, state.xref, pageobj);
    if (error)
        die(error);
}

static void openxref(char *filename, char *password, int dieonbadpass)
{
    fz_error error;
    int okay;

    basename1 = strrchr(filename, '/');
    if (!basename1)
        basename1 = filename;
    else
        basename1++;

    state.xref = pdf_newxref();
    error = pdf_loadxref(state.xref, filename);
    if (error)
    {
        fz_catch(error, "trying to repair");
        error = pdf_repairxref(state.xref, filename);
        if (error)
            die(error);
    }

    error = pdf_decryptxref(state.xref);
    if (error)
        die(error);

    if (pdf_needspassword(state.xref))
    {
        okay = pdf_authenticatepassword(state.xref, password);
        if (!okay && !dieonbadpass)
            fz_warn("invalid password, attempting to continue.");
        else if (!okay && dieonbadpass)
            die(fz_throw("invalid password"));
    }

    state.xref->root = fz_dictgets(state.xref->trailer, "Root");
    if (state.xref->root)
        fz_keepobj(state.xref->root);

    state.xref->info = fz_dictgets(state.xref->trailer, "Info");
    if (state.xref->info)
        fz_keepobj(state.xref->info);

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

    basename1 = nil;
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

static void drawfreepage(void)
{
    pdf_droppage(state.drawpage);
    state.drawpage = nil;

    flushxref();

    /* Flush resources between pages.
     * TODO: should check memory usage before deciding to do this.
     */
    if (state.xref && state.xref->store)
    {
        /* pdf_debugstore(xref->store); */
        pdf_agestoreditems(state.xref->store);
        pdf_evictageditems(state.xref->store);
        fflush(stderr);
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

    page = calloc (sizeof (*page), 1);
    if (!page) {
        err (1, "malloc page %d\n", pagenum);
    }
    page->prev = state.pages;
    state.pages = page;

    page2 = &state.pages2[pindex];

    pageobj = pdf_getpageobject(state.xref, pagenum);
    if (!pageobj)
        die (fz_throw ("cannot retrieve info from page %d", pagenum));

    error = pdf_loadpage(&page->drawpage, state.xref, pageobj);
    if (error)
        die(error);

    error = fz_newpixmapwithrect (&page->pixmap, page2->bbox, 1);
    if (error)
        die (error);

    error = fz_rendertree (&page->pixmap, state.drawgc, page->drawpage->tree,
                           page2->ctm, page2->bbox, 1);
    if (error)
        die (error);

    /* fz_debugpixmap (page->pixmap, "haha"); */
    pdf_droppage (page->drawpage);
    page->page2 = page2;
    page->pagenum = pagenum;
    return page;
}

static void createmmap (void)
{
    int fd, ret;
    long pagesize;

    pagesize = sysconf (_SC_PAGESIZE);
    if (pagesize == -1) {
        err (1, "sysconf");
    }
    fd = open ("pdfmap", O_CREAT|O_TRUNC|O_RDWR);
    if (fd == -1) {
        err (1, "open");
    }
    ret = unlink ("pdfmap");
    if (ret) {
        err (1, "unlink");
    }
    state.mapsize = (state.mapsize + pagesize - 1) & ~(pagesize - 1);
    ret = ftruncate (fd, state.mapsize);
    if (ret) {
        err (1, "ftruncate");
    }
    state.map = mmap (NULL, state.mapsize, PROT_READ|PROT_WRITE,
                      MAP_PRIVATE, fd, 0);
    if (state.map == MAP_FAILED) {
        err (1, "mmap");
    }
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
        fz_irect bbox;
        fz_error error;
        fz_obj *pageobj;

        pageobj = pdf_getpageobject (state.xref, pagenum);
        if (!pageobj)
            die (fz_throw ("cannot retrieve info from page %d", pagenum));

        obj = fz_dictgets (pageobj, "MediaBox");
        if (!fz_isarray (obj))
            die (fz_throw ("cannot find page bounds %d (%d R)",
                           fz_tonum (obj), fz_togen (obj)));

        box = pdf_torect (obj);

        obj = fz_dictgets (pageobj, "Rotate");
        if (fz_isint (obj))
            rotate = fz_toint (obj);
        else
            rotate = 0;

        if (pagenum != 1
            && (prevrotate == rotate
                && !memcmp (&prevbox, &box, sizeof (box))))
            continue;

        memcpy (&prevbox, &box, sizeof (box));
        prevrotate = rotate;

        box.x0 = MIN (prevbox.x0, prevbox.x1);
        box.y0 = MIN (prevbox.y0, prevbox.y1);
        box.x1 = MAX (prevbox.x0, prevbox.x1);
        box.y1 = MAX (prevbox.y0, prevbox.y1);

        ctm = fz_identity ();
        ctm = fz_concat (ctm, fz_translate (0, -box.y1));
        ctm = fz_concat (ctm, fz_rotate (rotate));
        box2 = fz_transformaabb (ctm, box);
        w = box2.x1 - box2.x0;

        zoom = state.w / w;
        ctm = fz_identity ();
        ctm = fz_concat (ctm, fz_translate (0, -box.y1));
        ctm = fz_concat (ctm, fz_scale (zoom, -zoom));
        ctm = fz_concat (ctm, fz_rotate (rotate));
        bbox = fz_roundrect (fz_transformaabb (ctm, box));

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

    state.mapsize = mapsize;
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
            state.w = w;
            state.h = h;
            for (page = state.pages; page; page = page->prev) {
                page->texid = 0;
            }
            layout1 ();
        }
        else if (!strncmp ("render", p, 6)) {
            int pagenum, pindex, w, h, ret;

            ret = sscanf (p + 6, " %d %d %d %d", &pagenum, &pindex, &w, &h);

            if (ret != 4) {
                errx (1, "bad render line `%.*s' ret=%d", len, p, ret);
            }

            printd (state.sock, "r %d %d %d %llx\n",
                    pagenum,
                    state.w,
                    state.h,
                    (unsigned long long) (intptr_t) render (pagenum, pindex));
        }
        else {
            errx (1, "unknown command %.*s", len, p);
        }
    }
    return NULL;
}

static void upload (struct page *page, const char *cap)
{
    int w, h, subimage = 0;

    w = page->page2->bbox.x1 - page->page2->bbox.x0;

    glPixelStorei(GL_UNPACK_ALIGNMENT, 4);
    glPixelStorei(GL_UNPACK_ROW_LENGTH, w);

    if (page->texid) {
        glBindTexture (GL_TEXTURE_RECTANGLE_ARB, page->texid);
    }
    else  {
        double start, end;
        struct page *p;
        int texid = (state.texid++ % 10) + 1;

        h = page->page2->bbox.y1 - page->page2->bbox.y0;

        for (p = state.pages; p; p = p->prev) {
            if (p->texid == texid) {
                int w1, h1;
                p->texid = 0;
                w1 = page->page2->bbox.x1 - page->page2->bbox.x0;
                h1 = page->page2->bbox.y1 - page->page2->bbox.y0;
                if (w == w1 && h == h1) {
                    subimage = 0;
                }
                break;
            }
        }
        page->texid = texid;

        /* glGenTextures (1, &page->texid); */
        glBindTexture (GL_TEXTURE_RECTANGLE_ARB, page->texid);

        start = now ();
        if (subimage)
            glTexSubImage2D (GL_TEXTURE_RECTANGLE_ARB,
                             0,
                             0,
                             0,
                             w,
                             h,
                             GL_ABGR_EXT,
                             GL_UNSIGNED_BYTE,
                             page->pixmap->samples
                );
        else
        glTexImage2D (GL_TEXTURE_RECTANGLE_ARB,
                      0,
                      GL_RGBA8,
                      w,
                      h,
                      0,
/*                       GL_BGRA_EXT, */
/*                       GL_UNSIGNED_INT_8_8_8_8_REV, */
                      GL_ABGR_EXT,
                      GL_UNSIGNED_BYTE, /* INT_8_8_8_8, */
                      page->pixmap->samples
            );

        end = now ();
        printf ("%s(%s) %d took %f sec\n", cap,
                subimage ? "sub" : "img",
                page->pagenum, end - start);
    }
}

CAMLprim value ml_preload (value ptr_v)
{
    CAMLparam1 (ptr_v);
    struct page *page;
    char *s = String_val (ptr_v);
    const char *r;
    void *ptr;

    r = parsepointer (s, caml_string_length (ptr_v), &ptr);
    if (r == s || *r != 0) {
        errx (1, "cannot parse pointer `%s r=%p s=%p [0]=%d'",
              s, r, s, r[0]);
    }
    page = ptr;
    upload (page, "preload");
    CAMLreturn (Val_unit);
}

CAMLprim value ml_draw (value dispy_v, value w_v, value h_v,
                        value py_v, value ptr_v)
{
    CAMLparam5 (dispy_v, w_v, h_v, py_v, ptr_v);
    int dispy = Int_val (dispy_v);
    int w = Int_val (w_v);
    int h = Int_val (h_v);
    int py = Int_val (py_v);
    char *s = String_val (ptr_v);
    const char *r;
    void *ptr;
    struct page *page;

    r = parsepointer (s, caml_string_length (ptr_v), &ptr);
    if (r == s || *r != 0) {
        errx (1, "cannot parse pointer `%s r=%p s=%p [0]=%d'",
              s, r, s, r[0]);
    }
    page = ptr;

    if (0)
        printf ("draw[%d=%dx%d] dispy=%d w=%d h=%d py=%d ptr=%p\n",
                page->pagenum,
                page->page2->bbox.x1 - page->page2->bbox.x0,
                page->page2->bbox.y1 - page->page2->bbox.y0,
                dispy,
                w,
                h,
                py,
                ptr);

    upload (page, "upload");
    glEnable (GL_TEXTURE_RECTANGLE_ARB);
    glEnable (GL_FRAGMENT_SHADER_ATI);
    glBegin (GL_QUADS);
    {
        glTexCoord2i (0, py);
        glVertex2i (0, dispy);

        glTexCoord2i (w, py);
        glVertex2i (w, dispy);

        glTexCoord2i (w, py+h);
        glVertex2i (w, dispy + h);

        glTexCoord2i (0, py+h);
        glVertex2i (0, dispy + h);
    }
    glEnd ();
    glDisable (GL_TEXTURE_RECTANGLE_ARB);
    glDisable (GL_FRAGMENT_SHADER_ATI);
    glFlush ();
    glFinish ();
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
}

CAMLprim value ml_init (value sock_v, value texid_v)
{
    int ret;
    fz_error error;
    CAMLparam2 (sock_v, texid_v);

    state.sock = Int_val (sock_v);
    initgl ();

    error = fz_newrenderer (&state.drawgc, pdf_devicergb, 0, 1024 * 512);
    if (error)
        die (error);

    ret = pthread_create (&state.thread, NULL, mainloop, NULL);
    if (ret) {
        unix_error (ret, "pthread_create", Nothing);
    }

    CAMLreturn (Val_unit);
}

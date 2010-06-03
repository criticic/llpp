#define _GNU_SOURCE
#include <err.h>
#include <errno.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <pthread.h>
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

struct page {
    int pagenum;
    fz_irect bbox;
    pdf_page *drawpage;
    fz_pixmap *pixmap;
    GLuint texid;
    struct page *next;
};

struct {
    int sock;
    pthread_t thread;
    pthread_cond_t cond;
    pthread_mutex_t mutex;
    struct page *pages;
    int page;
    int pagecount;
    pdf_xref *xref;
    fz_renderer *drawgc;
    pdf_page *drawpage;
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
    char buf[80];

    va_start (ap, fmt);
    len = vsnprintf (buf, sizeof (buf), fmt, ap);
    va_end (ap);
    writedata (fd, buf, len);
}

static const char *parseint (const char *p, int len, int *parsed)
{
    const char *s = p;
    int n = 1, i = 0, r = 0;

    while (i < len && s[i] >= '0' && s[i] <= '9') {
        i++;
    }

    p = &s[i-1];
    while (i--) {
        r += n*(s[i] - '0');
        n = (n << 3) + (n << 1);
    }

    *parsed = r;
    return p;
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

static const char *parsefloat (const char *p, int len, float *parsed)
{
    const char *s = p;
    union {
        unsigned int u;
        float f;
    } u = {0};

    for (;;) {
        if (*s >= '0' && *s <= '9') {
            u.u = (u.u << 4) | (*s - '0');
        }
        else if (*s >= 'a' && *s <= 'f') {
            u.u = (u.u << 4) | ((*s - 'a') + 10);
        }
        else {
            break;
        }
        s++;
    }
    *parsed = u.f;
    return s;
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

static void *render (int pagenum)
{
    fz_error error;
    fz_rect rect;
    fz_matrix ctm;
    fz_irect bbox;
    fz_obj *ref, *obj, *pageobj;
    int w, h;
    float zoom;
    struct page *page;

    page = calloc (sizeof (*page), 1);
    if (!page) {
        err (1, "malloc page %d\n", pagenum);
    }

    page->pagenum = pagenum;

    pageobj = pdf_getpageobject(state.xref, pagenum);
    if (!pageobj)
        die (fz_throw ("cannot retrieve info from page %d", pagenum));

    obj = ref = fz_dictgets (pageobj, "MediaBox");
    if (!fz_isarray (obj))
        die (fz_throw ("cannot find page bounds %d (%d R)",
                       fz_tonum (ref), fz_togen (ref)));

    error = pdf_loadpage(&page->drawpage, state.xref, pageobj);
    if (error)
        die(error);

    rect = pdf_torect (obj);
    w = rect.x1 - rect.x0;
    zoom = ((float) state.w / w);

    ctm = fz_identity ();
    ctm = fz_concat (ctm, fz_scale (zoom, -zoom));
    bbox = fz_roundrect (fz_transformaabb (ctm, rect));

    w = bbox.x1 - bbox.x0;
    h = bbox.y1 - bbox.y0;

    error = fz_newpixmap (&page->pixmap, bbox.x0, bbox.y0, w, h, 4);
    if (error)
        die (error);

    error = fz_rendertree (&page->pixmap, state.drawgc, page->drawpage->tree,
                           ctm, bbox, 1);
    if (error)
        die (error);

    page->pagenum = pagenum;
    page->bbox = bbox;
    return page;
}

static void layout (int viewy)
{
    int pagenum;
    fz_matrix ctm;
    int dispy, posy;

    dispy = 0;
    posy = 0;

    printd (state.sock, "c");
    for (pagenum = 1; pagenum < state.pagecount; ++pagenum) {
        int pw, ph;
        fz_obj *ref;
        fz_obj *obj;
        fz_rect rect;
        fz_irect bbox;
        float w, zoom;
        fz_obj *pageobj;

        pageobj = pdf_getpageobject (state.xref, pagenum);
        if (!pageobj)
            die (fz_throw ("cannot retrieve info from page %d", pagenum));

        obj = ref = fz_dictgets (pageobj, "MediaBox");
        if (!fz_isarray (obj))
            die (fz_throw ("cannot find page bounds %d (%d R)",
                           fz_tonum (ref), fz_togen (ref)));

        rect = pdf_torect (obj);
        w = rect.x1 - rect.x0;
        zoom = ((float) state.w / w);

        ctm = fz_identity ();
        ctm = fz_concat (ctm, fz_scale (zoom, zoom));
        bbox = fz_roundrect (fz_transformaabb (ctm, rect));

        pw = bbox.x1 - bbox.x0;
        ph = bbox.y1 - bbox.y0;

        if (posy + ph > viewy) {
            int py = viewy - posy;
            int vph = ph - py;

            if (dispy + vph > state.h) {
                vph = state.h - dispy;
                printd (state.sock, "p %d %d %d %d %d %d",
                        pagenum, pw, ph, dispy, py, vph);

                break;
            }

            printd (state.sock, "p %d %d %d %d %d %d",
                    pagenum, pw, ph, dispy, py, vph);

            dispy += vph;
            viewy += vph;
        }
        /* if (pagenum < state.pagecount - 1) */
            posy += ph;
    }
    if (pagenum == state.pagecount) {
        printd (state.sock, "m %d", posy);
    }
}

static void *mainloop (void *unused)
{
    char *p = NULL;
    int len, oldlen = 0;

    for (;;) {
        len = readlen (state.sock);
        if (len == 0) {
            errx (1, "readlen returned 0");
        }

        if (oldlen < len) {
            p = realloc (p, len);
            if (!p) {
                err (1, "realloc %d failed", len);
            }
            oldlen = len;
        }
        readdata (state.sock, p, len);

        if (!strncmp ("open", p, 4)) {
            char *filename = p + 5;

            openxref (filename, NULL, 1);
        }
        else if (!strncmp ("layout", p, 6)) {
            int y;
            char *opt;

            opt = p + 7;
            parseint (opt, len - 7, &y);
            layout (y);
        }
        else if (!strncmp ("geometry", p, 8)) {
            int w, h, len2;
            const char *opt, *opt2;

            opt = p + 9;
            opt2 = parseint (opt, len - 9, &w) + 2;

            len2 = opt2 - opt;
            if (len2 + 10 >= len || opt2[-1] != ' ') {
                errx (1, "malformed geometry command `%.*s'", len, p);
            }

            opt = opt2;
            parseint (opt, len - len2 - 9, &h);
            state.w = w;
            state.h = h;
        }
        else if (!strncmp ("render", p, 6)) {
            char *opt;
            int pagenum;

            opt = p + 7;
            parseint (opt, len - 7, &pagenum);
            printd (state.sock, "r %d %d %d %llx\n",
                    pagenum,
                    state.w,
                    state.h,
                    (unsigned long long) (intptr_t) render (pagenum));
        }
        else if (!strncmp ("upload", p, 6)) {
            int pagenum;
            const char *opt, *opt2;
            void *ptr;
            struct page *page;

            opt = p + 7;
            opt2 = parseint (opt, len - 7, &pagenum) + 2;
            opt = parsepointer (opt2, len - (opt2 - p), &ptr);
            page = ptr;

            glGenTextures (1, &page->texid);
            glBindTexture (GL_TEXTURE_RECTANGLE_ARB, page->texid);

            glTexImage2D (GL_TEXTURE_RECTANGLE_ARB,
                          0,
                          GL_RED,
                          page->bbox.x1 - page->bbox.x0,
                          page->bbox.y1 - page->bbox.y0,
                          0,
                          GL_BGRA,
                          GL_UNSIGNED_INT_8_8_8_8_REV,
                          page->pixmap->samples
                );
            printd (state.sock, "u %d", pagenum);
        }
        else {
            errx (1, "unknown command %.*s", len, p);
        }
    }
    return NULL;
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
                page->bbox.x1 - page->bbox.x0,
                page->bbox.y1 - page->bbox.y0,
                dispy,
                w,
                h,
                py,
                ptr);

    w = page->bbox.x1 - page->bbox.x0;

    glPixelStorei(GL_UNPACK_ALIGNMENT, 4);
    glPixelStorei(GL_UNPACK_ROW_LENGTH, w);

    if (page->texid) {
        glBindTexture (GL_TEXTURE_RECTANGLE_ARB, page->texid);
    }
    else  {
        double start, end;
        glGenTextures (1, &page->texid);
        glBindTexture (GL_TEXTURE_RECTANGLE_ARB, page->texid);

        start = now ();
        glTexImage2D (GL_TEXTURE_RECTANGLE_ARB,
                      0,
                      GL_RGBA8,
                      w,
                      page->bbox.y1 - page->bbox.y0,
                      0,
                      GL_ABGR_EXT,
                      GL_UNSIGNED_BYTE,
                      page->pixmap->samples
            );
        end = now ();
        printf ("took %f sec\n", end - start);
    }

    glEnable (GL_TEXTURE_RECTANGLE_ARB);
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
}

CAMLprim value ml_layout (value viewy_v)
{
    CAMLparam1 (viewy_v);
    int viewy = Int_val (viewy_v);
    printf ("%d\n", viewy);
    layout (viewy);
    CAMLreturn (Val_unit);
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

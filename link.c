/* lots of code c&p-ed directly from mupdf */
#include <errno.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <signal.h>

#include <unistd.h>
#include <pthread.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/ioctl.h>

#ifdef __CYGWIN__
#include <cygwin/socket.h>      /* FIONREAD */
#else
#include <spawn.h>
#endif

#include <regex.h>
#include <ctype.h>
#include <stdarg.h>
#include <limits.h>

#include <GL/gl.h>

#include <caml/fail.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/unixsupport.h>

#include <fitz.h>
#include <mupdf.h>
#include <mupdf-internal.h>
#include <muxps.h>
#include <muxps-internal.h>
#include <mucbz.h>

#include FT_FREETYPE_H

#define PIGGYBACK

#ifndef __USE_GNU
extern char **environ;
#endif

#define MIN(a,b) ((a) < (b) ? (a) : (b))
#define MAX(a,b) ((a) > (b) ? (a) : (b))

#if defined __GNUC__
#define NORETURN __attribute__ ((noreturn))
#define UNUSED __attribute__ ((unused))
#define OPTIMIZE(n) __attribute__ ((optimize ("O"#n)))
#define GCC_FMT_ATTR(a, b) __attribute__ ((format (printf, a, b)))
#else
#define NORETURN
#define UNUSED
#define OPTIMIZE(n)
#define GCC_FMT_ATTR(a, b)
#endif

#define FMT_s "zu"

#define FMT_ptr "p"
#define FMT_ptr_cast(p) (p)
#define FMT_ptr_cast2(p) (p)

static void NORETURN GCC_FMT_ATTR (2, 3)
    err (int exitcode, const char *fmt, ...)
{
    va_list ap;
    int savederrno;

    savederrno = errno;
    va_start (ap, fmt);
    vfprintf (stderr, fmt, ap);
    va_end (ap);
    fprintf (stderr, ": %s\n", strerror (savederrno));
    fflush (stderr);
    _exit (exitcode);
}

static void NORETURN GCC_FMT_ATTR (2, 3)
    errx (int exitcode, const char *fmt, ...)
{
    va_list ap;

    va_start (ap, fmt);
    vfprintf (stderr, fmt, ap);
    va_end (ap);
    fputc ('\n', stderr);
    fflush (stderr);
    _exit (exitcode);
}

#ifndef GL_TEXTURE_RECTANGLE_ARB
#define GL_TEXTURE_RECTANGLE_ARB          0x84F5
#endif

#ifndef GL_BGRA
#define GL_BGRA                           0x80E1
#endif

#ifndef GL_UNSIGNED_INT_8_8_8_8
#define GL_UNSIGNED_INT_8_8_8_8           0x8035
#endif

#ifndef GL_UNSIGNED_INT_8_8_8_8_REV
#define GL_UNSIGNED_INT_8_8_8_8_REV       0x8367
#endif

#if 0
#define lprintf printf
#else
#define lprintf(...)
#endif

#define ARSERT(cond) for (;;) {                         \
    if (!(cond)) {                                      \
        errx (1, "%s:%d " #cond, __FILE__, __LINE__);   \
    }                                                   \
    break;                                              \
}

struct slice {
    int h;
    int texindex;
};

struct tile {
    int x, y, w, h;
    int slicecount;
    int sliceheight;
    fz_pixmap *pixmap;
    struct slice slices[1];
};

struct pagedim {
    int pageno;
    int rotate;
    int left;
    int tctmready;
    fz_bbox bounds;
    fz_rect pagebox;
    fz_rect mediabox;
    fz_matrix ctm, zoomctm, lctm, tctm;
};

struct slink  {
    fz_bbox bbox;
    fz_link *link;
};

enum { DPDF, DXPS, DCBZ };

struct page {
    int tgen;
    int sgen;
    int type;
    int pageno;
    int pdimno;
    fz_text_page *text;
    fz_text_sheet *sheet;
    union {
        void *ptr;
        pdf_page *pdfpage;
        xps_page *xpspage;
        cbz_page *cbzpage;
    } u;
    fz_display_list *dlist;
    int slinkcount;
    struct slink *slinks;
    struct mark {
        int i;
        fz_text_span *span;
    } fmark, lmark;
    void (*freepage) (void *);
};

struct {
    int type;
    int sliceheight;
    struct pagedim *pagedims;
    int pagecount;
    int pagedimcount;
    union {
        pdf_document *pdf;
        xps_document *xps;
        cbz_document *cbz;
    } u;
    fz_context *ctx;
    int w, h;

    int texindex;
    int texcount;
    GLuint *texids;

    GLenum texiform;
    GLenum texform;
    GLenum texty;

    fz_colorspace *colorspace;

    struct {
        int w, h;
        struct slice *slice;
    } *texowners;

    int rotate;
    int proportional;
    int trimmargins;
    int needoutline;
    int gen;
    int aalevel;

    int trimanew;
    fz_bbox trimfuzz;
    fz_pixmap *pig;

    pthread_t thread;
    int cr, cw;
    FT_Face face;

    void (*closedoc) (void);
    void (*freepage) (void *);
} state;

static void UNUSED debug_rect (const char *cap, fz_rect r)
{
    printf ("%s(rect) %.2f,%.2f,%.2f,%.2f\n", cap, r.x0, r.y0, r.x1, r.y1);
}

static void UNUSED debug_bbox (const char *cap, fz_bbox r)
{
    printf ("%s(bbox) %d,%d,%d,%d\n", cap, r.x0, r.y0, r.x1, r.y1);
}

static void UNUSED debug_matrix (const char *cap, fz_matrix m)
{
    printf ("%s(matrix) %.2f,%.2f,%.2f,%.2f %.2f %.2f\n", cap,
            m.a, m.b, m.c, m.d, m.e, m.f);
}

static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

static void lock (const char *cap)
{
    int ret = pthread_mutex_lock (&mutex);
    if (ret) {
        errx (1, "%s: pthread_mutex_lock: %s", cap, strerror (ret));
    }
}

static void unlock (const char *cap)
{
    int ret = pthread_mutex_unlock (&mutex);
    if (ret) {
        errx (1, "%s: pthread_mutex_unlock: %s", cap, strerror (ret));
    }
}

static int trylock (const char *cap)
{
    int ret = pthread_mutex_trylock (&mutex);

    if (ret && ret != EBUSY) {
        errx (1, "%s: pthread_mutex_trylock: %s", cap, strerror (ret));
    }
    return ret == EBUSY;
}

static void *parse_pointer (const char *cap, const char *s)
{
    int ret;
    void *ptr;

    ret = sscanf (s, "%" FMT_ptr, FMT_ptr_cast (&ptr));
    if (ret != 1) {
        errx (1, "%s: cannot parse pointer in `%s'", cap, s);
    }
    return ptr;
}

static double now (void)
{
    struct timeval tv;

    if (gettimeofday (&tv, NULL)) {
        err (1, "gettimeofday");
    }
    return tv.tv_sec + tv.tv_usec*1e-6;
}

static int hasdata (void)
{
    int ret, avail;
    ret = ioctl (state.cr, FIONREAD, &avail);
    if (ret) err (1, "hasdata: FIONREAD error ret=%d", ret);
    return avail > 0;
}

CAMLprim value ml_hasdata (value fd_v)
{
    CAMLparam1 (fd_v);
    int ret, avail;

    ret = ioctl (Int_val (fd_v), FIONREAD, &avail);
    if (ret) uerror ("ioctl (FIONREAD)", Nothing);
    CAMLreturn (Val_bool (avail > 0));
}

static void readdata (void *p, int size)
{
    ssize_t n;

 again:
    n = read (state.cr, p, size);
    if (n < 0) {
        if (errno == EINTR) goto again;
        err (1, "read (req %d, ret %zd)", size, n);
    }
    if (n - size) {
        if (!n) errx (1, "EOF while reading");
        errx (1, "read (req %d, ret %zd)", size, n);
    }
}

static void writedata (char *p, int size)
{
    char buf[4];
    ssize_t n;

    buf[0] = (size >> 24) & 0xff;
    buf[1] = (size >> 16) & 0xff;
    buf[2] = (size >>  8) & 0xff;
    buf[3] = (size >>  0) & 0xff;

    n = write (state.cw, buf, 4);
    if (n != 4) {
        if (!n) errx (1, "EOF while writing length");
        err (1, "write %zd", n);
    }

    n = write (state.cw, p, size);
    if (n - size) {
        if (!n) errx (1, "EOF while writing data");
        err (1, "write (req %d, ret %zd)", size, n);
    }
}

static int readlen (void)
{
    unsigned char p[4];

    readdata (p, 4);
    return (p[0] << 24) | (p[1] << 16) | (p[2] << 8) | p[3];
}

static void GCC_FMT_ATTR (1, 2) printd (const char *fmt, ...)
{
    int size = 200, len;
    va_list ap;
    char *buf;

    buf = malloc (size);
    for (;;) {
        if (!buf) err (1, "malloc for temp buf (%d bytes) failed", size);

        va_start (ap, fmt);
        len = vsnprintf (buf, size, fmt, ap);
        va_end (ap);

        if (len > -1 && len < size) {
            writedata (buf, len);
            break;
        }

        if (len > -1) {
            size = len + 1;
        }
        else  {
            size *= 2;
        }
        buf = realloc (buf, size);
    }
    free (buf);
}

static void closepdf (void)
{
    if (state.u.pdf) {
        pdf_close_document (state.u.pdf);
        state.u.pdf = NULL;
    }
}

static void closexps (void)
{
    if (state.u.xps) {
        xps_close_document (state.u.xps);
        state.u.xps = NULL;
    }
}

static void closecbz (void)
{
    if (state.u.cbz) {
        cbz_close_document (state.u.cbz);
        state.u.cbz = NULL;
    }
}

static void freepdfpage (void *ptr)
{
    pdf_free_page (state.u.pdf, ptr);
}

static void freeemptypdfpage (void *ptr)
{
    (void) ptr;
}

static void freexpspage (void *ptr)
{
    xps_free_page (state.u.xps, ptr);
}

static void freecbzpage (void *ptr)
{
    cbz_free_page (state.u.cbz, ptr);
}

static void openxref (char *filename, char *password)
{
    int i, len;

    for (i = 0; i < state.texcount; ++i)  {
        state.texowners[i].w = -1;
        state.texowners[i].slice = NULL;
    }

    if (state.closedoc) state.closedoc ();

    len = strlen (filename);

    state.type = DPDF;
    if (len > 4) {
        char ext[4];

        ext[0] = tolower ((int) filename[len-3]);
        ext[1] = tolower ((int) filename[len-2]);
        ext[2] = tolower ((int) filename[len-1]);

        /**/ if (ext[0] == 'x' && ext[1] == 'p' && ext[2] == 's') {
            state.type = DXPS;
        }
        else if (ext[0] == 'c' && ext[1] == 'b' && ext[2] == 'z') {
            state.type = DCBZ;
        }
    }

    if (state.pagedims) {
        free (state.pagedims);
        state.pagedims = NULL;
    }
    state.pagedimcount = 0;

    fz_set_aa_level (state.ctx, state.aalevel);
    switch (state.type) {
    case DPDF:
        state.u.pdf = pdf_open_document (state.ctx, filename);
        if (pdf_needs_password (state.u.pdf)) {
            int okay = pdf_authenticate_password (state.u.pdf, password);
            if (!okay) {
                errx (1, "invalid password");
            }
        }
        state.pagecount = pdf_count_pages (state.u.pdf);
        state.closedoc = closepdf;
        state.freepage = freepdfpage;
        break;

    case DXPS:
        state.u.xps = xps_open_document (state.ctx, filename);
        state.pagecount = xps_count_pages (state.u.xps);
        state.closedoc = closexps;
        state.freepage = freexpspage;
        break;

    case DCBZ:
        state.u.cbz = cbz_open_document (state.ctx, filename);
        state.pagecount = cbz_count_pages (state.u.cbz);
        state.closedoc = closecbz;
        state.freepage = freecbzpage;
        break;
    }
}

static void pdfinfo (void)
{
    if (state.type == DPDF) {
        pdf_obj *infoobj;

        printd ("info PDF version\t%d.%d",
                state.u.pdf->version / 10, state.u.pdf->version % 10);

        infoobj = pdf_dict_gets (state.u.pdf->trailer, "Info");
        if (infoobj) {
            int i;
            char *s;
            char *items[] = { "Title", "Author", "Creator",
                              "Producer", "CreationDate" };

            for (i = 0; i < sizeof (items) / sizeof (*items); ++i) {
                pdf_obj *obj = pdf_dict_gets (infoobj, items[i]);
                s = pdf_to_utf8 (state.ctx, obj);
                if (*s) {
                    if (i == 0) {
                        printd ("title %s", s);
                    }
                    printd ("info %s\t%s", items[i], s);
                }
                fz_free (state.ctx, s);
            }
        }
        printd ("infoend");
    }
}

static void unlinktile (struct tile *tile)
{
    int i;

    for (i = 0; i < tile->slicecount; ++i) {
        struct slice *s = &tile->slices[i];

        if (s->texindex != -1) {
            if (state.texowners[s->texindex].slice == s) {
                state.texowners[s->texindex].slice = NULL;
            }
        }
    }
}

static void freepage (struct page *page)
{
    if (page->text) {
        fz_free_text_page (state.ctx, page->text);
    }
    if (page->sheet) {
        fz_free_text_sheet (state.ctx, page->sheet);
    }
    if (page->slinks) {
        free (page->slinks);
    }
    page->freepage (page->u.ptr);
    fz_free_display_list (state.ctx, page->dlist);
    free (page);
}

static void freetile (struct tile *tile)
{
    unlinktile (tile);
#ifndef PIGGYBACK
    fz_drop_pixmap (state.ctx, tile->pixmap);
#else
    if (state.pig) {
        fz_drop_pixmap (state.ctx, state.pig);
    }
    state.pig = tile->pixmap;
#endif
    free (tile);
}

#ifdef __ALTIVEC__
#include <altivec.h>

static int cacheline32bytes;

static void __attribute__ ((constructor)) clcheck (void)
{
    char **envp = environ;
    unsigned long *auxv;

    while (*envp++);

    for (auxv = (unsigned long *) envp; *auxv != 0; auxv += 2) {
        if (*auxv == 19) {
            cacheline32bytes = auxv[1] == 32;
            return;
        }
    }
}

static void OPTIMIZE (3) clearpixmap (fz_pixmap *pixmap)
{
    if (cacheline32bytes) {
        intptr_t a1, a2, diff;
        size_t sizea, i, size = pixmap->w * pixmap->h * pixmap->n;
        vector unsigned char v = vec_splat_u8 (-1);
        vector unsigned char *p;

        a1 = a2 = (intptr_t) pixmap->samples;
        a2 = (a1 + 31) & ~31;
        diff = a2 - a1;
        sizea = size - diff;
        p = (void *) a2;

        while (a1 != a2) *(char *) a1++ = 0xff;
        for (i = 0; i < (sizea & ~31); i += 32)  {
            __asm volatile ("dcbz %0, %1"::"b"(a2),"r"(i));
            vec_st (v, i, p);
            vec_st (v, i + 16, p);
        }
        while (i < sizea) *((char *) a1 + i++) = 0xff;
    }
    else fz_clear_pixmap_with_value (state.ctx, pixmap, 0xff);
}
#else
#define clearpixmap(p) fz_clear_pixmap_with_value (state.ctx, p, 0xff)
#endif

static fz_matrix trimctm (pdf_page *page, int pindex)
{
    fz_matrix ctm;
    struct pagedim *pdim = &state.pagedims[pindex];

    if (!pdim->tctmready) {
        if (state.trimmargins) {
            fz_rect realbox;

            ctm = fz_concat (fz_rotate (-pdim->rotate), fz_scale (1, -1));
            realbox = fz_transform_rect (ctm, pdim->mediabox);
            ctm = fz_concat (ctm, fz_translate (-realbox.x0, -realbox.y0));
            ctm = fz_concat (fz_invert_matrix (page->ctm), ctm);
        }
        else {
            ctm = fz_identity;
        }
        pdim->tctm = ctm;
        pdim->tctmready = 1;
    }
    return pdim->tctm;
}

static fz_matrix pagectm (struct page *page)
{
    if (page->type == DPDF) {
        return fz_concat (trimctm (page->u.pdfpage, page->pdimno),
                          state.pagedims[page->pdimno].ctm);
    }
    else {
        fz_matrix ctm;
        struct pagedim *pdim = &state.pagedims[page->pdimno];

        ctm = state.pagedims[page->pdimno].ctm;
        ctm = fz_concat (fz_translate (-pdim->mediabox.x0,
                                       -pdim->mediabox.y0), ctm);
        return ctm;
    }
}

static void *loadpage (int pageno, int pindex)
{
    fz_device *dev;
    struct page *page = NULL;

    page = calloc (sizeof (struct page), 1);
    if (!page) {
        err (1, "calloc page %d", pageno);
    }

    page->dlist = fz_new_display_list (state.ctx);
    dev = fz_new_list_device (state.ctx, page->dlist);
    switch (state.type) {
    case DPDF:
        fz_try (state.ctx) {
            page->u.pdfpage = pdf_load_page (state.u.pdf, pageno);
            pdf_run_page (state.u.pdf, page->u.pdfpage, dev, fz_identity, NULL);
            page->freepage = freepdfpage;
        }
        fz_catch (state.ctx) {
            page->u.ptr = NULL;
            page->freepage = freeemptypdfpage;
        }
        break;

    case DXPS:
        page->u.xpspage = xps_load_page (state.u.xps, pageno);
        xps_run_page (state.u.xps, page->u.xpspage, dev, fz_identity, NULL);
        page->freepage = freexpspage;
        break;

    case DCBZ:
        page->u.cbzpage = cbz_load_page (state.u.cbz, pageno);
        cbz_run_page (state.u.cbz, page->u.cbzpage, dev, fz_identity, NULL);
        page->freepage = freecbzpage;
        break;
    }
    fz_free_device (dev);

    page->pdimno = pindex;
    page->pageno = pageno;
    page->sgen = state.gen;
    page->tgen = state.gen;
    page->type = state.type;

    return page;
}

static struct tile *alloctile (int h)
{
    int i;
    int slicecount;
    size_t tilesize;
    struct tile *tile;

    slicecount = (h + state.sliceheight - 1) / state.sliceheight;
    tilesize = sizeof (*tile) + ((slicecount - 1) * sizeof (struct slice));
    tile = calloc (tilesize, 1);
    if (!tile) {
        err (1, "can not allocate tile (%" FMT_s " bytes)", tilesize);
    }
    for (i = 0; i < slicecount; ++i) {
        int sh = MIN (h, state.sliceheight);
        tile->slices[i].h = sh;
        tile->slices[i].texindex = -1;
        h -= sh;
    }
    tile->slicecount = slicecount;
    tile->sliceheight = state.sliceheight;
    return tile;
}

struct obs {
    int cured;
    fz_bbox b;
};

static void obs_fill_image (fz_device *dev, fz_image *image, fz_matrix ctm,
                            float alpha)
{
    struct obs *obs = dev->user;

    if (!obs->cured && fabs (1.0 - alpha) < 1e6) {
        fz_bbox b = fz_round_rect (fz_transform_rect (ctm, fz_unit_rect));
        b = fz_intersect_bbox (b, obs->b);
        obs->cured = b.x0 == obs->b.x0
            && b.x1 == obs->b.x1
            && b.y0 == obs->b.y0
            && b.y1 == obs->b.y1;
    }
}

static int obscured (struct page *page, fz_bbox bbox)
{
    fz_device dev;
    struct obs obs;

    memset (&dev, 0, sizeof (dev));
    memset (&obs, 0, sizeof (obs));
    dev.hints = 0;
    dev.flags = 0;
    dev.user = &obs;
    dev.ctx = state.ctx;
    dev.fill_image = obs_fill_image;
    obs.b = bbox;
    fz_run_display_list (page->dlist, &dev, pagectm (page), bbox, NULL);
    return obs.cured;
}

static struct tile *rendertile (struct page *page, int x, int y, int w, int h)
{
    fz_bbox bbox;
    fz_device *dev;
    struct tile *tile;
    struct pagedim *pdim;

    tile = alloctile (h);
    pdim = &state.pagedims[page->pdimno];

    bbox = pdim->bounds;
    bbox.x0 += x;
    bbox.y0 += y;
    bbox.x1 = bbox.x0 + w;
    bbox.y1 = bbox.y0 + h;

    if (state.pig) {
        if (state.pig->w == w
            && state.pig->h == h
            && state.pig->colorspace == state.colorspace) {
            tile->pixmap = state.pig;
            tile->pixmap->x = bbox.x0;
            tile->pixmap->y = bbox.y0;
        }
        else {
            fz_drop_pixmap (state.ctx, state.pig);
        }
        state.pig = NULL;
    }
    if (!tile->pixmap) {
        tile->pixmap =
            fz_new_pixmap_with_bbox (state.ctx, state.colorspace, bbox);
    }

    tile->w = w;
    tile->h = h;
    if (!page->u.ptr || ((w < 128 && h < 128) || !obscured (page, bbox))) {
        clearpixmap (tile->pixmap);
    }
    dev = fz_new_draw_device (state.ctx, tile->pixmap);
    fz_run_display_list (page->dlist, dev, pagectm (page), bbox, NULL);
    fz_free_device (dev);

    return tile;
}

static void initpdims (void)
{
    double start, end;
    int pageno, trim, show;

    start = now ();
    for (pageno = 0; pageno < state.pagecount; ++pageno) {
        int rotate = 0;
        struct pagedim *p;
        fz_rect mediabox;

        switch (state.type) {
        case DPDF: {
            pdf_obj *pageobj = state.u.pdf->page_objs[pageno];

            if (state.trimmargins) {
                pdf_obj *obj;
                pdf_page *page;

                fz_try (state.ctx) {
                    page = pdf_load_page (state.u.pdf, pageno);
                    obj = pdf_dict_gets (pageobj, "llpp.TrimBox");
                    trim = state.trimanew || !obj;
                    if (trim) {
                        fz_rect rect;
                        fz_bbox bbox;
                        fz_matrix ctm;
                        fz_device *dev;

                        dev = fz_new_bbox_device (state.ctx, &bbox);
                        dev->hints |= FZ_IGNORE_SHADE;
                        ctm = fz_invert_matrix (page->ctm);
                        pdf_run_page (state.u.pdf, page, dev, fz_identity, NULL);
                        fz_free_device (dev);

                        rect.x0 = bbox.x0 + state.trimfuzz.x0;
                        rect.x1 = bbox.x1 + state.trimfuzz.x1;
                        rect.y0 = bbox.y0 + state.trimfuzz.y0;
                        rect.y1 = bbox.y1 + state.trimfuzz.y1;
                        rect = fz_transform_rect (ctm, rect);
                        rect = fz_intersect_rect (rect, page->mediabox);

                        if (fz_is_empty_rect (rect)) {
                            mediabox = page->mediabox;
                        }
                        else {
                            mediabox = rect;
                        }

                        obj = pdf_new_array (state.ctx, 4);
                        pdf_array_push (obj, pdf_new_real (state.ctx, mediabox.x0));
                        pdf_array_push (obj, pdf_new_real (state.ctx, mediabox.y0));
                        pdf_array_push (obj, pdf_new_real (state.ctx, mediabox.x1));
                        pdf_array_push (obj, pdf_new_real (state.ctx, mediabox.y1));
                        pdf_dict_puts (pageobj, "llpp.TrimBox", obj);
                    }
                    else {
                        mediabox.x0 = pdf_to_real (pdf_array_get (obj, 0));
                        mediabox.y0 = pdf_to_real (pdf_array_get (obj, 1));
                        mediabox.x1 = pdf_to_real (pdf_array_get (obj, 2));
                        mediabox.y1 = pdf_to_real (pdf_array_get (obj, 3));
                    }

                    rotate = page->rotate;
                    pdf_free_page (state.u.pdf, page);

                    show = trim ? pageno % 5 == 0 : pageno % 20 == 0;
                    if (show) {
                        printd ("progress %f Trimming %d",
                                (double) (pageno + 1) / state.pagecount,
                                pageno + 1);
                    }
                }
                fz_catch (state.ctx) {
                }
            }
            else {
                fz_rect cropbox;

                mediabox = pdf_to_rect (state.ctx,
                                        pdf_dict_gets (pageobj, "MediaBox"));
                if (fz_is_empty_rect (mediabox)) {
                    fprintf (stderr, "cannot find page size for page %d\n",
                             pageno+1);
                    mediabox.x0 = 0;
                    mediabox.y0 = 0;
                    mediabox.x1 = 612;
                    mediabox.y1 = 792;
                }

                cropbox = pdf_to_rect (state.ctx,
                                       pdf_dict_gets (pageobj, "CropBox"));
                if (!fz_is_empty_rect (cropbox)) {
                    mediabox = fz_intersect_rect (mediabox, cropbox);
                }
                rotate = pdf_to_int (pdf_dict_gets (pageobj, "Rotate"));
            }
            break;
        }

        case DXPS:
            {
                xps_page *page;

                page = xps_load_page (state.u.xps, pageno);
                mediabox = xps_bound_page (state.u.xps, page);
                rotate = 0;
                if (state.trimmargins) {
                    fz_rect rect;
                    fz_bbox bbox;
                    fz_device *dev;

                    dev = fz_new_bbox_device (state.ctx, &bbox);
                    dev->hints |= FZ_IGNORE_SHADE;
                    xps_run_page (state.u.xps, page, dev, fz_identity, NULL);
                    fz_free_device (dev);

                    rect.x0 = bbox.x0 + state.trimfuzz.x0;
                    rect.x1 = bbox.x1 + state.trimfuzz.x1;
                    rect.y0 = bbox.y0 + state.trimfuzz.y0;
                    rect.y1 = bbox.y1 + state.trimfuzz.y1;
                    rect = fz_intersect_rect (rect, mediabox);

                    if (!fz_is_empty_rect (rect)) {
                        mediabox = rect;
                    }
                }
                xps_free_page (state.u.xps, page);
                printd ("progress %f loading %d",
                        (double) (pageno + 1) / state.pagecount,
                        pageno + 1);
            }
            break;

        case DCBZ:
            {
                rotate = 0;
                if (state.trimmargins) {
                    cbz_page *page;

                    page = cbz_load_page (state.u.cbz, pageno);
                    mediabox = cbz_bound_page (state.u.cbz, page);
                    cbz_free_page (state.u.cbz, page);
                    printd ("progress %f Trimming %d",
                            (double) (pageno + 1) / state.pagecount,
                            pageno + 1);
                }
                else {
                    mediabox.x0 = mediabox.y0 = 0;
                    mediabox.x1 = 900;
                    mediabox.y1 = 900;
                }
            }
            break;

        default:
            ARSERT (0 && state.type);
        }

        if (state.pagedimcount == 0
            || (p = &state.pagedims[state.pagedimcount-1], p->rotate != rotate)
            || memcmp (&p->mediabox, &mediabox, sizeof (mediabox))) {
            size_t size;

            size = (state.pagedimcount + 1) * sizeof (*state.pagedims);
            state.pagedims = realloc (state.pagedims, size);
            if (!state.pagedims) {
                err (1, "realloc pagedims to %" FMT_s " (%d elems)",
                     size, state.pagedimcount + 1);
            }

            p = &state.pagedims[state.pagedimcount++];
            p->rotate = rotate;
            p->mediabox = mediabox;
            p->pageno = pageno;
        }
    }
    end = now ();
    if (state.trimmargins) {
        printd ("progress 1 Trimmed %d pages in %f seconds",
                state.pagecount, end - start);
    }
    else {
        printd ("vmsg Processed %d pages in %f seconds",
                state.pagecount, end - start);
    }
    state.trimanew = 0;
}

static void layout (void)
{
    int pindex;
    fz_rect box;
    fz_matrix ctm;
    double zoom, w, maxw = 0;
    struct pagedim *p = state.pagedims;

    if (state.proportional) {
        for (pindex = 0; pindex < state.pagedimcount; ++pindex, ++p) {
            box = fz_transform_rect (fz_rotate (p->rotate + state.rotate),
                                     p->mediabox);
            w = box.x1 - box.x0;
            maxw = MAX (w, maxw);
        }
    }

    p = state.pagedims;
    for (pindex = 0; pindex < state.pagedimcount; ++pindex, ++p) {
        fz_bbox bbox;

        ctm = fz_rotate (state.rotate);
        box = fz_transform_rect (fz_rotate (p->rotate + state.rotate),
                                 p->mediabox);
        w = box.x1 - box.x0;

        if (state.proportional) {
            double scale = w / maxw;
            zoom = (state.w / w) * scale;
        }
        else {
            zoom = state.w / w;
        }

        p->zoomctm = fz_scale (zoom, zoom);
        ctm = fz_concat (p->zoomctm, ctm);

        p->pagebox = fz_transform_rect (fz_rotate (p->rotate), p->mediabox);
        p->pagebox.x1 -= p->pagebox.x0;
        p->pagebox.y1 -= p->pagebox.y0;
        p->pagebox.x0 = 0;
        p->pagebox.y0 = 0;
        bbox = fz_round_rect (fz_transform_rect (ctm, p->pagebox));

        p->bounds = bbox;
        p->left = state.proportional ? ((maxw - w) * zoom) / 2.0 : 0;
        p->ctm = ctm;

        ctm = fz_identity;
        ctm = fz_concat (ctm, fz_translate (0, -p->mediabox.y1));
        ctm = fz_concat (ctm, fz_scale (zoom, -zoom));
        ctm = fz_concat (ctm, fz_rotate (p->rotate + state.rotate));
        p->lctm = ctm;

        p->tctmready = 0;
    }

    while (p-- != state.pagedims)  {
        int x0 = MIN (p->bounds.x0, p->bounds.x1);
        int y0 = MIN (p->bounds.y0, p->bounds.y1);
        int x1 = MAX (p->bounds.x0, p->bounds.x1);
        int y1 = MAX (p->bounds.y0, p->bounds.y1);
        int w = x1 - x0;
        int h = y1 - y0;

        printd ("pdim %d %d %d %d", p->pageno, w, h, p->left);
    }
}

static void recurse_outline (fz_outline *outline, int level)
{
    while (outline) {
        fz_link_dest *dest;
        int i, top = 0;
        struct pagedim *pdim = state.pagedims;

        dest = &outline->dest;
        for (i = 0; i < state.pagedimcount; ++i) {
            if (state.pagedims[i].pageno > dest->ld.gotor.page)
                break;
            pdim = &state.pagedims[i];
        }
        if (dest->ld.gotor.flags & fz_link_flag_t_valid) {
            fz_point p;
            p.x = 0;
            p.y = dest->ld.gotor.lt.y;
            p = fz_transform_point (pdim->lctm, p);
            top = p.y;
        }
        if (dest->ld.gotor.page >= 0 && dest->ld.gotor.page < 1<<30) {
            int h;
            double y0, y1;

            y0 = MIN (pdim->bounds.y0, pdim->bounds.y1);
            y1 = MAX (pdim->bounds.y0, pdim->bounds.y1);
            h = y1 - y0;
            printd ("o %d %d %d %d %s",
                    level, dest->ld.gotor.page, top, h, outline->title);
        }
        if (outline->down) {
            recurse_outline (outline->down, level + 1);
        }
        outline = outline->next;
    }
}

static void process_outline (void)
{
    fz_outline *outline;

    if (!state.needoutline) return;

    state.needoutline = 0;
    switch (state.type) {
    case DPDF:
        outline = pdf_load_outline (state.u.pdf);
        break;
    case DXPS:
        outline = xps_load_outline (state.u.xps);
        break;
    default:
        outline = NULL;
        break;
    }
    if (outline) {
        recurse_outline (outline, 0);
        fz_free_outline (state.ctx, outline);
    }
}

static char *strofspan (fz_text_span *span)
{
    char *p;
    char utf8[10];
    fz_text_char *ch;
    size_t size = 0, cap = 80;

    p = malloc (cap + 1);
    if (!p) return NULL;

    for (ch = span->text; ch < span->text + span->len; ++ch) {
        int n = fz_runetochar (utf8, ch->c);
        if (size + n > cap) {
            cap *= 2;
            p = realloc (p, cap + 1);
            if (!p) return NULL;
        }

        memcpy (p + size, utf8, n);
        size += n;
    }
    p[size] = 0;
    return p;
}

static int matchspan (regex_t *re, fz_text_span *span, fz_matrix ctm,
                      int stop, int pageno, double start)
{
    int ret;
    char *p;
    regmatch_t rm;
    int a, b, c;
    fz_rect *sb, *eb;
    fz_point p1, p2, p3, p4;

    p = strofspan (span);
    if (!p) return -1;

    ret = regexec (re, p, 1, &rm, 0);
    if (ret)  {
        free (p);
        if (ret != REG_NOMATCH) {
            size_t size;
            char errbuf[80];
            size = regerror (ret, re, errbuf, sizeof (errbuf));
            printd ("msg regexec error `%.*s'",
                    (int) size, errbuf);
            return -1;
        }
        return 0;
    }
    else  {
        int l = span->len;
        for (a = 0, c = 0; c < rm.rm_so && a < l; a++) {
            c += fz_runelen (span->text[a].c);
        }
        for (b = a; c < rm.rm_eo - 1 && b < l; b++) {
            c += fz_runelen (span->text[b].c);
        }

        if (fz_runelen (span->text[b].c) > 1) {
            b = MAX (0, b-1);
        }
        sb = &span->text[MIN (a, l-1)].bbox;
        eb = &span->text[MIN (b, l-1)].bbox;

        p1.x = sb->x0;
        p1.y = sb->y0;
        p2.x = eb->x1;
        p2.y = sb->y0;
        p3.x = eb->x1;
        p3.y = eb->y1;
        p4.x = sb->x0;
        p4.y = eb->y1;

        if (!stop) {
            printd ("firstmatch %d %d %f %f %f %f %f %f %f %f",
                    pageno, 1,
                    p1.x, p1.y,
                    p2.x, p2.y,
                    p3.x, p3.y,
                    p4.x, p4.y);

            printd ("progress 1 found at %d `%.*s' in %f sec",
                    pageno + 1, (int) (rm.rm_eo - rm.rm_so), &p[rm.rm_so],
                    now () - start);
        }
        else  {
            printd ("match %d %d %f %f %f %f %f %f %f %f",
                    pageno, 2,
                    p1.x, p1.y,
                    p2.x, p2.y,
                    p3.x, p3.y,
                    p4.x, p4.y);
        }
        free (p);
        return 1;
    }
}

static int compareblocks (const void *l, const void *r)
{
    fz_text_block const *ls = l;
    fz_text_block const* rs = r;
    return ls->bbox.y0 - rs->bbox.y0;
}

/* wishful thinking function */
static void search (regex_t *re, int pageno, int y, int forward)
{
    int i, j;
    fz_matrix ctm;
    fz_device *tdev;
    union { void *ptr; pdf_page *pdfpage; xps_page *xpspage; } u;
    fz_text_page *text;
    fz_text_sheet *sheet;
    struct pagedim *pdim, *pdimprev;
    int stop = 0, niters = 0;
    double start, end;

    if (!(state.type == DPDF || state.type == DXPS))
        return;

    start = now ();
    while (pageno >= 0 && pageno < state.pagecount && !stop) {
        if (niters++ == 5) {
            niters = 0;
            if (hasdata ()) {
                printd ("progress 1 attention requested aborting search at %d",
                        pageno);
                stop = 1;
            }
            else {
                printd ("progress %f searching in page %d",
                        (double) (pageno + 1) / state.pagecount,
                        pageno);
            }
        }
        pdimprev = NULL;
        for (i = 0; i < state.pagedimcount; ++i) {
            pdim = &state.pagedims[i];
            if (pdim->pageno == pageno)  {
                goto found;
            }
            if (pdim->pageno > pageno) {
                pdim = pdimprev;
                goto found;
            }
            pdimprev = pdim;
        }
        pdim = pdimprev;
    found:

        sheet = fz_new_text_sheet (state.ctx);
        text = fz_new_text_page (state.ctx, fz_infinite_rect);
        tdev = fz_new_text_device (state.ctx, sheet, text);

        switch (state.type) {
        case DPDF:
            u.ptr = NULL;
            fz_try (state.ctx) {
                u.pdfpage = pdf_load_page (state.u.pdf, pageno);
                trimctm (u.pdfpage, pdim - state.pagedims);
                ctm = fz_concat (pdim->tctm, pdim->zoomctm);
                pdf_run_page (state.u.pdf, u.pdfpage, tdev, ctm, NULL);
            }
            fz_catch (state.ctx) {
                fz_free_device (tdev);
                u.ptr = NULL;
                goto nextiter;
            }
            break;

        case DXPS:
            u.xpspage = xps_load_page (state.u.xps, pageno);
            ctm = pdim->ctm;
            xps_run_page (state.u.xps, u.xpspage, tdev, ctm, NULL);
            break;

        default:
            ARSERT (0 && state.type);
        }

        qsort (text->blocks, text->len, sizeof (*text->blocks), compareblocks);
        fz_free_device (tdev);

        for (j = 0; j < text->len; ++j) {
            int k;
            fz_text_block *block;

            block = &text->blocks[forward ? j : text->len - 1 - j];

            for (k = 0; k < block->len; ++k) {
                fz_text_line *line;
                fz_text_span *span;

                if (forward) {
                    line = &block->lines[k];
                    if (line->bbox.y0 < y + 1) continue;
                }
                else {
                    line = &block->lines[block->len - 1 - k];
                    if (line->bbox.y0 > y - 1) continue;
                }

                for (span = line->spans;
                     span < line->spans + line->len;
                     ++span) {

                    switch (matchspan (re, span, ctm, stop, pageno, start)) {
                    case 0: break;
                    case 1: stop = 1; break;
                    case -1: stop = 1; goto endloop;
                    }
                }
            }
        }
    nextiter:
        if (forward) {
            pageno += 1;
            y = 0;
        }
        else {
            pageno -= 1;
            y = INT_MAX;
        }
    endloop:
        fz_free_text_page (state.ctx, text);
        fz_free_text_sheet (state.ctx, sheet);
        if (u.ptr) {
            state.freepage (u.ptr);
        }
    }
    end = now ();
    if (!stop)  {
        printd ("progress 1 no matches %f sec", end - start);
    }
    printd ("clearrects");
}

static void set_tex_params (int colorspace)
{
    union {
        unsigned char b;
        unsigned int s;
    } endianness = {1};

    switch (colorspace) {
    case 0:
        state.texiform = GL_RGBA8;
        state.texform = GL_RGBA;
        state.texty = GL_UNSIGNED_BYTE;
        state.colorspace = fz_device_rgb;
        break;
    case 1:
        state.texiform = GL_RGBA8;
        state.texform = GL_BGRA;
        state.texty = endianness.s > 1
            ? GL_UNSIGNED_INT_8_8_8_8
            : GL_UNSIGNED_INT_8_8_8_8_REV;
        state.colorspace = fz_device_bgr;
        break;
    case 2:
        state.texiform = GL_LUMINANCE_ALPHA;
        state.texform = GL_LUMINANCE_ALPHA;
        state.texty = GL_UNSIGNED_BYTE;
        state.colorspace = fz_device_gray;
        break;
    default:
        errx (1, "invalid colorspce %d", colorspace);
    }
}

static void realloctexts (int texcount)
{
    size_t size;

    if (texcount == state.texcount) return;

    if (texcount < state.texcount) {
        glDeleteTextures (state.texcount - texcount,
                          state.texids + texcount);
    }

    size = texcount * sizeof (*state.texids);
    state.texids = realloc (state.texids, size);
    if (!state.texids) {
        err (1, "realloc texids %" FMT_s, size);
    }

    size = texcount * sizeof (*state.texowners);
    state.texowners = realloc (state.texowners, size);
    if (!state.texowners) {
        err (1, "realloc texowners %" FMT_s, size);
    }
    if (texcount > state.texcount) {
        int i;

        glGenTextures (texcount - state.texcount,
                       state.texids + state.texcount);
        for (i = state.texcount; i < texcount; ++i) {
            state.texowners[i].w = -1;
            state.texowners[i].slice = NULL;
        }
    }
    state.texcount = texcount;
    state.texindex = 0;
}

static void * mainloop (void *unused)
{
    char *p = NULL;
    int len, ret, oldlen = 0;

    for (;;) {
        len = readlen ();
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
        readdata (p, len);
        p[len] = 0;

        if (!strncmp ("open", p, 4)) {
            size_t filenamelen;
            char *password;
            char *filename = p + 5;

            filenamelen = strlen (filename);
            password = filename + filenamelen + 1;

            openxref (filename, password);
            pdfinfo ();
            initpdims ();
            printd ("msg Opened %s (press h/F1 to get help)", filename);
            state.needoutline = 1;
        }
        else if (!strncmp ("cs", p, 2)) {
            int i, colorspace;

            ret = sscanf (p + 2, " %d", &colorspace);
            if (ret != 1) {
                errx (1, "malformed cs `%.*s' ret=%d", len, p, ret);
            }
            lock ("cs");
            set_tex_params (colorspace);
            for (i = 0; i < state.texcount; ++i)  {
                state.texowners[i].w = -1;
                state.texowners[i].slice = NULL;
            }
            unlock ("cs");
        }
        else if (!strncmp ("freepage", p, 8)) {
            void *ptr;

            ret = sscanf (p + 8, " %" FMT_ptr, FMT_ptr_cast (&ptr));
            if (ret != 1) {
                errx (1, "malformed freepage `%.*s' ret=%d", len, p, ret);
            }
            freepage (ptr);
        }
        else if (!strncmp ("freetile", p, 8)) {
            void *ptr;

            ret = sscanf (p + 8, " %" FMT_ptr, FMT_ptr_cast (&ptr));
            if (ret != 1) {
                errx (1, "malformed freetile `%.*s' ret=%d", len, p, ret);
            }
            freetile (ptr);
        }
        else if (!strncmp ("search", p, 6)) {
            int icase, pageno, y, ret, len2, forward;
            char *pattern;
            regex_t re;

            ret = sscanf (p + 6, " %d %d %d %d,%n",
                          &icase, &pageno, &y, &forward, &len2);
            if (ret != 4) {
                errx (1, "malformed search `%s' ret=%d", p, ret);
            }

            pattern = p + 6 + len2;
            ret = regcomp (&re, pattern,
                           REG_EXTENDED | (icase ? REG_ICASE : 0));
            if (ret) {
                char errbuf[80];
                size_t size;

                size = regerror (ret, &re, errbuf, sizeof (errbuf));
                printd ("msg regcomp failed `%.*s'", (int) size, errbuf);
            }
            else  {
                search (&re, pageno, y, forward);
                regfree (&re);
            }
        }
        else if (!strncmp ("geometry", p, 8)) {
            int w, h;

            printd ("clear");
            ret = sscanf (p + 8, " %d %d", &w, &h);
            if (ret != 2) {
                errx (1, "malformed geometry `%.*s' ret=%d", len, p, ret);
            }

            lock ("geometry");
            state.h = h;
            if (w != state.w) {
                int i;
                state.w = w;
                for (i = 0; i < state.texcount; ++i)  {
                    state.texowners[i].slice = NULL;
                }
            }
            layout ();
            process_outline ();
            state.gen++;
            unlock ("geometry");
            printd ("continue %d", state.pagecount);
        }
        else if (!strncmp ("reqlayout", p, 9)) {
            int rotate, proportional;

            printd ("clear");
            ret = sscanf (p + 9, " %d %d", &rotate, &proportional);
            if (ret != 2) {
                errx (1, "bad reqlayout line `%.*s' ret=%d", len, p, ret);
            }
            lock ("reqlayout");
            if (state.rotate != rotate || state.proportional != proportional) {
                state.gen += 1;
            }
            state.rotate = rotate;
            state.proportional = proportional;
            layout ();
            process_outline ();
            state.gen++;
            unlock ("reqlayout");
            printd ("continue %d", state.pagecount);
        }
        else if (!strncmp ("page", p, 4)) {
            double a, b;
            struct page *page;
            int pageno, pindex, ret;

            ret = sscanf (p + 4, " %d %d", &pageno, &pindex);
            if (ret != 2) {
                errx (1, "bad render line `%.*s' ret=%d", len, p, ret);
            }

            lock ("page");
            a = now ();
            page = loadpage (pageno, pindex);
            b = now ();
            unlock ("page");

            printd ("page %" FMT_ptr " %f", FMT_ptr_cast2 (page), b - a);
        }
        else if (!strncmp ("tile", p, 4)) {
            int x, y, w, h, ret;
            struct page *page;
            struct tile *tile;
            double a, b;

            ret = sscanf (p + 4, " %" FMT_ptr " %d %d %d %d",
                          FMT_ptr_cast (&page), &x, &y, &w, &h);
            if (ret != 5) {
                errx (1, "bad tile line `%.*s' ret=%d", len, p, ret);
            }

            lock ("tile");
            a = now ();
            tile = rendertile (page, x, y, w, h);
            b = now ();
            unlock ("tile");

            printd ("tile %d %d %" FMT_ptr " %u %f",
                    x, y,
                    FMT_ptr_cast2 (tile),
                    tile->w * tile->h * tile->pixmap->n,
                    b - a);
        }
        else if (!strncmp ("settrim", p, 7))  {
            int trimmargins;
            fz_bbox fuzz;

            ret = sscanf (p + 7, " %d %d %d %d %d", &trimmargins,
                          &fuzz.x0, &fuzz.y0, &fuzz.x1, &fuzz.y1);
            if (ret != 5) {
                errx (1, "malformed settrim `%.*s' ret=%d", len, p, ret);
            }
            printd ("clear");
            lock ("settrim");
            state.trimmargins = trimmargins;
            state.needoutline = 1;
            if (memcmp (&fuzz, &state.trimfuzz, sizeof (fuzz))) {
                state.trimanew = 1;
                state.trimfuzz = fuzz;
            }
            state.pagedimcount = 0;
            free (state.pagedims);
            state.pagedims = NULL;
            initpdims ();
            layout ();
            process_outline ();
            unlock ("settrim");
            printd ("continue %d", state.pagecount);
        }
        else if (!strncmp ("sliceh", p, 6)) {
            int h;

            ret = sscanf (p + 6, " %d", &h);
            if (ret != 1) {
                errx (1, "malformed sliceh `%.*s' ret=%d", len, p, ret);
            }
            if (h != state.sliceheight) {
                int i;

                state.sliceheight = h;
                for (i = 0; i < state.texcount; ++i) {
                    state.texowners[i].w = -1;
                    state.texowners[i].h = -1;
                    state.texowners[i].slice = NULL;
                }
            }
        }
        else if (!strncmp ("interrupt", p, 9)) {
            printd ("vmsg interrupted");
        }
        else {
            errx (1, "unknown command %.*s", len, p);
        }
    }
    return 0;
}

CAMLprim value ml_realloctexts (value texcount_v)
{
    CAMLparam1 (texcount_v);
    int ok;

    if (trylock ("ml_realloctexts")) {
        ok = 0;
        goto done;
    }
    realloctexts (Int_val (texcount_v));
    ok = 1;
    unlock ("ml_realloctexts");

 done:
    CAMLreturn (Val_bool (ok));
}

static void showsel (struct page *page, int ox, int oy)
{
    int seen = 0;
    fz_bbox bbox;
    fz_text_line *line;
    fz_text_span *span;
    fz_text_block *block;
    struct mark first, last;

    first = page->fmark;
    last = page->lmark;

    if (!first.span || !last.span) return;

    glEnable (GL_BLEND);
    glBlendFunc (GL_SRC_ALPHA, GL_SRC_ALPHA);
    glColor4f (0.5f, 0.5f, 0.0f, 0.6f);

    ox += state.pagedims[page->pdimno].bounds.x0;
    oy += state.pagedims[page->pdimno].bounds.y0;
    for (block = page->text->blocks;
         block < page->text->blocks + page->text->len;
         ++block) {
        for (line = block->lines;
             line < block->lines + block->len;
             ++line) {
            for (span = line->spans;
                 span < line->spans + line->len;
                 ++span) {
                int i, j, k;

                bbox.x0 = bbox.y0 = bbox.x1 = bbox.y1 = 0;

                j = 0;
                k = span->len - 1;

                if (span == page->fmark.span && span == page->lmark.span) {
                    seen = 1;
                    j = MIN (first.i, last.i);
                    k = MAX (first.i, last.i);
                }
                else if (span == first.span) {
                    seen = 1;
                    j = first.i;
                }
                else if (span == last.span) {
                    seen = 1;
                    k = last.i;
                }

                if (seen) {
                    for (i = j; i <= k; ++i) {
                        fz_bbox b = fz_round_rect (span->text[i].bbox);
                        bbox = fz_union_bbox (bbox, b);
                    }
                    lprintf ("%d %d %d %d oy=%d ox=%d\n",
                             bbox.x0,
                             bbox.y0,
                             bbox.x1,
                             bbox.y1,
                             oy, ox);

                    glRecti (bbox.x0 + ox, bbox.y0 + oy,
                             bbox.x1 + ox, bbox.y1 + oy);
                    if (span == last.span) {
                        goto done;
                    }
                }
            }
        }
    }
 done:
    glDisable (GL_BLEND);
}

#include "glfont.c"

static void highlightlinks (struct page *page, int xoff, int yoff)
{
    fz_matrix ctm;
    fz_link *link, *links;

    switch (page->type) {
    case DPDF:
        links = page->u.pdfpage->links;
        break;

    case DXPS:
        links = page->u.xpspage->links;
        break;

    default:
        return;
    }

    glPolygonMode (GL_FRONT_AND_BACK, GL_LINE);
    glEnable (GL_LINE_STIPPLE);
    glLineStipple (0.5, 0xcccc);

    xoff -= state.pagedims[page->pdimno].bounds.x0;
    yoff -= state.pagedims[page->pdimno].bounds.y0;
    ctm = fz_concat (pagectm (page), fz_translate (xoff, yoff));

    glBegin (GL_QUADS);
    for (link = links; link; link = link->next) {
        fz_point p1, p2, p3, p4;

        p1.x = link->rect.x0;
        p1.y = link->rect.y0;

        p2.x = link->rect.x1;
        p2.y = link->rect.y0;

        p3.x = link->rect.x1;
        p3.y = link->rect.y1;

        p4.x = link->rect.x0;
        p4.y = link->rect.y1;

        p1 = fz_transform_point (ctm, p1);
        p2 = fz_transform_point (ctm, p2);
        p3 = fz_transform_point (ctm, p3);
        p4 = fz_transform_point (ctm, p4);

        switch (link->dest.kind) {
        case FZ_LINK_GOTO: glColor3ub (255, 0, 0); break;
        case FZ_LINK_URI: glColor3ub (0, 0, 255); break;
        default: glColor3ub (0, 0, 0); break;
        }

        glVertex2f (p1.x, p1.y);
        glVertex2f (p2.x, p2.y);
        glVertex2f (p3.x, p3.y);
        glVertex2f (p4.x, p4.y);
    }
    glEnd ();

    glPolygonMode (GL_FRONT_AND_BACK, GL_FILL);
    glDisable (GL_LINE_STIPPLE);
}

static int compareslinks (const void *l, const void *r)
{
    struct slink const *ls = l;
    struct slink const *rs = r;
    if (ls->bbox.y0 == rs->bbox.y0) {
        return rs->bbox.x0 - rs->bbox.x0;
    }
    return ls->bbox.y0 - rs->bbox.y0;
}

static void droptext (struct page *page)
{
    if (page->text) {
        fz_free_text_page (state.ctx, page->text);
        page->fmark.i = -1;
        page->lmark.i = -1;
        page->fmark.span = NULL;
        page->lmark.span = NULL;
        page->text = NULL;
    }
    if (page->sheet) {
        fz_free_text_sheet (state.ctx, page->sheet);
    }
}

static void dropslinks (struct page *page)
{
    if (page->slinks) {
        free (page->slinks);
        page->slinks = NULL;
        page->slinkcount = 0;
    }
}

static void ensureslinks (struct page *page)
{
    fz_matrix ctm;
    int i, count = 0;
    size_t slinksize = sizeof (*page->slinks);
    fz_link *link, *links;

    if (state.gen != page->sgen) {
        dropslinks (page);
        page->sgen = state.gen;
    }
    if (page->slinks) return;

    switch (page->type) {
    case DPDF:
        links = page->u.pdfpage->links;
        ctm = fz_concat (trimctm (page->u.pdfpage, page->pdimno),
                         state.pagedims[page->pdimno].ctm);
        break;

    case DXPS:
        links = page->u.xpspage->links;
        ctm = state.pagedims[page->pdimno].ctm;
        break;

    default:
        return;
    }

    for (link = links; link; link = link->next) {
        count++;
    }
    if (count > 0) {
        page->slinkcount = count;
        page->slinks = calloc (count, slinksize);
        if (!page->slinks) {
            err (1, "realloc slinks %d", count);
        }

        for (i = 0, link = links; link; ++i, link = link->next) {
            page->slinks[i].link = link;
            page->slinks[i].bbox =
                fz_round_rect (fz_transform_rect (ctm, link->rect));
        }
        qsort (page->slinks, count, slinksize, compareslinks);
    }
}

/* slightly tweaked fmt_ulong by D.J. Bernstein */
static void fmt_linkn (char *s, unsigned int u)
{
  unsigned int len; unsigned int q;
  int zma = 'z' - 'a' + 1;
  len = 1; q = u;
  while (q > zma - 1) { ++len; q /= zma; }
  if (s) {
    s += len;
    do { *--s = 'a' + (u % zma) - (u < zma && len > 1); u /= zma; } while(u);
    /* handles u == 0 */
  }
  s[len] = 0;
}

static void highlightslinks (struct page *page, int xoff, int yoff,
                             int noff, char *targ, int tlen, int hfsize)
{
    int i;
    char buf[40];
    struct slink *slink;
    double x0, y0, x1, y1, w;

    ensureslinks (page);
    glColor3ub (0xc3, 0xb0, 0x91);
    for (i = 0; i < page->slinkcount; ++i) {
        fmt_linkn (buf, i + noff);
        if (!tlen || !strncmp (targ, buf, tlen)) {
            slink = &page->slinks[i];

            x0 = slink->bbox.x0 + xoff - 5;
            y1 = slink->bbox.y0 + yoff - 5;
            y0 = y1 + 10 + hfsize;
            w = measure_string (state.face, hfsize, buf);
            x1 = x0 + w + 10;
            glRectd (x0, y0, x1, y1);
        }
    }

    glEnable (GL_BLEND);
    glBlendFunc (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable (GL_TEXTURE_2D);
    glColor3ub (0, 0, 0);
    for (i = 0; i < page->slinkcount; ++i) {
        fmt_linkn (buf, i + noff);
        if (!tlen || !strncmp (targ, buf, tlen)) {
            slink = &page->slinks[i];

            x0 = slink->bbox.x0 + xoff;
            y0 = slink->bbox.y0 + yoff + hfsize;
            draw_string (state.face, hfsize, x0, y0, buf);
        }
    }
    glDisable (GL_TEXTURE_2D);
    glDisable (GL_BLEND);
}


static void uploadslice (struct tile *tile, struct slice *slice)
{
    int offset;
    struct slice *slice1;

    offset = 0;
    for (slice1 = tile->slices; slice != slice1; slice1++) {
        offset += slice1->h * tile->w * tile->pixmap->n;
    }
    if (slice->texindex != -1 && slice->texindex < state.texcount
        && state.texowners[slice->texindex].slice == slice) {
        glBindTexture (GL_TEXTURE_RECTANGLE_ARB, state.texids[slice->texindex]);
    }
    else {
        int subimage = 0;
        int texindex = state.texindex++ % state.texcount;

        if (state.texowners[texindex].w == tile->w) {
            if (state.texowners[texindex].h >= slice->h) {
                subimage = 1;
            }
            else {
                state.texowners[texindex].h = slice->h;
            }
        }
        else  {
            state.texowners[texindex].h = slice->h;
        }

        state.texowners[texindex].w = tile->w;
        state.texowners[texindex].slice = slice;
        slice->texindex = texindex;

        glBindTexture (GL_TEXTURE_RECTANGLE_ARB, state.texids[texindex]);
        if (subimage) {
            glTexSubImage2D (GL_TEXTURE_RECTANGLE_ARB,
                             0,
                             0,
                             0,
                             tile->w,
                             slice->h,
                             state.texform,
                             state.texty,
                             tile->pixmap->samples+offset
                );
        }
        else {
            glTexImage2D (GL_TEXTURE_RECTANGLE_ARB,
                          0,
                          state.texiform,
                          tile->w,
                          slice->h,
                          0,
                          state.texform,
                          state.texty,
                          tile->pixmap->samples+offset
                );
        }
    }
}

CAMLprim value ml_drawtile (value args_v, value ptr_v)
{
    CAMLparam2 (args_v, ptr_v);
    int dispx = Int_val (Field (args_v, 0));
    int dispy = Int_val (Field (args_v, 1));
    int dispw = Int_val (Field (args_v, 2));
    int disph = Int_val (Field (args_v, 3));
    int tilex = Int_val (Field (args_v, 4));
    int tiley = Int_val (Field (args_v, 5));
    char *s = String_val (ptr_v);
    struct tile *tile = parse_pointer ("ml_drawtile", s);

    glEnable (GL_TEXTURE_RECTANGLE_ARB);
    {
        int slicey, firstslice;
        struct slice *slice;

        firstslice = tiley / tile->sliceheight;
        slice = &tile->slices[firstslice];
        slicey = tiley % tile->sliceheight;

        while (disph > 0) {
            int dh;

            dh = slice->h - slicey;
            dh = MIN (disph, dh);
            uploadslice (tile, slice);

            glBegin (GL_QUADS);
            {
                glTexCoord2i (tilex, slicey);
                glVertex2i (dispx, dispy);

                glTexCoord2i (tilex+dispw, slicey);
                glVertex2i (dispx+dispw, dispy);

                glTexCoord2i (tilex+dispw, slicey+dh);
                glVertex2i (dispx+dispw, dispy+dh);

                glTexCoord2i (tilex, slicey+dh);
                glVertex2i (dispx, dispy+dh);
            }
            glEnd ();

            dispy += dh;
            disph -= dh;
            slice++;
            ARSERT (!(slice - tile->slices >= tile->slicecount && disph > 0));
            slicey = 0;
        }
    }
    glDisable (GL_TEXTURE_RECTANGLE_ARB);
    CAMLreturn (Val_unit);
}

CAMLprim value ml_postprocess (value ptr_v, value hlinks_v,
                               value xoff_v, value yoff_v,
                               value li_v)
{
    CAMLparam5 (ptr_v, hlinks_v, xoff_v, yoff_v, li_v);
    int xoff = Int_val (xoff_v);
    int yoff = Int_val (yoff_v);
    int noff = Int_val (Field (li_v, 0));
    char *targ = String_val (Field (li_v, 1));
    int tlen = caml_string_length (Field (li_v, 1));
    int hfsize = Int_val (Field (li_v, 2));
    char *s = String_val (ptr_v);
    int hlmask = Int_val (hlinks_v);
    struct page *page = parse_pointer ("ml_postprocess", s);

    if (hlmask & 1) highlightlinks (page, xoff, yoff);
    if (trylock ("ml_postprocess")) {
        noff = 0;
        goto done;
    }
    if (hlmask & 2) {
        highlightslinks (page, xoff, yoff, noff, targ, tlen, hfsize);
        noff = page->slinkcount;
    }
    showsel (page, xoff, yoff);
    unlock ("ml_postprocess");

 done:
    CAMLreturn (Val_int (noff));
}

static fz_link *getlink (struct page *page, int x, int y)
{
    fz_point p;
    fz_matrix ctm;
    fz_link *link, *links;

    switch (page->type) {
    case DPDF:
        ctm = trimctm (page->u.pdfpage, page->pdimno);
        links = page->u.pdfpage->links;
        break;

    case DXPS:
        ctm = fz_identity;
        links = page->u.xpspage->links;
        break;

    default:
        return NULL;
    }
    p.x = x;
    p.y = y;

    ctm = fz_concat (ctm, state.pagedims[page->pdimno].ctm);
    ctm = fz_invert_matrix (ctm);
    p = fz_transform_point (ctm, p);

    for (link = links; link; link = link->next) {
        if (p.x >= link->rect.x0 && p.x <= link->rect.x1) {
            if (p.y >= link->rect.y0 && p.y <= link->rect.y1) {
                return link;
            }
        }
    }
    return NULL;
}

static void ensuretext (struct page *page)
{
    if (state.gen != page->tgen) {
        droptext (page);
        page->tgen = state.gen;
    }
    if (!page->text) {
        fz_device *tdev;

        page->text = fz_new_text_page (state.ctx, fz_infinite_rect);
        page->sheet = fz_new_text_sheet (state.ctx);
        tdev = fz_new_text_device (state.ctx, page->sheet, page->text);
        fz_run_display_list (page->dlist,
                             tdev,
                             pagectm (page),
                             fz_infinite_bbox, NULL);
        qsort (page->text->blocks, page->text->len,
               sizeof (*page->text->blocks), compareblocks);
        fz_free_device (tdev);
    }
}

CAMLprim value ml_find_page_with_links (value start_page_v, value dir_v)
{
    CAMLparam2 (start_page_v, dir_v);
    CAMLlocal1 (ret_v);
    int i, dir = Int_val (dir_v);
    int start_page = Int_val (start_page_v);
    int end_page = dir > 0 ? state.pagecount : -1;

    ret_v = Val_int (0);
    if (!(state.type == DPDF || state.type == DXPS))  {
        goto done;
    }

    lock ("ml_findpage_with_links");
    for (i = start_page + dir; i != end_page; i += dir) {
        int found;

        switch (state.type) {
        case DPDF:
            {
                pdf_page *page = NULL;

                fz_try (state.ctx) {
                    page = pdf_load_page (state.u.pdf, i);
                    found = !!page->links;
                }
                fz_catch (state.ctx) {
                    found = 0;
                }
                if (page) {
                    freepdfpage (page);
                }
            }
            break;
        case DXPS:
            {
                xps_page *page = xps_load_page (state.u.xps, i);
                found = !!page->links;
                freexpspage (page);
            }
            break;

        default:
            ARSERT ("invalid document type");
        }

        if (found) {
            ret_v = caml_alloc_small (1, 1);
            Field (ret_v, 0) = Val_int (i);
            goto unlock;
        }
    }
 unlock:
    unlock ("ml_findpage_with_links");

 done:
    CAMLreturn (ret_v);
}

enum { dir_first, dir_last};
enum { dir_first_visible, dir_left, dir_right, dir_down, dir_up };

CAMLprim value ml_findlink (value ptr_v, value dir_v)
{
    CAMLparam2 (ptr_v, dir_v);
    CAMLlocal2 (ret_v, pos_v);
    struct page *page;
    int dirtag, i, slinkindex;
    struct slink *found = NULL ,*slink;
    char *s = String_val (ptr_v);

    page = parse_pointer ("ml_findlink", s);
    ret_v = Val_int (0);
    if (trylock ("ml_findlink")) {
        goto done;
    }

    ensureslinks (page);

    if (Is_block (dir_v)) {
        dirtag = Tag_val (dir_v);
        switch (dirtag) {
        case dir_first_visible:
            {
                int x0, y0, dir, first_index, last_index;

                pos_v = Field (dir_v, 0);
                x0 = Int_val (Field (pos_v, 0));
                y0 = Int_val (Field (pos_v, 1));
                dir = Int_val (Field (pos_v, 2));

                if (dir >= 0) {
                    dir = 1;
                    first_index = 0;
                    last_index = page->slinkcount;
                }
                else {
                    first_index = page->slinkcount - 1;
                    last_index = -1;
                }

                for (i = first_index; i != last_index; i += dir) {
                    slink = &page->slinks[i];
                    if (slink->bbox.y0 >= y0 && slink->bbox.x0 >= x0) {
                        found = slink;
                        break;
                    }
                }
            }
            break;

        case dir_left:
            slinkindex = Int_val (Field (dir_v, 0));
            found = &page->slinks[slinkindex];
            for (i = slinkindex - 1; i >= 0; --i) {
                slink = &page->slinks[i];
                if (slink->bbox.x0 < found->bbox.x0) {
                    found = slink;
                    break;
                }
            }
            break;

        case dir_right:
            slinkindex = Int_val (Field (dir_v, 0));
            found = &page->slinks[slinkindex];
            for (i = slinkindex + 1; i < page->slinkcount; ++i) {
                slink = &page->slinks[i];
                if (slink->bbox.x0 > found->bbox.x0) {
                    found = slink;
                    break;
                }
            }
            break;

        case dir_down:
            slinkindex = Int_val (Field (dir_v, 0));
            found = &page->slinks[slinkindex];
            for (i = slinkindex + 1; i < page->slinkcount; ++i) {
                slink = &page->slinks[i];
                if (slink->bbox.y0 >= found->bbox.y0) {
                    found = slink;
                    break;
                }
            }
            break;

        case dir_up:
            slinkindex = Int_val (Field (dir_v, 0));
            found = &page->slinks[slinkindex];
            for (i = slinkindex - 1; i >= 0; --i) {
                slink = &page->slinks[i];
                if (slink->bbox.y0 <= found->bbox.y0) {
                    found = slink;
                    break;
                }
            }
            break;
        }
    }
    else {
        dirtag = Int_val (dir_v);
        switch (dirtag) {
        case dir_first:
            found = page->slinks;
            break;

        case dir_last:
            if (page->slinks) {
                found = page->slinks + (page->slinkcount - 1);
            }
            break;
        }
    }
    if (found) {
        ret_v = caml_alloc_small (2, 1);
        Field (ret_v, 0) = Val_int (found - page->slinks);
    }

    unlock ("ml_findlink");
 done:
    CAMLreturn (ret_v);
}

enum  { uuri, ugoto, utext, uunexpected, ulaunch, unamed, uremote };

#define LINKTOVAL                                                       \
{                                                                       \
    int pageno;                                                         \
                                                                        \
    switch (link->dest.kind) {                                          \
    case FZ_LINK_GOTO:                                                  \
        {                                                               \
            fz_point p;                                                 \
                                                                        \
            pageno = link->dest.ld.gotor.page;                          \
            p.x = 0;                                                    \
            p.y = 0;                                                    \
                                                                        \
            if (link->dest.ld.gotor.flags & fz_link_flag_t_valid) {     \
                p.y = link->dest.ld.gotor.lt.y;                         \
                p = fz_transform_point (pdim->lctm, p);                 \
            }                                                           \
            tup_v = caml_alloc_tuple (2);                               \
            ret_v = caml_alloc_small (1, ugoto);                        \
            Field (tup_v, 0) = Val_int (pageno);                        \
            Field (tup_v, 1) = Val_int (p.y);                           \
            Field (ret_v, 0) = tup_v;                                   \
        }                                                               \
        break;                                                          \
                                                                        \
    case FZ_LINK_URI:                                                   \
        str_v = caml_copy_string (link->dest.ld.uri.uri);               \
        ret_v = caml_alloc_small (1, uuri);                             \
        Field (ret_v, 0) = str_v;                                       \
        break;                                                          \
                                                                        \
    case FZ_LINK_LAUNCH:                                                \
        str_v = caml_copy_string (link->dest.ld.launch.file_spec);      \
        ret_v = caml_alloc_small (1, ulaunch);                          \
        Field (ret_v, 0) = str_v;                                       \
        break;                                                          \
                                                                        \
    case FZ_LINK_NAMED:                                                 \
        str_v = caml_copy_string (link->dest.ld.named.named);           \
        ret_v = caml_alloc_small (1, unamed);                           \
        Field (ret_v, 0) = str_v;                                       \
        break;                                                          \
                                                                        \
    case FZ_LINK_GOTOR:                                                 \
        str_v = caml_copy_string (link->dest.ld.gotor.file_spec);       \
        pageno = link->dest.ld.gotor.page;                              \
        tup_v = caml_alloc_tuple (2);                                   \
        ret_v = caml_alloc_small (1, uremote);                          \
        Field (tup_v, 0) = str_v;                                       \
        Field (tup_v, 1) = Val_int (pageno);                            \
        Field (ret_v, 0) = tup_v;                                       \
        break;                                                          \
                                                                        \
    default:                                                            \
        {                                                               \
            char buf[80];                                               \
                                                                        \
            snprintf (buf, sizeof (buf),                                \
                      "unhandled link kind %d", link->dest.kind);       \
            str_v = caml_copy_string (buf);                             \
            ret_v = caml_alloc_small (1, uunexpected);                  \
            Field (ret_v, 0) = str_v;                                   \
        }                                                               \
        break;                                                          \
    }                                                                   \
}

CAMLprim value ml_getlink (value ptr_v, value n_v)
{
    CAMLparam2 (ptr_v, n_v);
    CAMLlocal3 (ret_v, tup_v, str_v);
    fz_link *link;
    struct page *page;
    struct pagedim *pdim;
    char *s = String_val (ptr_v);

    ret_v = Val_int (0);
    if (trylock ("ml_getlink")) {
        goto done;
    }

    page = parse_pointer ("ml_getlink", s);
    ensureslinks (page);
    pdim = &state.pagedims[page->pdimno];
    link = page->slinks[Int_val (n_v)].link;
    LINKTOVAL;

    unlock ("ml_getlink");
 done:
    CAMLreturn (ret_v);
}

CAMLprim value ml_getlinkcount (value ptr_v)
{
    CAMLparam1 (ptr_v);
    struct page *page;
    char *s = String_val (ptr_v);

    page = parse_pointer ("ml_getlinkcount", s);
    CAMLreturn (Val_int (page->slinkcount));
}

CAMLprim value ml_getlinkrect (value ptr_v, value n_v)
{
    CAMLparam2 (ptr_v, n_v);
    CAMLlocal1 (ret_v);
    struct page *page;
    struct slink *slink;
    char *s = String_val (ptr_v);

    page = parse_pointer ("ml_getlinkrect", s);
    ret_v = caml_alloc_tuple (4);
    if (trylock ("ml_getlinkrect")) {
        Field (ret_v, 0) = Val_int (0);
        Field (ret_v, 1) = Val_int (0);
        Field (ret_v, 2) = Val_int (0);
        Field (ret_v, 3) = Val_int (0);
        goto done;
    }
    ensureslinks (page);

    slink = &page->slinks[Int_val (n_v)];
    Field (ret_v, 0) = Val_int (slink->bbox.x0);
    Field (ret_v, 1) = Val_int (slink->bbox.y0);
    Field (ret_v, 2) = Val_int (slink->bbox.x1);
    Field (ret_v, 3) = Val_int (slink->bbox.y1);
    unlock ("ml_getlinkrect");

 done:
    CAMLreturn (ret_v);
}

CAMLprim value ml_whatsunder (value ptr_v, value x_v, value y_v)
{
    CAMLparam3 (ptr_v, x_v, y_v);
    CAMLlocal3 (ret_v, tup_v, str_v);
    fz_link *link;
    struct page *page;
    char *s = String_val (ptr_v);
    int x = Int_val (x_v), y = Int_val (y_v);
    struct pagedim *pdim;

    ret_v = Val_int (0);
    if (trylock ("ml_whatsunder")) {
        goto done;
    }

    page = parse_pointer ("ml_whatsunder", s);
    pdim = &state.pagedims[page->pdimno];
    x += pdim->bounds.x0;
    y += pdim->bounds.y0;
    link = getlink (page, x, y);
    if (link) {
        LINKTOVAL;
    }
    else {
        fz_rect *b;
        fz_text_block *block;

        ensuretext (page);
        for (block = page->text->blocks;
             block < page->text->blocks + page->text->len;
             ++block) {
            fz_text_line *line;

            b = &block->bbox;
            if (!(x >= b->x0 && x <= b->x1 && y >= b->y0 && y <= b->y1))
                continue;

            for (line = block->lines;
                 line < block->lines + block->len;
                 ++line) {
                fz_text_span *span;

                b = &line->bbox;
                if (!(x >= b->x0 && x <= b->x1 && y >= b->y0 && y <= b->y1))
                    continue;

                for (span = line->spans;
                     span < line->spans + line->len;
                     ++span) {
                    fz_text_char *ch;

                    b = &span->bbox;
                    if (!(x >= b->x0 && x <= b->x1 && y >= b->y0 && y <= b->y1))
                        continue;

                    for (ch = span->text; ch < span->text + span->len; ++ch) {
                        b = &ch->bbox;

                        if (x >= b->x0 && x <= b->x1
                            && y >= b->y0 && y <= b->y1) {
                            const char *n2 =
                                span->style->font && span->style->font->name
                                ? span->style->font->name
                                : "Span has no font name"
                                ;
                            FT_FaceRec *face = span->style->font->ft_face;
                            if (face && face->family_name) {
                                char *s;
                                char *n1 = face->family_name;
                                size_t l1 = strlen (n1);
                                size_t l2 = strlen (n2);

                                if (l1 != l2 || memcmp (n1, n2, l1)) {
                                    s = malloc (l1 + l2 + 2);
                                    if (s) {
                                        memcpy (s, n2, l2);
                                        s[l2] = '=';
                                        memcpy (s + l2 + 1, n1, l1 + 1);
                                        str_v = caml_copy_string (s);
                                        free (s);
                                    }
                                }
                            }
                            if (str_v == 0) {
                                str_v = caml_copy_string (n2);
                            }
                            ret_v = caml_alloc_small (1, utext);
                            Field (ret_v, 0) = str_v;
                            goto unlock;
                        }
                    }
                }
            }
        }
    }
 unlock:
    unlock ("ml_whatsunder");

 done:
    CAMLreturn (ret_v);
}

CAMLprim value ml_seltext (value ptr_v, value rect_v)
{
    CAMLparam2 (ptr_v, rect_v);
    fz_rect *b;
    struct page *page;
    struct pagedim *pdim;
    int i, x0, x1, y0, y1;
    char *s = String_val (ptr_v);
    int fi = 0, li = 0;
    fz_text_block *block;
    fz_text_span *span, *fspan, *lspan;
    fz_text_line *line, *fline = NULL, *lline = NULL;

    if (trylock ("ml_seltext")) {
        goto done;
    }

    page = parse_pointer ("ml_seltext", s);
    ensuretext (page);

    pdim = &state.pagedims[page->pdimno];
    x0 = Int_val (Field (rect_v, 0)) + pdim->bounds.x0;;
    y0 = Int_val (Field (rect_v, 1)) + pdim->bounds.y0;
    x1 = Int_val (Field (rect_v, 2)) + pdim->bounds.x0;
    y1 = Int_val (Field (rect_v, 3)) + pdim->bounds.y0;

    if (0) {
        glPolygonMode (GL_FRONT_AND_BACK, GL_LINE);
        glColor3ub (128, 128, 128);
        glRecti (x0, y0, x1, y1);
        glPolygonMode (GL_FRONT_AND_BACK, GL_FILL);
    }

    fspan = lspan = NULL;

    for (block = page->text->blocks;
         block < page->text->blocks + page->text->len;
         ++block) {
        for (line = block->lines;
             line < block->lines + block->len;
             ++line) {
            for (span = line->spans;
                 span < line->spans + line->len;
                 ++span) {
                for (i = 0; i < span->len; ++i) {
                    b = &span->text[i].bbox;
                    int selected = 0;

                    if (x0 >= b->x0 && x0 <= b->x1
                        && y0 >= b->y0 && y0 <= b->y1) {
                        fspan = span;
                        fline = line;
                        fi = i;
                        selected = 1;
                    }
                    if (x1 >= b->x0 && x1 <= b->x1
                        && y1 >= b->y0 && y1 <= b->y1) {
                        lspan = span;
                        lline = line;
                        li = i;
                        selected = 1;
                    }
                    if (0 && selected) {
                        glPolygonMode (GL_FRONT_AND_BACK, GL_LINE);
                        glColor3ub (128, 128, 128);
                        glRecti (b->x0, b->y0, b->x1, b->y1);
                        glPolygonMode (GL_FRONT_AND_BACK, GL_FILL);
                    }
                }
            }
        }
    }
    if (y1 < y0 || x1 < x0) {
        int swap = 0;

        if (fspan == lspan)  {
            swap = 1;
        }
        else {
            if (y1 < y0) {
                if (fline != lline) {
                    swap = 1;
                }
            }
        }

        if (swap) {
            i = fi;
            span = fspan;

            fi = li;
            fspan = lspan;

            li = i;
            lspan = span;
        }
    }

    page->fmark.i = fi;
    page->fmark.span = fspan;

    page->lmark.i = li;
    page->lmark.span = lspan;

    unlock ("ml_seltext");

 done:
    CAMLreturn (Val_unit);
}

static int UNUSED pipespan (FILE *f, fz_text_span *span, int a, int b)
{
    char buf[4];
    int i, len, ret;

    for (i = a; i <= b; ++i) {
        len = fz_runetochar (buf, span->text[i].c);
        ret = fwrite (buf, len, 1, f);

        if (ret != 1) {
            fprintf (stderr, "failed to write %d bytes ret=%d: %s\n",
                     len, ret, strerror (errno));
            return -1;
        }
    }
    return 0;
}

#ifdef __CYGWIN__
value ml_popen (value UNUSED u1, value UNUSED u2)
{
    caml_failwith ("ml_popen not implemented under Cygwin");
}
#else
CAMLprim value ml_popen (value command_v, value fds_v)
{
    CAMLparam2 (command_v, fds_v);
    CAMLlocal2 (l_v, tup_v);
    int ret;
    char *msg = NULL;
    value earg_v = Nothing;
    posix_spawnattr_t attr;
    posix_spawn_file_actions_t fa;
    char *argv[] = { "/bin/sh", "-c", String_val (command_v), NULL };

    if ((ret = posix_spawn_file_actions_init (&fa)) != 0) {
        unix_error (ret, "posix_spawn_file_actions_init", Nothing);
    }

    if ((ret = posix_spawnattr_init (&attr)) != 0) {
        msg = "posix_spawnattr_init";
        goto fail1;
    }

#ifdef POSIX_SPAWN_USEVFORK
    if ((ret = posix_spawnattr_setflags (&attr, POSIX_SPAWN_USEVFORK)) != 0) {
        msg =  "posix_spawnattr_setflags POSIX_SPAWN_USEVFORK";
        goto fail;
    }
#endif

    for (l_v = fds_v; l_v != Val_int (0); l_v = Field (l_v, 1)) {
        int fd1, fd2;

        tup_v = Field (l_v, 0);
        fd1 = Int_val (Field (tup_v, 0));
        fd2 = Int_val (Field (tup_v, 1));
        if (fd2 < 0) {
            if ((ret = posix_spawn_file_actions_addclose (&fa, fd1)) != 0) {
                msg = "posix_spawn_file_actions_addclose";
                earg_v = tup_v;
                goto fail;
            }
        }
        else {
            if ((ret = posix_spawn_file_actions_adddup2 (&fa, fd1, fd2)) != 0) {
                msg = "posix_spawn_file_actions_adddup2";
                earg_v = tup_v;
                goto fail;
            }
        }
    }

    if ((ret = posix_spawn (NULL, "/bin/sh", &fa, &attr, argv, environ))) {
        msg = "posix_spawn";
        goto fail;
    }

 fail:
    if ((ret = posix_spawnattr_destroy (&attr)) != 0) {
        fprintf (stderr, "posix_spawnattr_destroy: %s\n", strerror (ret));
    }

 fail1:
    if ((ret = posix_spawn_file_actions_destroy (&fa)) != 0) {
        fprintf (stderr, "posix_spawn_file_actions_destroy: %s\n",
                 strerror (ret));
    }

    if (msg)
        unix_error (ret, msg, earg_v);

    CAMLreturn (Val_unit);
}
#endif

CAMLprim value ml_copysel (value fd_v, value ptr_v)
{
    CAMLparam1 (ptr_v);
    FILE *f;
    int seen = 0;
    struct page *page;
    fz_text_line *line;
    fz_text_span *span;
    fz_text_block *block;
    int fd = Int_val (fd_v);
    char *s = String_val (ptr_v);

    if (trylock ("ml_copysel")) {
        goto done;
    }

    page = parse_pointer ("ml_sopysel", s);

    if (!page->fmark.span || !page->lmark.span) {
        fprintf (stderr, "nothing to copy\n");
        goto unlock;
    }

    f = fdopen (fd, "w");
    if (!f) {
        fprintf (stderr, "failed to fopen sel pipe: %s\n",
                 strerror (errno));
        f = stdout;
    }

    for (block = page->text->blocks;
         block < page->text->blocks + page->text->len;
         ++block) {
        for (line = block->lines;
             line < block->lines + block->len;
             ++line) {
            for (span = line->spans;
                 span < line->spans + line->len;
                 ++span) {
                int a, b;

                seen |= span == page->fmark.span || span == page->lmark.span;
                a = span == page->fmark.span ? page->fmark.i : 0;
                b = span == page->lmark.span ? page->lmark.i : span->len - 1;

                if (seen) {
                    if (pipespan (f, span, a, b))  {
                        goto close;
                    }
                    if (span == line->spans + line->len - 1)  {
                        if (putc ('\n', f) == EOF) {
                            fprintf (stderr,
                                     "failed break line on sel pipe: %s\n",
                                     strerror (errno));
                            goto close;
                        }
                    }
                    if (span == page->lmark.span) {
                        goto endloop;
                    }
                }
            }
        }
    }
 endloop:
    page->lmark.span = NULL;
    page->fmark.span = NULL;

 close:
    if (f != stdout) {
        int ret = fclose (f);
        fd = -1;
        if (ret == -1)  {
            if (errno != ECHILD) {
                fprintf (stderr, "failed to close sel pipe: %s\n",
                         strerror (errno));
            }
        }
    }
 unlock:
    unlock ("ml_copysel");

 done:
    if (fd >= 0) {
        if (close (fd)) {
            fprintf (stderr, "failed to close sel pipe: %s\n",
                     strerror (errno));
        }
    }
    CAMLreturn (Val_unit);
}

CAMLprim value ml_getpdimrect (value pagedimno_v)
{
    CAMLparam1 (pagedimno_v);
    CAMLlocal1 (ret_v);
    int pagedimno = Int_val (pagedimno_v);
    fz_rect box;

    ret_v = caml_alloc_small (4 * Double_wosize, Double_array_tag);
    if (trylock ("ml_getpdimrect")) {
        box = fz_empty_rect;
    }
    else {
        box = state.pagedims[pagedimno].mediabox;
        unlock ("ml_getpdimrect");
    }

    Store_double_field (ret_v, 0, box.x0);
    Store_double_field (ret_v, 1, box.x1);
    Store_double_field (ret_v, 2, box.y0);
    Store_double_field (ret_v, 3, box.y1);

    CAMLreturn (ret_v);
}

static double getmaxw (void)
{
    int i;
    struct pagedim *p;
    double maxw = 0.0;

    for (i = 0, p = state.pagedims; i < state.pagedimcount; ++i, ++p) {
        double x0, x1, w;

        x0 = MIN (p->mediabox.x0, p->mediabox.x1);
        x1 = MAX (p->mediabox.x0, p->mediabox.x1);

        w = x1 - x0;
        maxw = MAX (w, maxw);
    }
    return maxw;
}

CAMLprim value ml_getmaxw (value unit_v)
{
    CAMLparam1 (unit_v);
    CAMLlocal1 (ret_v);
    double maxw = 0.0;

    if (trylock ("ml_getmaxw")) {
        goto done;
    }
    maxw = getmaxw ();
    unlock ("ml_getmaxw");
 done:
    ret_v = caml_copy_double (maxw);
    CAMLreturn (ret_v);
}

CAMLprim value ml_zoom_for_height (value winw_v, value winh_v,
                                   value dw_v, value cols_v)
{
    CAMLparam3 (winw_v, winh_v, dw_v);
    CAMLlocal1 (ret_v);
    int i;
    double zoom = 1.0;
    double maxw = 0.0, maxh = 0.0;
    struct pagedim *p;
    double winw = Int_val (winw_v);
    double winh = Int_val (winh_v);
    double dw = Int_val (dw_v);
    double cols = Int_val (cols_v);
    double pw = 1.0, ph = 1.0, num, den;

    if (trylock ("ml_zoom_for_height")) {
        goto done;
    }

    if (state.proportional) {
        maxw = getmaxw () / cols;
    }

    for (i = 0, p = state.pagedims; i < state.pagedimcount; ++i, ++p) {
        double x0, x1, y0, y1, w, h, scaledh, scale;

        x0 = MIN (p->mediabox.x0, p->mediabox.x1);
        x1 = MAX (p->mediabox.x0, p->mediabox.x1);
        y0 = MIN (p->mediabox.y0, p->mediabox.y1);
        y1 = MAX (p->mediabox.y0, p->mediabox.y1);

        w = (x1 - x0) / cols;
        h = y1 - y0;

        if (state.proportional) {
            scale = w / maxw;
            scaledh = h * scale;
        }
        else  {
            scale = 1.0;
            scaledh = h;
        }

        if (scaledh > maxh) {
            maxh = scaledh;
            ph = scaledh;
            pw = w * scale;
        }
    }

    num = (winh * pw) + (ph * dw);
    den = ph * winw;
    zoom = num / den;

    unlock ("ml_zoom_for_height");
 done:
    ret_v = caml_copy_double (zoom);
    CAMLreturn (ret_v);
}

CAMLprim value ml_draw_string (value pt_v, value x_v, value y_v, value string_v)
{
    CAMLparam4 (pt_v, x_v, y_v, string_v);
    CAMLlocal1 (ret_v);
    int pt = Int_val(pt_v);
    int x = Int_val (x_v);
    int y = Int_val (y_v);
    double w;

    w = draw_string (state.face, pt, x, y, String_val (string_v));
    ret_v = caml_copy_double (w);
    CAMLreturn (ret_v);
}

CAMLprim value ml_measure_string (value pt_v, value string_v)
{
    CAMLparam2 (pt_v, string_v);
    CAMLlocal1 (ret_v);
    int pt = Int_val (pt_v);
    double w;

    w = measure_string (state.face, pt, String_val (string_v));
    ret_v = caml_copy_double (w);
    CAMLreturn (ret_v);
}

CAMLprim value ml_getpagebox (value opaque_v)
{
    CAMLparam1 (opaque_v);
    CAMLlocal1 (ret_v);
    fz_bbox bbox;
    fz_device *dev;
    char *s = String_val (opaque_v);
    struct page *page = parse_pointer ("ml_getpagebox", s);

    ret_v = caml_alloc_tuple (4);
    dev = fz_new_bbox_device (state.ctx, &bbox);
    dev->hints |= FZ_IGNORE_SHADE;

    switch (page->type) {
    case DPDF:
        pdf_run_page (state.u.pdf, page->u.pdfpage, dev, pagectm (page), NULL);
        break;

    case DXPS:
        xps_run_page (state.u.xps, page->u.xpspage, dev, pagectm (page), NULL);
        break;

    default:
        bbox = fz_infinite_bbox;
        break;
    }

    fz_free_device (dev);
    Field (ret_v, 0) = Val_int (bbox.x0);
    Field (ret_v, 1) = Val_int (bbox.y0);
    Field (ret_v, 2) = Val_int (bbox.x1);
    Field (ret_v, 3) = Val_int (bbox.y1);

    CAMLreturn (ret_v);
}

CAMLprim value ml_setaalevel (value level_v)
{
    CAMLparam1 (level_v);

    state.aalevel = Int_val (level_v);
    CAMLreturn (Val_unit);
}

#undef pixel
#include <X11/Xlib.h>
#include <GL/glx.h>

static struct {
    Display *dpy;
    GLXContext ctx;
    GLXDrawable drawable;
} glx;

#include "keysym2ucs.c"

CAMLprim value ml_keysymtoutf8 (value keysym_v)
{
    CAMLparam1 (keysym_v);
    CAMLlocal1 (str_v);
    KeySym keysym = Int_val (keysym_v);
    Rune rune;
    int len;
    char buf[5];

    rune = keysym2ucs (keysym);
    len = fz_runetochar (buf, rune);
    buf[len] = 0;
    str_v = caml_copy_string (buf);
    CAMLreturn (str_v);
}

CAMLprim value ml_glx (value win_v)
{
    CAMLparam1 (win_v);
    XVisualInfo *visual;
    int screen, wid = Int_val (win_v);
    int attributes[] = { GLX_RGBA, GLX_DOUBLEBUFFER, None };

    glx.dpy = XOpenDisplay (NULL);
    if (!glx.dpy) {
        caml_failwith ("XOpenDisplay failed");
    }

    screen = DefaultScreen (glx.dpy);
    visual = glXChooseVisual (glx.dpy, screen, attributes);
    if (!visual) {
        XCloseDisplay (glx.dpy);
        glx.dpy = NULL;
        caml_failwith ("glXChooseVisual");
    }

    glx.ctx = glXCreateContext (glx.dpy, visual, NULL, True);
    XFree (visual);
    if (!glx.ctx) {
        XCloseDisplay (glx.dpy);
        glx.dpy = NULL;
        caml_failwith ("glXCreateContext");
    }

    if (!glXMakeCurrent (glx.dpy, wid, glx.ctx)) {
        glXDestroyContext (glx.dpy, glx.ctx);
        XCloseDisplay (glx.dpy);
        glx.dpy = NULL;
        glx.ctx = NULL;
        caml_failwith ("glXMakeCurrent");
    }
    glx.drawable = wid;

    CAMLreturn (Val_unit);
}

CAMLprim value ml_swapb (value unit_v)
{
    CAMLparam1 (unit_v);
    glXSwapBuffers (glx.dpy, glx.drawable);
    CAMLreturn (Val_unit);
}

CAMLprim value ml_glxsync (value unit_v)
{
    CAMLparam1 (unit_v);
    if (glx.dpy && glx.ctx) {
        glXWaitX ();
        glXWaitGL ();
    }
    CAMLreturn (Val_unit);
}

enum { piunknown, pilinux, piosx, pisun, pifreebsd,
       pidragonflybsd, piopenbsd, pinetbsd, picygwin };

CAMLprim value ml_platform (value unit_v)
{
    CAMLparam1 (unit_v);
    int platid = piunknown;

#if defined __linux__
    platid = pilinux;
#elif defined __CYGWIN__
    platid = picygwin;
#elif defined __DragonFly__
    platid = pidragonflybsd;
#elif defined __FreeBSD__
    platid = pifreebsd;
#elif defined __OpenBSD__
    platid = piopenbsd;
#elif defined __NetBSD__
    platid = pinetbsd;
#elif defined __sun__
    platid = pisun;
#elif defined __APPLE__
    platid = piosx;
#endif
    CAMLreturn (Val_int (platid));
}

CAMLprim value ml_cloexec (value fd_v)
{
    CAMLparam1 (fd_v);
    int fd = Int_val (fd_v);

    if (fcntl (fd, F_SETFD, FD_CLOEXEC, 1)) {
        uerror ("fcntl", Nothing);
    }
    CAMLreturn (Val_unit);
}

CAMLprim value ml_init (value pipe_v, value params_v)
{
    CAMLparam2 (pipe_v, params_v);
    CAMLlocal2 (trim_v, fuzz_v);
    int ret;
    int texcount;
    char *fontpath;
    int colorspace;
    int mustoresize;
    struct sigaction sa;

    state.cr           = Int_val (Field (pipe_v, 0));
    state.cw           = Int_val (Field (pipe_v, 1));
    state.rotate       = Int_val (Field (params_v, 0));
    state.proportional = Bool_val (Field (params_v, 1));
    trim_v             = Field (params_v, 2);
    texcount           = Int_val (Field (params_v, 3));
    state.sliceheight  = Int_val (Field (params_v, 4));
    mustoresize        = Int_val (Field (params_v, 5));
    colorspace         = Int_val (Field (params_v, 6));
    fontpath           = String_val (Field (params_v, 7));

    state.ctx = fz_new_context (NULL, NULL, mustoresize);

    state.trimmargins = Bool_val (Field (trim_v, 0));
    fuzz_v            = Field (trim_v, 1);
    state.trimfuzz.x0 = Int_val (Field (fuzz_v, 0));
    state.trimfuzz.y0 = Int_val (Field (fuzz_v, 1));
    state.trimfuzz.x1 = Int_val (Field (fuzz_v, 2));
    state.trimfuzz.y1 = Int_val (Field (fuzz_v, 3));

    set_tex_params (colorspace);

    if (*fontpath) {
        state.face = load_font (fontpath);
    }
    else {
        unsigned int len;
        void *base = pdf_lookup_substitute_font (0, 0, 0, 0, &len);

        state.face = load_builtin_font (base, len);
    }
    if (!state.face) _exit (1);

    realloctexts (texcount);

#ifdef __CYGWIN__
    sa.sa_handler = SIG_IGN;
    sa.sa_flags = SA_RESTART | SA_NOCLDSTOP;
#else
    sa.sa_handler = SIG_DFL;
    sa.sa_flags = SA_RESTART | SA_NOCLDSTOP | SA_NOCLDWAIT;
#endif
    if (sigemptyset (&sa.sa_mask)) {
        err (1, "sigemptyset");
    }
    if (sigaction (SIGCHLD, &sa, NULL)) {
        err (1, "sigaction");
    }

    ret = pthread_create (&state.thread, NULL, mainloop, NULL);
    if (ret) {
        errx (1, "pthread_create: %s", strerror (ret));
    }

    CAMLreturn (Val_unit);
}

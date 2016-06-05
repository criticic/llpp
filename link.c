/* lots of code c&p-ed directly from mupdf */
#define CAML_NAME_SPACE

#include <errno.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <signal.h>
#include <wchar.h>

#include <unistd.h>
#include <pthread.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/utsname.h>

#ifdef __CYGWIN__
#include <cygwin/socket.h>      /* FIONREAD */
#else
#include <spawn.h>
#endif

#include <regex.h>
#include <stdarg.h>
#include <limits.h>
#include <inttypes.h>

#include <GL/gl.h>

#include <caml/fail.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/unixsupport.h>

#if __GNUC__ < 5 && !defined(__clang__)
/* At least gcc (Gentoo 4.9.3 p1.0, pie-0.6.2) 4.9.3 emits erroneous
   clobbered diagnostics */
#pragma GCC diagnostic ignored "-Wclobbered"
#endif

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-parameter"
#pragma GCC diagnostic ignored "-Wshadow"
#include <mupdf/fitz.h>
#pragma GCC diagnostic pop
#include <mupdf/pdf.h>

#include <ft2build.h>
#include FT_FREETYPE_H

#ifdef USE_FONTCONFIG
#include <fontconfig/fontconfig.h>
#endif

#define PIGGYBACK
#define CACHE_PAGEREFS

#ifndef __USE_GNU
extern char **environ;
#endif

#define MIN(a,b) ((a) < (b) ? (a) : (b))
#define MAX(a,b) ((a) > (b) ? (a) : (b))

#if defined __GNUC__
#define NORETURN_ATTR __attribute__ ((noreturn))
#define UNUSED_ATTR __attribute__ ((unused))
#if !defined __clang__
#define OPTIMIZE_ATTR(n) __attribute__ ((optimize ("O"#n)))
#else
#define OPTIMIZE_ATTR(n)
#endif
#define GCC_FMT_ATTR(a, b) __attribute__ ((format (printf, a, b)))
#else
#define NORETURN_ATTR
#define UNUSED_ATTR
#define OPTIMIZE_ATTR(n)
#define GCC_FMT_ATTR(a, b)
#endif

#define FMT_s "zu"

#define FMT_ptr PRIxPTR
#define SCN_ptr SCNxPTR
#define FMT_ptr_cast(p) ((uintptr_t) (p))
#define SCN_ptr_cast(p) ((uintptr_t *) (p))

static void NORETURN_ATTR GCC_FMT_ATTR (2, 3)
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

static void NORETURN_ATTR GCC_FMT_ATTR (2, 3)
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

#ifdef USE_NPOT
#define TEXT_TYPE GL_TEXTURE_2D
#else
#define TEXT_TYPE GL_TEXTURE_RECTANGLE_ARB
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
    int w, h;
    int slicecount;
    int sliceheight;
    struct bo *pbo;
    fz_pixmap *pixmap;
    struct slice slices[1];
};

struct pagedim {
    int pageno;
    int rotate;
    int left;
    int tctmready;
    fz_irect bounds;
    fz_rect pagebox;
    fz_rect mediabox;
    fz_matrix ctm, zoomctm, lctm, tctm;
};

struct slink {
    enum { SLINK, SANNOT } tag;
    fz_irect bbox;
    union {
        fz_link *link;
        fz_annot *annot;
    } u;
};

struct annot {
    fz_irect bbox;
    fz_annot *annot;
};

struct page {
    int tgen;
    int sgen;
    int agen;
    int pageno;
    int pdimno;
    fz_stext_page *text;
    fz_stext_sheet *sheet;
    fz_page *fzpage;
    fz_display_list *dlist;
    int slinkcount;
    struct slink *slinks;
    int annotcount;
    struct annot *annots;
    struct mark {
        int i;
        fz_stext_span *span;
    } fmark, lmark;
};

struct {
    int sliceheight;
    struct pagedim *pagedims;
    int pagecount;
    int pagedimcount;
    fz_document *doc;
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
    enum { FitWidth, FitProportional, FitPage } fitmodel;
    int trimmargins;
    int needoutline;
    int gen;
    int aalevel;

    int trimanew;
    fz_irect trimfuzz;
    fz_pixmap *pig;

    pthread_t thread;
    int csock;
    FT_Face face;

    char *trimcachepath;
    int cxack;
    int dirty;

    GLuint stid;

    int bo_usable;
    GLuint boid;

    void (*glBindBufferARB) (GLenum, GLuint);
    GLboolean (*glUnmapBufferARB) (GLenum);
    void *(*glMapBufferARB) (GLenum, GLenum);
    void (*glBufferDataARB) (GLenum, GLsizei, void *, GLenum);
    void (*glGenBuffersARB) (GLsizei, GLuint *);
    void (*glDeleteBuffersARB) (GLsizei, GLuint *);

    GLfloat texcoords[8];
    GLfloat vertices[16];

#ifdef CACHE_PAGEREFS
    struct {
        int idx;
        int count;
        pdf_obj **objs;
        pdf_document *pdf;
    } pdflut;
#endif
} state;

struct bo {
    GLuint id;
    void *ptr;
    size_t size;
};

static void UNUSED_ATTR debug_rect (const char *cap, fz_rect r)
{
    printf ("%s(rect) %.2f,%.2f,%.2f,%.2f\n", cap, r.x0, r.y0, r.x1, r.y1);
}

static void UNUSED_ATTR debug_bbox (const char *cap, fz_irect r)
{
    printf ("%s(bbox) %d,%d,%d,%d\n", cap, r.x0, r.y0, r.x1, r.y1);
}

static void UNUSED_ATTR debug_matrix (const char *cap, fz_matrix m)
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

    ret = sscanf (s, "%" SCN_ptr, SCN_ptr_cast (&ptr));
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
    ret = ioctl (state.csock, FIONREAD, &avail);
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

static void readdata (int fd, void *p, int size)
{
    ssize_t n;

 again:
    n = read (fd, p, size);
    if (n < 0) {
        if (errno == EINTR) goto again;
        err (1, "read (fd %d, req %d, ret %zd)", fd, size, n);
    }
    if (n - size) {
        if (!n) errx (1, "EOF while reading");
        errx (1, "read (fd %d, req %d, ret %zd)", fd, size, n);
    }
}

static void writedata (int fd, char *p, int size)
{
    ssize_t n;

    /* One should lookup type punning/strict aliasing etc in standard,DRs,Web to
       convince herself that this is:
       a. safe
       b. practically the only way to achieve the result
          (union puns notwithstanding) */
    memcpy (p, &size, 4);
    n = write (fd, p, size + 4);
    if (n - size - 4) {
        if (!n) errx (1, "EOF while writing data");
        err (1, "write (fd %d, req %d, ret %zd)", fd, size + 4, n);
    }
}

static int readlen (int fd)
{
    /* Type punned unions here. Why? Less code (Adjusted by more comments).
       https://en.wikipedia.org/wiki/Type_punning */
    union { int len; char raw[4]; } buf;
    readdata (fd, buf.raw, 4);
    return buf.len;
}

CAMLprim void ml_wcmd (value fd_v, value bytes_v, value len_v)
{
    CAMLparam3 (fd_v, bytes_v, len_v);
    writedata (Int_val (fd_v), &Byte (bytes_v, 0), Int_val (len_v));
    CAMLreturn0;
}

CAMLprim value ml_rcmd (value fd_v)
{
    CAMLparam1 (fd_v);
    CAMLlocal1 (strdata_v);
    int fd = Int_val (fd_v);
    int len = readlen (fd);
    strdata_v = caml_alloc_string (len);
    readdata (fd, String_val (strdata_v), len);
    CAMLreturn (strdata_v);
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
        len = vsnprintf (buf + 4, size - 4, fmt, ap);
        va_end (ap);

        if (len > -1) {
            if (len < size - 4) {
                writedata (state.csock, buf, len);
                break;
            }
            else size = len + 5;
        }
        else {
            err (1, "vsnprintf for `%s' failed", fmt);
        }
        buf = realloc (buf, size);
    }
    free (buf);
}

static void closedoc (void)
{
#ifdef CACHE_PAGEREFS
    if (state.pdflut.objs) {
        int i;

        for (i = 0; i < state.pdflut.count; ++i) {
            pdf_drop_obj (state.ctx, state.pdflut.objs[i]);
        }
        free (state.pdflut.objs);
        state.pdflut.objs = NULL;
        state.pdflut.idx = 0;
    }
#endif
    if (state.doc) {
        fz_drop_document (state.ctx, state.doc);
        state.doc = NULL;
    }
}

static int openxref (char *filename, char *password)
{
    int i;

    for (i = 0; i < state.texcount; ++i) {
        state.texowners[i].w = -1;
        state.texowners[i].slice = NULL;
    }

    closedoc ();

    state.dirty = 0;
    if (state.pagedims) {
        free (state.pagedims);
        state.pagedims = NULL;
    }
    state.pagedimcount = 0;

    fz_set_aa_level (state.ctx, state.aalevel);
#ifdef CSS_HACK_TO_READ_EPUBS_COMFORTABLY
    fz_set_user_css (state.ctx,
                     "body { margin-left: 20%; margin-right: 20%; }");
#endif
    state.doc = fz_open_document (state.ctx, filename);
    if (fz_needs_password (state.ctx, state.doc)) {
        if (password && !*password) {
            printd ("pass");
            return 0;
        }
        else {
            int ok = fz_authenticate_password (state.ctx, state.doc, password);
            if (!ok) {
                printd ("pass fail");
                return 0;
            }
        }
    }
    state.pagecount = fz_count_pages (state.ctx, state.doc);
    return 1;
}

static void pdfinfo (void)
{
    pdf_document *pdf = pdf_specifics (state.ctx, state.doc);
    if (pdf) {
        pdf_obj *infoobj;

        printd ("info PDF version\t%d.%d",
                pdf->version / 10, pdf->version % 10);

        infoobj = pdf_dict_gets (state.ctx, pdf_trailer (state.ctx,
                                                         pdf), "Info");
        if (infoobj) {
            unsigned int i;
            char *s;
            char *items[] = { "Title", "Author", "Creator",
                              "Producer", "CreationDate" };

            for (i = 0; i < sizeof (items) / sizeof (*items); ++i) {
                pdf_obj *obj = pdf_dict_gets (state.ctx, infoobj, items[i]);
                s = pdf_to_utf8 (state.ctx, pdf, obj);
                if (*s) printd ("info %s\t%s", items[i], s);
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
    if (!page) return;
    if (page->text) {
        fz_drop_stext_page (state.ctx, page->text);
    }
    if (page->sheet) {
        fz_drop_stext_sheet (state.ctx, page->sheet);
    }
    if (page->slinks) {
        free (page->slinks);
    }
    fz_drop_display_list (state.ctx, page->dlist);
    fz_drop_page (state.ctx, page->fzpage);
    free (page);
}

static void freetile (struct tile *tile)
{
    unlinktile (tile);
    if (!tile->pbo) {
#ifndef PIGGYBACK
        fz_drop_pixmap (state.ctx, tile->pixmap);
#else
        if (state.pig) {
            fz_drop_pixmap (state.ctx, state.pig);
        }
        state.pig = tile->pixmap;
#endif
    }
    else {
        free (tile->pbo);
        fz_drop_pixmap (state.ctx, tile->pixmap);
    }
    free (tile);
}

#ifdef __ALTIVEC__
#include <stdint.h>
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

static void OPTIMIZE_ATTR (3) clearpixmap (fz_pixmap *pixmap)
{
    size_t size = pixmap->w * pixmap->h * pixmap->n;
    if (cacheline32bytes && size > 32) {
        intptr_t a1, a2, diff;
        size_t sizea, i;
        vector unsigned char v = vec_splat_u8 (-1);
        vector unsigned char *p;

        a1 = a2 = (intptr_t) pixmap->samples;
        a2 = (a1 + 31) & ~31;
        diff = a2 - a1;
        sizea = size - diff;
        p = (void *) a2;

        while (a1 != a2) *(char *) a1++ = 0xff;
        for (i = 0; i < (sizea & ~31); i += 32) {
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

static void trimctm (pdf_page *page, int pindex)
{
    fz_matrix ctm;
    struct pagedim *pdim = &state.pagedims[pindex];

    if (!pdim->tctmready) {
        if (state.trimmargins) {
            fz_rect realbox;
            fz_matrix rm, sm, tm, im, ctm1;

            fz_rotate (&rm, -pdim->rotate);
            fz_scale (&sm, 1, -1);
            fz_concat (&ctm, &rm, &sm);
            realbox = pdim->mediabox;
            fz_transform_rect (&realbox, &ctm);
            fz_translate (&tm, -realbox.x0, -realbox.y0);
            fz_concat (&ctm1, &ctm, &tm);
            fz_invert_matrix (&im, &page->ctm);
            fz_concat (&ctm, &im, &ctm1);
        }
        else {
            ctm = fz_identity;
        }
        pdim->tctm = ctm;
        pdim->tctmready = 1;
    }
}

static fz_matrix pagectm1 (fz_page *fzpage, struct pagedim *pdim)
{
    fz_matrix ctm, tm;
    int pdimno = pdim - state.pagedims;

    if (pdf_specifics (state.ctx, state.doc)) {
        trimctm ((pdf_page *) fzpage, pdimno);
        fz_concat (&ctm, &pdim->tctm, &pdim->ctm);
    }
    else {
        fz_translate (&tm, -pdim->mediabox.x0, -pdim->mediabox.y0);
        fz_concat (&ctm, &tm, &pdim->ctm);
    }
    return ctm;
}

static fz_matrix pagectm (struct page *page)
{
    return pagectm1 (page->fzpage, &state.pagedims[page->pdimno]);
}

static void *loadpage (int pageno, int pindex)
{
    fz_device *dev;
    struct page *page;

    page = calloc (sizeof (struct page), 1);
    if (!page) {
        err (1, "calloc page %d", pageno);
    }

    page->dlist = fz_new_display_list (state.ctx);
    dev = fz_new_list_device (state.ctx, page->dlist);
    fz_try (state.ctx) {
        page->fzpage = fz_load_page (state.ctx, state.doc, pageno);
        fz_run_page (state.ctx, page->fzpage, dev,
                     &fz_identity, NULL);
    }
    fz_catch (state.ctx) {
        page->fzpage = NULL;
    }
    fz_drop_device (state.ctx, dev);

    page->pdimno = pindex;
    page->pageno = pageno;
    page->sgen = state.gen;
    page->agen = state.gen;
    page->tgen = state.gen;
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
        err (1, "cannot allocate tile (%" FMT_s " bytes)", tilesize);
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

static struct tile *rendertile (struct page *page, int x, int y, int w, int h,
                                struct bo *pbo)
{
    fz_rect rect;
    fz_irect bbox;
    fz_matrix ctm;
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
        if (pbo) {
            tile->pixmap =
                fz_new_pixmap_with_bbox_and_data (state.ctx, state.colorspace,
                                                  &bbox, 1, pbo->ptr);
            tile->pbo = pbo;
        }
        else {
            tile->pixmap =
                fz_new_pixmap_with_bbox (state.ctx, state.colorspace, &bbox, 1);
        }
    }

    tile->w = w;
    tile->h = h;
    clearpixmap (tile->pixmap);

    dev = fz_new_draw_device (state.ctx, tile->pixmap);
    ctm = pagectm (page);
    fz_rect_from_irect (&rect, &bbox);
    fz_run_display_list (state.ctx, page->dlist, dev, &ctm, &rect, NULL);
    fz_drop_device (state.ctx, dev);

    return tile;
}

#ifdef CACHE_PAGEREFS
/* modified mupdf/source/pdf/pdf-page.c:pdf_lookup_page_loc_imp
   thanks to Robin Watts */
static void
pdf_collect_pages(pdf_document *doc, pdf_obj *node)
{
    fz_context *ctx = state.ctx; /* doc->ctx; */
    pdf_obj *kids;
    int i, len;

    if (state.pdflut.idx == state.pagecount) return;

    kids = pdf_dict_gets (ctx, node, "Kids");
    len = pdf_array_len (ctx, kids);

    if (len == 0)
        fz_throw (ctx, FZ_ERROR_GENERIC, "malformed pages tree");

    if (pdf_mark_obj (ctx, node))
        fz_throw (ctx, FZ_ERROR_GENERIC, "cycle in page tree");
    for (i = 0; i < len; i++) {
        pdf_obj *kid = pdf_array_get (ctx, kids, i);
        char *type = pdf_to_name (ctx, pdf_dict_gets (ctx, kid, "Type"));
        if (*type
            ? !strcmp (type, "Pages")
            : pdf_dict_gets (ctx, kid, "Kids")
            && !pdf_dict_gets (ctx, kid, "MediaBox")) {
            pdf_collect_pages (doc, kid);
        }
        else {
            if (*type
                ? strcmp (type, "Page") != 0
                : !pdf_dict_gets (ctx, kid, "MediaBox"))
                fz_warn (ctx, "non-page object in page tree (%s)", type);
            state.pdflut.objs[state.pdflut.idx++] = pdf_keep_obj (ctx, kid);
        }
    }
    pdf_unmark_obj (ctx, node);
}

static void
pdf_load_page_objs (pdf_document *doc)
{
    pdf_obj *root = pdf_dict_gets (state.ctx,
                                   pdf_trailer (state.ctx, doc), "Root");
    pdf_obj *node = pdf_dict_gets (state.ctx, root, "Pages");

    if (!node)
        fz_throw (state.ctx, FZ_ERROR_GENERIC, "cannot find page tree");

    state.pdflut.idx = 0;
    pdf_collect_pages (doc, node);
}
#endif

static void initpdims (int wthack)
{
    double start, end;
    FILE *trimf = NULL;
    fz_rect rootmediabox;
    int pageno, trim, show;
    int trimw = 0, cxcount;
    fz_context *ctx = state.ctx;
    pdf_document *pdf = pdf_specifics (ctx, state.doc);

    fz_var (trimw);
    fz_var (trimf);
    fz_var (cxcount);
    start = now ();

    if (state.trimmargins && state.trimcachepath) {
        trimf = fopen (state.trimcachepath, "rb");
        if (!trimf) {
            trimf = fopen (state.trimcachepath, "wb");
            trimw = 1;
        }
    }

    if (state.trimmargins || pdf || !state.cxack)
        cxcount = state.pagecount;
    else
        cxcount = MIN (state.pagecount, 1);

    if (pdf) {
        pdf_obj *obj;
        obj = pdf_dict_getp (ctx, pdf_trailer (ctx, pdf),
                             "Root/Pages/MediaBox");
        pdf_to_rect (ctx, obj, &rootmediabox);
    }

#ifdef CACHE_PAGEREFS
    if (pdf && (!state.pdflut.objs || state.pdflut.pdf != pdf)) {
        state.pdflut.objs = calloc (sizeof (*state.pdflut.objs), cxcount);
        if (!state.pdflut.objs) {
            err (1, "malloc pageobjs %zu %d %zu failed",
                 sizeof (*state.pdflut.objs), cxcount,
                 sizeof (*state.pdflut.objs) * cxcount);
        }
        state.pdflut.count = cxcount;
        pdf_load_page_objs (pdf);
        state.pdflut.pdf = pdf;
    }
#endif

    for (pageno = 0; pageno < cxcount; ++pageno) {
        int rotate = 0;
        struct pagedim *p;
        fz_rect mediabox;

        fz_var (rotate);
        if (pdf) {
            pdf_obj *pageref, *pageobj;

#ifdef CACHE_PAGEREFS
            pageref = state.pdflut.objs[pageno];
#else
            pageref = pdf_lookup_page_obj (ctx, pdf, pageno);
#endif
            pageobj = pdf_resolve_indirect (ctx, pageref);

            if (state.trimmargins) {
                pdf_obj *obj;
                pdf_page *page;

                fz_try (ctx) {
                    page = pdf_load_page (ctx, pdf, pageno);
                    obj = pdf_dict_gets (ctx, pageobj, "llpp.TrimBox");
                    trim = state.trimanew || !obj;
                    if (trim) {
                        fz_rect rect;
                        fz_matrix ctm;
                        fz_device *dev;

                        dev = fz_new_bbox_device (ctx, &rect);
                        dev->hints |= FZ_IGNORE_SHADE;
                        fz_invert_matrix (&ctm, &page->ctm);
                        pdf_run_page (ctx, page, dev, &fz_identity, NULL);
                        fz_drop_device (ctx, dev);

                        rect.x0 += state.trimfuzz.x0;
                        rect.x1 += state.trimfuzz.x1;
                        rect.y0 += state.trimfuzz.y0;
                        rect.y1 += state.trimfuzz.y1;
                        fz_transform_rect (&rect, &ctm);
                        fz_intersect_rect (&rect, &page->mediabox);

                        if (fz_is_empty_rect (&rect)) {
                            mediabox = page->mediabox;
                        }
                        else {
                            mediabox = rect;
                        }

                        obj = pdf_new_array (ctx, pdf, 4);
                        pdf_array_push (ctx, obj, pdf_new_real (ctx, pdf,
                                                                mediabox.x0));
                        pdf_array_push (ctx, obj, pdf_new_real (ctx, pdf,
                                                                mediabox.y0));
                        pdf_array_push (ctx, obj, pdf_new_real (ctx, pdf,
                                                                mediabox.x1));
                        pdf_array_push (ctx, obj, pdf_new_real (ctx, pdf,
                                                                mediabox.y1));
                        pdf_dict_puts (ctx, pageobj, "llpp.TrimBox", obj);
                    }
                    else {
                        mediabox.x0 = pdf_to_real (ctx,
                                                   pdf_array_get (ctx, obj, 0));
                        mediabox.y0 = pdf_to_real (ctx,
                                                   pdf_array_get (ctx, obj, 1));
                        mediabox.x1 = pdf_to_real (ctx,
                                                   pdf_array_get (ctx, obj, 2));
                        mediabox.y1 = pdf_to_real (ctx,
                                                   pdf_array_get (ctx, obj, 3));
                    }

                    rotate = page->rotate;
                    fz_drop_page (ctx, &page->super);

                    show = trim ? pageno % 5 == 0 : pageno % 20 == 0;
                    if (show) {
                        printd ("progress %f Trimming %d",
                                (double) (pageno + 1) / state.pagecount,
                                pageno + 1);
                    }
                }
                fz_catch (ctx) {
                    fprintf (stderr, "failed to load page %d\n", pageno+1);
                }
            }
            else {
                int empty = 0;
                fz_rect cropbox;

                pdf_to_rect (ctx,
                             pdf_dict_gets (ctx, pageobj, "MediaBox"),
                             &mediabox);
                if (fz_is_empty_rect (&mediabox)) {
                    mediabox.x0 = 0;
                    mediabox.y0 = 0;
                    mediabox.x1 = 612;
                    mediabox.y1 = 792;
                    empty = 1;
                }

                pdf_to_rect (ctx,
                             pdf_dict_gets (ctx, pageobj, "CropBox"),
                             &cropbox);
                if (!fz_is_empty_rect (&cropbox)) {
                    if (empty) {
                        mediabox = cropbox;
                    }
                    else {
                        fz_intersect_rect (&mediabox, &cropbox);
                    }
                }
                else {
                    if (empty) {
                        if (fz_is_empty_rect (&rootmediabox)) {
                            fprintf (stderr,
                                     "cannot find page size for page %d\n",
                                     pageno+1);
                        }
                        else {
                            mediabox = rootmediabox;
                        }
                    }
                }
                rotate = pdf_to_int (ctx,
                                     pdf_dict_gets (ctx, pageobj, "Rotate"));
            }
        }
        else {
            if (state.trimmargins && trimw) {
                fz_page *page;

                fz_try (ctx) {
                    page = fz_load_page (ctx, state.doc, pageno);
                    fz_bound_page (ctx, page, &mediabox);
                    rotate = 0;
                    if (state.trimmargins) {
                        fz_rect rect;
                        fz_device *dev;

                        dev = fz_new_bbox_device (ctx, &rect);
                        dev->hints |= FZ_IGNORE_SHADE;
                        fz_run_page (ctx, page, dev, &fz_identity, NULL);
                        fz_drop_device (ctx, dev);

                        rect.x0 += state.trimfuzz.x0;
                        rect.x1 += state.trimfuzz.x1;
                        rect.y0 += state.trimfuzz.y0;
                        rect.y1 += state.trimfuzz.y1;
                        fz_intersect_rect (&rect, &mediabox);

                        if (!fz_is_empty_rect (&rect)) {
                            mediabox = rect;
                        }
                    }
                    fz_drop_page (ctx, page);
                    if (!state.cxack) {
                        printd ("progress %f loading %d",
                                (double) (pageno + 1) / state.pagecount,
                                pageno + 1);
                    }
                }
                fz_catch (ctx) {
                }
                if (trimf) {
                    int n = fwrite (&mediabox, sizeof (mediabox), 1, trimf);
                    if (n - 1) {
                        err (1, "fwrite trim mediabox");
                    }
                }
            }
            else {
                if (trimf) {
                    int n = fread (&mediabox, sizeof (mediabox), 1, trimf);
                    if (n - 1) {
                        err (1, "fread trim mediabox %d", pageno);
                    }
                }
                else {
                    fz_page *page;
                    fz_try (ctx) {
                        page = fz_load_page (ctx, state.doc, pageno);
                        fz_bound_page (ctx, page, &mediabox);
                        fz_drop_page (ctx, page);

                        show = !state.trimmargins && pageno % 20 == 0;
                        if (show) {
                            printd ("progress %f Gathering dimensions %d",
                                    (double) (pageno) / state.pagecount,
                                    pageno);
                        }
                    }
                    fz_catch (ctx) {
                        fprintf (stderr, "failed to load page %d\n", pageno);
                    }
                }
            }
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
    if (!wthack) {
        printd ("progress 1 %s %d pages in %f seconds",
                state.trimmargins ? "Trimmed" : "Processed",
                state.pagecount, end - start);
    }
    state.trimanew = 0;
    if (trimf) {
        if (fclose (trimf)) {
            err (1, "fclose");
        }
    }
}

static void layout (void)
{
    int pindex;
    fz_rect box;
    fz_matrix ctm, rm;
    struct pagedim *p = p;
    double zw, w, maxw = 0.0, zoom = zoom;

    if (state.pagedimcount == 0) return;

    switch (state.fitmodel) {
    case FitProportional:
        for (pindex = 0; pindex < state.pagedimcount; ++pindex) {
            double x0, x1;

            p = &state.pagedims[pindex];
            fz_rotate (&rm, p->rotate + state.rotate);
            box = p->mediabox;
            fz_transform_rect (&box, &rm);

            x0 = MIN (box.x0, box.x1);
            x1 = MAX (box.x0, box.x1);

            w = x1 - x0;
            maxw = MAX (w, maxw);
            zoom = state.w / maxw;
        }
        break;

    case FitPage:
        maxw = state.w;
        break;

    case FitWidth:
        break;

    default:
        ARSERT (0 && state.fitmodel);
    }

    for (pindex = 0; pindex < state.pagedimcount; ++pindex) {
        fz_rect rect;
        fz_matrix tm, sm;

        p = &state.pagedims[pindex];
        fz_rotate (&ctm, state.rotate);
        fz_rotate (&rm, p->rotate + state.rotate);
        box = p->mediabox;
        fz_transform_rect (&box, &rm);
        w = box.x1 - box.x0;
        switch (state.fitmodel) {
        case FitProportional:
            p->left = ((maxw - w) * zoom) / 2.0;
            break;
        case FitPage:
            {
                double zh, h;
                zw = maxw / w;
                h = box.y1 - box.y0;
                zh = state.h / h;
                zoom = MIN (zw, zh);
                p->left = (maxw - (w * zoom)) / 2.0;
            }
            break;
        case FitWidth:
            p->left = 0;
            zoom = state.w / w;
            break;
        }

        fz_scale (&p->zoomctm, zoom, zoom);
        fz_concat (&ctm, &p->zoomctm, &ctm);

        fz_rotate (&rm, p->rotate);
        p->pagebox = p->mediabox;
        fz_transform_rect (&p->pagebox, &rm);
        p->pagebox.x1 -= p->pagebox.x0;
        p->pagebox.y1 -= p->pagebox.y0;
        p->pagebox.x0 = 0;
        p->pagebox.y0 = 0;
        rect = p->pagebox;
        fz_transform_rect (&rect, &ctm);
        fz_round_rect (&p->bounds, &rect);
        p->ctm = ctm;

        fz_translate (&tm, 0, -p->mediabox.y1);
        fz_scale (&sm, zoom, -zoom);
        fz_concat (&ctm, &tm, &sm);
        fz_concat (&p->lctm, &ctm, &rm);

        p->tctmready = 0;
    }

    do {
        int x0 = MIN (p->bounds.x0, p->bounds.x1);
        int y0 = MIN (p->bounds.y0, p->bounds.y1);
        int x1 = MAX (p->bounds.x0, p->bounds.x1);
        int y1 = MAX (p->bounds.y0, p->bounds.y1);
        int boundw = x1 - x0;
        int boundh = y1 - y0;

        printd ("pdim %d %d %d %d", p->pageno, boundw, boundh, p->left);
    } while (p-- != state.pagedims);
}

static
struct anchor { int n; int x; int y; int w; int h; }
desttoanchor (fz_link_dest *dest)
{
    int i;
    struct anchor a;
    struct pagedim *pdim = state.pagedims;

    a.n = -1;
    a.x = 0;
    a.y = 0;
    for (i = 0; i < state.pagedimcount; ++i) {
        if (state.pagedims[i].pageno > dest->ld.gotor.page)
            break;
        pdim = &state.pagedims[i];
    }
    if (dest->ld.gotor.flags & fz_link_flag_t_valid) {
        fz_point p;
        if (dest->ld.gotor.flags & fz_link_flag_l_valid)
            p.x = dest->ld.gotor.lt.x;
        else
            p.x = 0.0;
        p.y = dest->ld.gotor.lt.y;
        fz_transform_point (&p, &pdim->lctm);
        a.x = p.x;
        a.y = p.y;
    }
    if (dest->ld.gotor.page >= 0 && dest->ld.gotor.page < 1<<30) {
        double x0, x1, y0, y1;

        x0 = MIN (pdim->bounds.x0, pdim->bounds.x1);
        x1 = MAX (pdim->bounds.x0, pdim->bounds.x1);
        a.w = x1 - x0;
        y0 = MIN (pdim->bounds.y0, pdim->bounds.y1);
        y1 = MAX (pdim->bounds.y0, pdim->bounds.y1);
        a.h = y1 - y0;
        a.n = dest->ld.gotor.page;
    }
    return a;
}

static void recurse_outline (fz_outline *outline, int level)
{
    while (outline) {
        switch (outline->dest.kind) {
        case FZ_LINK_GOTO:
            {
                struct anchor a = desttoanchor (&outline->dest);

                if (a.n >= 0) {
                    printd ("o %d %d %d %d %s",
                            level, a.n, a.y, a.h, outline->title);
                }
            }
            break;

        case FZ_LINK_URI:
            printd ("ou %d %" FMT_s " %s %s", level,
                    strlen (outline->title), outline->title,
                    outline->dest.ld.uri.uri);
            break;

        case FZ_LINK_NONE:
            printd ("on %d %s", level, outline->title);
            break;

        default:
            printd ("emsg Unhandled outline kind %d for %s\n",
                    outline->dest.kind, outline->title);
            break;
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

    if (!state.needoutline || !state.pagedimcount) return;

    state.needoutline = 0;
    outline = fz_load_outline (state.ctx, state.doc);
    if (outline) {
        recurse_outline (outline, 0);
        fz_drop_outline (state.ctx, outline);
    }
}

static char *strofspan (fz_stext_span *span)
{
    char *p;
    char utf8[10];
    fz_stext_char *ch;
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

static int matchspan (regex_t *re, fz_stext_span *span,
                      int stop, int pageno, double start)
{
    int ret;
    char *p;
    regmatch_t rm;
    int a, b, c;
    fz_rect sb, eb;
    fz_point p1, p2, p3, p4;

    p = strofspan (span);
    if (!p) return -1;

    ret = regexec (re, p, 1, &rm, 0);
    if (ret) {
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
    else {
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

        fz_stext_char_bbox (state.ctx, &sb, span, a);
        fz_stext_char_bbox (state.ctx, &eb, span, b);

        p1.x = sb.x0;
        p1.y = sb.y0;
        p2.x = eb.x1;
        p2.y = sb.y0;
        p3.x = eb.x1;
        p3.y = eb.y1;
        p4.x = sb.x0;
        p4.y = eb.y1;

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
        else {
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
    fz_stext_block const *ls = l;
    fz_stext_block const *rs = r;
    return ls->bbox.y0 - rs->bbox.y0;
}

/* wishful thinking function */
static void search (regex_t *re, int pageno, int y, int forward)
{
    int i, j;
    fz_device *tdev;
    fz_stext_page *text;
    fz_stext_sheet *sheet;
    struct pagedim *pdim, *pdimprev;
    int stop = 0, niters = 0;
    double start, end;
    fz_page *page;

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
            if (pdim->pageno == pageno) {
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

        sheet = fz_new_stext_sheet (state.ctx);
        text = fz_new_stext_page (state.ctx);
        tdev = fz_new_stext_device (state.ctx, sheet, text);

        page = fz_load_page (state.ctx, state.doc, pageno);
        {
            fz_matrix ctm = pagectm1 (page, pdim);
            fz_run_page (state.ctx, page, tdev, &ctm, NULL);
        }

        qsort (text->blocks, text->len, sizeof (*text->blocks), compareblocks);
        fz_drop_device (state.ctx, tdev);

        for (j = 0; j < text->len; ++j) {
            int k;
            fz_page_block *pb;
            fz_stext_block *block;

            pb = &text->blocks[forward ? j : text->len - 1 - j];
            if (pb->type != FZ_PAGE_BLOCK_TEXT) continue;
            block = pb->u.text;

            for (k = 0; k < block->len; ++k) {
                fz_stext_line *line;
                fz_stext_span *span;

                if (forward) {
                    line = &block->lines[k];
                    if (line->bbox.y0 < y + 1) continue;
                }
                else {
                    line = &block->lines[block->len - 1 - k];
                    if (line->bbox.y0 > y - 1) continue;
                }

                for (span = line->first_span; span; span = span->next) {
                    switch (matchspan (re, span, stop, pageno, start)) {
                    case 0: break;
                    case 1: stop = 1; break;
                    case -1: stop = 1; goto endloop;
                    }
                }
            }
        }
        if (forward) {
            pageno += 1;
            y = 0;
        }
        else {
            pageno -= 1;
            y = INT_MAX;
        }
    endloop:
        fz_drop_stext_page (state.ctx, text);
        fz_drop_stext_sheet (state.ctx, sheet);
        fz_drop_page (state.ctx, page);
    }
    end = now ();
    if (!stop) {
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
        state.colorspace = fz_device_rgb (state.ctx);
        break;
    case 1:
        state.texiform = GL_RGBA8;
        state.texform = GL_BGRA;
        state.texty = endianness.s > 1
            ? GL_UNSIGNED_INT_8_8_8_8
            : GL_UNSIGNED_INT_8_8_8_8_REV;
        state.colorspace = fz_device_bgr (state.ctx);
        break;
    case 2:
        state.texiform = GL_LUMINANCE_ALPHA;
        state.texform = GL_LUMINANCE_ALPHA;
        state.texty = GL_UNSIGNED_BYTE;
        state.colorspace = fz_device_gray (state.ctx);
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

static char *mbtoutf8 (char *s)
{
    char *p, *r;
    wchar_t *tmp;
    size_t i, ret, len;

    len = mbstowcs (NULL, s, strlen (s));
    if (len == 0) {
        return s;
    }
    else {
        if (len == (size_t) -1) {
            return s;
        }
    }

    tmp = malloc (len * sizeof (wchar_t));
    if (!tmp) {
        return s;
    }

    ret = mbstowcs (tmp, s, len);
    if (ret == (size_t) -1) {
        free (tmp);
        return s;
    }

    len = 0;
    for (i = 0; i < ret; ++i) {
        len += fz_runelen (tmp[i]);
    }

    p = r = malloc (len + 1);
    if (!r) {
        free (tmp);
        return s;
    }

    for (i = 0; i < ret; ++i) {
        p += fz_runetochar (p, tmp[i]);
    }
    *p = 0;
    free (tmp);
    return r;
}

CAMLprim value ml_mbtoutf8 (value s_v)
{
    CAMLparam1 (s_v);
    CAMLlocal1 (ret_v);
    char *s, *r;

    s = String_val (s_v);
    r = mbtoutf8 (s);
    if (r == s) {
        ret_v = s_v;
    }
    else {
        ret_v = caml_copy_string (r);
        free (r);
    }
    CAMLreturn (ret_v);
}

static void * mainloop (void UNUSED_ATTR *unused)
{
    char *p = NULL;
    int len, ret, oldlen = 0;

    fz_var (p);
    fz_var (oldlen);
    for (;;) {
        len = readlen (state.csock);
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
        readdata (state.csock, p, len);
        p[len] = 0;

        if (!strncmp ("open", p, 4)) {
            int wthack, off, ok = 0;
            char *password;
            char *filename;
            char *utf8filename;
            size_t filenamelen;

            fz_var (ok);
            ret = sscanf (p + 5, " %d %d %n", &wthack, &state.cxack, &off);
            if (ret != 2) {
                errx (1, "malformed open `%.*s' ret=%d", len, p, ret);
            }

            filename = p + 5 + off;
            filenamelen = strlen (filename);
            password = filename + filenamelen + 1;

            lock ("open");
            fz_try (state.ctx) {
                ok = openxref (filename, password);
            }
            fz_catch (state.ctx) {
                utf8filename = mbtoutf8 (filename);
                printd ("msg Could not open %s", utf8filename);
            }
            if (ok) {
                pdfinfo ();
                initpdims (wthack);
            }
            unlock ("open");

            if (ok) {
                if (!wthack) {
                    utf8filename = mbtoutf8 (filename);
                    printd ("msg Opened %s (press h/F1 to get help)",
                            utf8filename);
                    if (utf8filename != filename) {
                        free (utf8filename);
                    }
                }
                state.needoutline = 1;
            }
        }
        else if (!strncmp ("cs", p, 2)) {
            int i, colorspace;

            ret = sscanf (p + 2, " %d", &colorspace);
            if (ret != 1) {
                errx (1, "malformed cs `%.*s' ret=%d", len, p, ret);
            }
            lock ("cs");
            set_tex_params (colorspace);
            for (i = 0; i < state.texcount; ++i) {
                state.texowners[i].w = -1;
                state.texowners[i].slice = NULL;
            }
            unlock ("cs");
        }
        else if (!strncmp ("freepage", p, 8)) {
            void *ptr;

            ret = sscanf (p + 8, " %" SCN_ptr, SCN_ptr_cast (&ptr));
            if (ret != 1) {
                errx (1, "malformed freepage `%.*s' ret=%d", len, p, ret);
            }
            freepage (ptr);
        }
        else if (!strncmp ("freetile", p, 8)) {
            void *ptr;

            ret = sscanf (p + 8, " %" SCN_ptr, SCN_ptr_cast (&ptr));
            if (ret != 1) {
                errx (1, "malformed freetile `%.*s' ret=%d", len, p, ret);
            }
            freetile (ptr);
        }
        else if (!strncmp ("search", p, 6)) {
            int icase, pageno, y, len2, forward;
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
            else {
                search (&re, pageno, y, forward);
                regfree (&re);
            }
        }
        else if (!strncmp ("geometry", p, 8)) {
            int w, h, fitmodel;

            printd ("clear");
            ret = sscanf (p + 8, " %d %d %d", &w, &h, &fitmodel);
            if (ret != 3) {
                errx (1, "malformed geometry `%.*s' ret=%d", len, p, ret);
            }

            lock ("geometry");
            state.h = h;
            if (w != state.w) {
                int i;
                state.w = w;
                for (i = 0; i < state.texcount; ++i) {
                    state.texowners[i].slice = NULL;
                }
            }
            state.fitmodel = fitmodel;
            layout ();
            process_outline ();

            state.gen++;
            unlock ("geometry");
            printd ("continue %d", state.pagecount);
        }
        else if (!strncmp ("reqlayout", p, 9)) {
            char *nameddest;
            int rotate, off, h;
            unsigned int fitmodel;
            pdf_document *pdf = pdf_specifics (state.ctx, state.doc);

            printd ("clear");
            ret = sscanf (p + 9, " %d %u %d %n",
                          &rotate, &fitmodel, &h, &off);
            if (ret != 3) {
                errx (1, "bad reqlayout line `%.*s' ret=%d", len, p, ret);
            }
            lock ("reqlayout");
            if (state.rotate != rotate || state.fitmodel != fitmodel) {
                state.gen += 1;
            }
            state.rotate = rotate;
            state.fitmodel = fitmodel;
            state.h = h;
            layout ();
            process_outline ();

            nameddest = p + 9 + off;
            if (pdf && nameddest && *nameddest) {
                struct anchor a;
                fz_link_dest dest;
                pdf_obj *needle, *obj;

                needle = pdf_new_string (state.ctx, pdf, nameddest,
                                         strlen (nameddest));
                obj = pdf_lookup_dest (state.ctx, pdf, needle);
                if (obj) {
                    dest = pdf_parse_link_dest (state.ctx, pdf,
                                                FZ_LINK_GOTO, obj);

                    a = desttoanchor (&dest);
                    if (a.n >= 0) {
                        printd ("a %d %d %d", a.n, a.x, a.y);
                    }
                    else {
                        printd ("emsg failed to parse destination `%s'\n",
                                nameddest);
                    }
                }
                else {
                    printd ("emsg destination `%s' not found\n",
                            nameddest);
                }
                pdf_drop_obj (state.ctx, needle);
            }

            state.gen++;
            unlock ("reqlayout");
            printd ("continue %d", state.pagecount);
        }
        else if (!strncmp ("page", p, 4)) {
            double a, b;
            struct page *page;
            int pageno, pindex;

            ret = sscanf (p + 4, " %d %d", &pageno, &pindex);
            if (ret != 2) {
                errx (1, "bad page line `%.*s' ret=%d", len, p, ret);
            }

            lock ("page");
            a = now ();
            page = loadpage (pageno, pindex);
            b = now ();
            unlock ("page");

            printd ("page %" FMT_ptr " %f", FMT_ptr_cast (page), b - a);
        }
        else if (!strncmp ("tile", p, 4)) {
            int x, y, w, h;
            struct page *page;
            struct tile *tile;
            double a, b;
            void *data;

            ret = sscanf (p + 4, " %" SCN_ptr " %d %d %d %d %" SCN_ptr,
                          SCN_ptr_cast (&page), &x, &y, &w, &h,
                          SCN_ptr_cast (&data));
            if (ret != 6) {
                errx (1, "bad tile line `%.*s' ret=%d", len, p, ret);
            }

            lock ("tile");
            a = now ();
            tile = rendertile (page, x, y, w, h, data);
            b = now ();
            unlock ("tile");

            printd ("tile %d %d %" FMT_ptr " %u %f",
                    x, y,
                    FMT_ptr_cast (tile),
                    tile->w * tile->h * tile->pixmap->n,
                    b - a);
        }
        else if (!strncmp ("trimset", p, 7)) {
            fz_irect fuzz;
            int trimmargins;

            ret = sscanf (p + 7, " %d %d %d %d %d",
                          &trimmargins, &fuzz.x0, &fuzz.y0, &fuzz.x1, &fuzz.y1);
            if (ret != 5) {
                errx (1, "malformed trimset `%.*s' ret=%d", len, p, ret);
            }
            lock ("trimset");
            state.trimmargins = trimmargins;
            if (memcmp (&fuzz, &state.trimfuzz, sizeof (fuzz))) {
                state.trimanew = 1;
                state.trimfuzz = fuzz;
            }
            unlock ("trimset");
        }
        else if (!strncmp ("settrim", p, 7)) {
            fz_irect fuzz;
            int trimmargins;

            ret = sscanf (p + 7, " %d %d %d %d %d",
                          &trimmargins, &fuzz.x0, &fuzz.y0, &fuzz.x1, &fuzz.y1);
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
            initpdims (0);
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

    if (trylock (__func__)) {
        ok = 0;
        goto done;
    }
    realloctexts (Int_val (texcount_v));
    ok = 1;
    unlock (__func__);

 done:
    CAMLreturn (Val_bool (ok));
}

static void recti (int x0, int y0, int x1, int y1)
{
    GLfloat *v = state.vertices;

    glVertexPointer (2, GL_FLOAT, 0, v);
    v[0] = x0; v[1] = y0;
    v[2] = x1; v[3] = y0;
    v[4] = x0; v[5] = y1;
    v[6] = x1; v[7] = y1;
    glDrawArrays (GL_TRIANGLE_STRIP, 0, 4);
}

static void showsel (struct page *page, int ox, int oy)
{
    int seen = 0;
    fz_irect bbox;
    fz_rect rect;
    fz_stext_line *line;
    fz_page_block *pageb;
    fz_stext_block *block;
    struct mark first, last;
    unsigned char selcolor[] = {15,15,15,140};

    first = page->fmark;
    last = page->lmark;

    if (!first.span || !last.span) return;

    glEnable (GL_BLEND);
    glBlendFunc (GL_SRC_ALPHA, GL_SRC_ALPHA);
    glColor4ubv (selcolor);

    ox += state.pagedims[page->pdimno].bounds.x0;
    oy += state.pagedims[page->pdimno].bounds.y0;
    for (pageb = page->text->blocks;
         pageb < page->text->blocks + page->text->len;
         ++pageb) {
        if (pageb->type != FZ_PAGE_BLOCK_TEXT) continue;
        block = pageb->u.text;

        for (line = block->lines;
             line < block->lines + block->len;
             ++line) {
            fz_stext_span *span;
            rect = fz_empty_rect;

            for (span = line->first_span; span; span = span->next) {
                int i, j, k;
                bbox.x0 = bbox.y0 = bbox.x1 = bbox.y1 = 0;

                j = 0;
                k = span->len - 1;

                if (span == page->fmark.span && span == page->lmark.span) {
                    seen = 1;
                    j = MIN (first.i, last.i);
                    k = MAX (first.i, last.i);
                }
                else {
                    if (span == first.span) {
                        seen = 1;
                        j = first.i;
                    }
                    else if (span == last.span) {
                        seen = 1;
                        k = last.i;
                    }
                }

                if (seen) {
                    for (i = j; i <= k; ++i) {
                        fz_rect bbox1;
                        fz_union_rect (&rect,
                                       fz_stext_char_bbox (state.ctx, &bbox1,
                                                           span, i));
                    }
                    fz_round_rect (&bbox, &rect);
                    lprintf ("%d %d %d %d oy=%d ox=%d\n",
                             bbox.x0,
                             bbox.y0,
                             bbox.x1,
                             bbox.y1,
                             oy, ox);

                    recti (bbox.x0 + ox, bbox.y0 + oy,
                           bbox.x1 + ox, bbox.y1 + oy);
                    if (span == last.span) {
                        goto done;
                    }
                    rect = fz_empty_rect;
                }
            }
        }
    }
 done:
    glDisable (GL_BLEND);
}

#include "glfont.c"

static void stipplerect (fz_matrix *m,
                         fz_point *p1,
                         fz_point *p2,
                         fz_point *p3,
                         fz_point *p4,
                         GLfloat *texcoords,
                         GLfloat *vertices)
{
    fz_transform_point (p1, m);
    fz_transform_point (p2, m);
    fz_transform_point (p3, m);
    fz_transform_point (p4, m);
    {
        float w, h, s, t;

        w = p2->x - p1->x;
        h = p2->y - p1->y;
        t = sqrtf (w*w + h*h) * .25f;

        w = p3->x - p2->x;
        h = p3->y - p2->y;
        s = sqrtf (w*w + h*h) * .25f;

        texcoords[0] = 0; vertices[0] = p1->x; vertices[1] = p1->y;
        texcoords[1] = t; vertices[2] = p2->x; vertices[3] = p2->y;

        texcoords[2] = 0; vertices[4] = p2->x; vertices[5] = p2->y;
        texcoords[3] = s; vertices[6] = p3->x; vertices[7] = p3->y;

        texcoords[4] = 0; vertices[8] = p3->x; vertices[9] = p3->y;
        texcoords[5] = t; vertices[10] = p4->x; vertices[11] = p4->y;

        texcoords[6] = 0; vertices[12] = p4->x; vertices[13] = p4->y;
        texcoords[7] = s; vertices[14] = p1->x; vertices[15] = p1->y;
    }
    glDrawArrays (GL_LINES, 0, 8);
}

static void solidrect (fz_matrix *m,
                       fz_point *p1,
                       fz_point *p2,
                       fz_point *p3,
                       fz_point *p4,
                       GLfloat *vertices)
{
    fz_transform_point (p1, m);
    fz_transform_point (p2, m);
    fz_transform_point (p3, m);
    fz_transform_point (p4, m);
    vertices[0] = p1->x; vertices[1] = p1->y;
    vertices[2] = p2->x; vertices[3] = p2->y;

    vertices[4] = p3->x; vertices[5] = p3->y;
    vertices[6] = p4->x; vertices[7] = p4->y;
    glDrawArrays (GL_TRIANGLE_FAN, 0, 4);
}

static void highlightlinks (struct page *page, int xoff, int yoff)
{
    int i;
    fz_matrix ctm, tm, pm;
    fz_link *link, *links;
    GLfloat *texcoords = state.texcoords;
    GLfloat *vertices = state.vertices;

    links = fz_load_links (state.ctx, page->fzpage);

    glEnable (GL_TEXTURE_1D);
    glEnable (GL_BLEND);
    glBlendFunc (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glBindTexture (GL_TEXTURE_1D, state.stid);

    xoff -= state.pagedims[page->pdimno].bounds.x0;
    yoff -= state.pagedims[page->pdimno].bounds.y0;
    fz_translate (&tm, xoff, yoff);
    pm = pagectm (page);
    fz_concat (&ctm, &pm, &tm);

    glTexCoordPointer (1, GL_FLOAT, 0, texcoords);
    glVertexPointer (2, GL_FLOAT, 0, vertices);

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

        switch (link->dest.kind) {
        case FZ_LINK_GOTO: glColor3ub (255, 0, 0); break;
        case FZ_LINK_URI: glColor3ub (0, 0, 255); break;
        case FZ_LINK_LAUNCH: glColor3ub (0, 255, 0); break;
        default: glColor3ub (0, 0, 0); break;
        }
        stipplerect (&ctm, &p1, &p2, &p3, &p4, texcoords, vertices);
    }

    for (i = 0; i < page->annotcount; ++i) {
        fz_point p1, p2, p3, p4;
        struct annot *annot = &page->annots[i];

        p1.x = annot->bbox.x0;
        p1.y = annot->bbox.y0;

        p2.x = annot->bbox.x1;
        p2.y = annot->bbox.y0;

        p3.x = annot->bbox.x1;
        p3.y = annot->bbox.y1;

        p4.x = annot->bbox.x0;
        p4.y = annot->bbox.y1;

        glColor3ub (0, 0, 128);
        stipplerect (&ctm, &p1, &p2, &p3, &p4, texcoords, vertices);
    }

    glDisable (GL_BLEND);
    glDisable (GL_TEXTURE_1D);
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
        fz_drop_stext_page (state.ctx, page->text);
        page->fmark.i = -1;
        page->lmark.i = -1;
        page->fmark.span = NULL;
        page->lmark.span = NULL;
        page->text = NULL;
    }
    if (page->sheet) {
        fz_drop_stext_sheet (state.ctx, page->sheet);
        page->sheet = NULL;
    }
}

static void dropannots (struct page *page)
{
    if (page->annots) {
        free (page->annots);
        page->annots = NULL;
        page->annotcount = 0;
    }
}

static void ensureannots (struct page *page)
{
    int i, count = 0;
    size_t annotsize = sizeof (*page->annots);
    fz_annot *annot;

    if (state.gen != page->agen) {
        dropannots (page);
        page->agen = state.gen;
    }
    if (page->annots) return;

    for (annot = fz_first_annot (state.ctx, page->fzpage);
         annot;
         annot = fz_next_annot (state.ctx, annot)) {
        count++;
    }

    if (count > 0) {
        page->annotcount = count;
        page->annots = calloc (count, annotsize);
        if (!page->annots) {
            err (1, "calloc annots %d", count);
        }

        for (annot = fz_first_annot (state.ctx, page->fzpage), i = 0;
             annot;
             annot = fz_next_annot (state.ctx, annot), i++) {
            fz_rect rect;

            fz_bound_annot (state.ctx, annot, &rect);
            page->annots[i].annot = annot;
            fz_round_rect (&page->annots[i].bbox, &rect);
        }
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
    int i, count;
    size_t slinksize = sizeof (*page->slinks);
    fz_link *link, *links;

    ensureannots (page);
    if (state.gen != page->sgen) {
        dropslinks (page);
        page->sgen = state.gen;
    }
    if (page->slinks) return;

    links = fz_load_links (state.ctx, page->fzpage);
    ctm = pagectm (page);

    count = page->annotcount;
    for (link = links; link; link = link->next) {
        count++;
    }
    if (count > 0) {
        int j;

        page->slinkcount = count;
        page->slinks = calloc (count, slinksize);
        if (!page->slinks) {
            err (1, "calloc slinks %d", count);
        }

        for (i = 0, link = links; link; ++i, link = link->next) {
            fz_rect rect;

            rect = link->rect;
            fz_transform_rect (&rect, &ctm);
            page->slinks[i].tag = SLINK;
            page->slinks[i].u.link = link;
            fz_round_rect (&page->slinks[i].bbox, &rect);
        }
        for (j = 0; j < page->annotcount; ++j, ++i) {
            fz_rect rect;
            fz_bound_annot (state.ctx, page->annots[j].annot, &rect);
            fz_transform_rect (&rect, &ctm);
            fz_round_rect (&page->slinks[i].bbox, &rect);

            page->slinks[i].tag = SANNOT;
            page->slinks[i].u.annot = page->annots[j].annot;
        }
        qsort (page->slinks, count, slinksize, compareslinks);
    }
}

/* slightly tweaked fmt_ulong by D.J. Bernstein */
static void fmt_linkn (char *s, unsigned int u)
{
  unsigned int len; unsigned int q;
  unsigned int zma = 'z' - 'a' + 1;
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
            recti (x0, y0, x1, y1);
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
    unsigned char *texdata;

    offset = 0;
    for (slice1 = tile->slices; slice != slice1; slice1++) {
        offset += slice1->h * tile->w * tile->pixmap->n;
    }
    if (slice->texindex != -1 && slice->texindex < state.texcount
        && state.texowners[slice->texindex].slice == slice) {
        glBindTexture (TEXT_TYPE, state.texids[slice->texindex]);
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
        else {
            state.texowners[texindex].h = slice->h;
        }

        state.texowners[texindex].w = tile->w;
        state.texowners[texindex].slice = slice;
        slice->texindex = texindex;

        glBindTexture (TEXT_TYPE, state.texids[texindex]);
#if TEXT_TYPE == GL_TEXTURE_2D
        glTexParameteri (TEXT_TYPE, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
        glTexParameteri (TEXT_TYPE, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
        glTexParameteri (TEXT_TYPE, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
        glTexParameteri (TEXT_TYPE, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
#endif
        if (tile->pbo) {
            state.glBindBufferARB (GL_PIXEL_UNPACK_BUFFER_ARB, tile->pbo->id);
            texdata = 0;
        }
        else {
            texdata = tile->pixmap->samples;
        }
        if (subimage) {
            glTexSubImage2D (TEXT_TYPE,
                             0,
                             0,
                             0,
                             tile->w,
                             slice->h,
                             state.texform,
                             state.texty,
                             texdata+offset
                );
        }
        else {
            glTexImage2D (TEXT_TYPE,
                          0,
                          state.texiform,
                          tile->w,
                          slice->h,
                          0,
                          state.texform,
                          state.texty,
                          texdata+offset
                );
        }
        if (tile->pbo) {
            state.glBindBufferARB (GL_PIXEL_UNPACK_BUFFER_ARB, 0);
        }
    }
}

CAMLprim value ml_begintiles (value unit_v)
{
    CAMLparam1 (unit_v);
    glEnable (TEXT_TYPE);
    glTexCoordPointer (2, GL_FLOAT, 0, state.texcoords);
    glVertexPointer (2, GL_FLOAT, 0, state.vertices);
    CAMLreturn (unit_v);
}

CAMLprim value ml_endtiles (value unit_v)
{
    CAMLparam1 (unit_v);
    glDisable (TEXT_TYPE);
    CAMLreturn (unit_v);
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
    struct tile *tile = parse_pointer (__func__, s);
    int slicey, firstslice;
    struct slice *slice;
    GLfloat *texcoords = state.texcoords;
    GLfloat *vertices = state.vertices;

    firstslice = tiley / tile->sliceheight;
    slice = &tile->slices[firstslice];
    slicey = tiley % tile->sliceheight;

    while (disph > 0) {
        int dh;

        dh = slice->h - slicey;
        dh = MIN (disph, dh);
        uploadslice (tile, slice);

        texcoords[0] = tilex;       texcoords[1] = slicey;
        texcoords[2] = tilex+dispw; texcoords[3] = slicey;
        texcoords[4] = tilex;       texcoords[5] = slicey+dh;
        texcoords[6] = tilex+dispw; texcoords[7] = slicey+dh;

        vertices[0] = dispx;        vertices[1] = dispy;
        vertices[2] = dispx+dispw;  vertices[3] = dispy;
        vertices[4] = dispx;        vertices[5] = dispy+dh;
        vertices[6] = dispx+dispw;  vertices[7] = dispy+dh;

#if TEXT_TYPE == GL_TEXTURE_2D
        for (int i = 0; i < 8; ++i) {
            texcoords[i] /= ((i & 1) == 0 ? tile->w : slice->h);
        }
#endif

        glDrawArrays (GL_TRIANGLE_STRIP, 0, 4);
        dispy += dh;
        disph -= dh;
        slice++;
        ARSERT (!(slice - tile->slices >= tile->slicecount && disph > 0));
        slicey = 0;
    }
    CAMLreturn (Val_unit);
}

static void drawprect (struct page *page, int xoff, int yoff, value rects_v)
{
    fz_matrix ctm, tm, pm;
    fz_point p1, p2, p3, p4;
    GLfloat *vertices = state.vertices;
    double *v = (double *) rects_v;

    xoff -= state.pagedims[page->pdimno].bounds.x0;
    yoff -= state.pagedims[page->pdimno].bounds.y0;
    fz_translate (&tm, xoff, yoff);
    pm = pagectm (page);
    fz_concat (&ctm, &pm, &tm);

    glEnable (GL_BLEND);
    glVertexPointer (2, GL_FLOAT, 0, vertices);

    glColor4dv (v);
    p1.x = v[4];
    p1.y = v[5];

    p2.x = v[6];
    p2.y = v[5];

    p3.x = v[6];
    p3.y = v[7];

    p4.x = v[4];
    p4.y = v[7];
    solidrect (&ctm, &p1, &p2, &p3, &p4, vertices);
    glDisable (GL_BLEND);
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
    struct page *page = parse_pointer (__func__, s);

    if (!page->fzpage) {
        /* deal with loadpage failed pages */
        goto done;
    }

    ensureannots (page);

    if (hlmask & 1) highlightlinks (page, xoff, yoff);
    if (trylock (__func__)) {
        noff = 0;
        goto done;
    }
    if (hlmask & 2) {
        highlightslinks (page, xoff, yoff, noff, targ, tlen, hfsize);
        noff = page->slinkcount;
    }
    if (page->tgen == state.gen) {
        showsel (page, xoff, yoff);
    }
    unlock (__func__);

 done:
    CAMLreturn (Val_int (noff));
}

CAMLprim value ml_drawprect (value ptr_v, value xoff_v, value yoff_v,
                              value rects_v)
{
    CAMLparam4 (ptr_v, xoff_v, yoff_v, rects_v);
    int xoff = Int_val (xoff_v);
    int yoff = Int_val (yoff_v);
    char *s = String_val (ptr_v);
    struct page *page = parse_pointer (__func__, s);

    drawprect (page, xoff, yoff, rects_v);
    CAMLreturn (Val_unit);
}

static struct annot *getannot (struct page *page, int x, int y)
{
    int i;
    fz_point p;
    fz_matrix ctm;
    const fz_matrix *tctm;
    pdf_document *pdf = pdf_specifics (state.ctx, state.doc);

    if (!page->annots) return NULL;

    if (pdf) {
        trimctm ((pdf_page *) page->fzpage, page->pdimno);
        tctm = &state.pagedims[page->pdimno].tctm;
    }
    else {
        tctm = &fz_identity;
    }

    p.x = x;
    p.y = y;

    fz_concat (&ctm, tctm, &state.pagedims[page->pdimno].ctm);
    fz_invert_matrix (&ctm, &ctm);
    fz_transform_point (&p, &ctm);

    if (pdf) {
        for (i = 0; i < page->annotcount; ++i) {
            struct annot *a = &page->annots[i];
            pdf_annot *annot = (pdf_annot *) a->annot;
            if (p.x >= annot->pagerect.x0 && p.x <= annot->pagerect.x1) {
                if (p.y >= annot->pagerect.y0 && p.y <= annot->pagerect.y1) {
                    return a;
                }
            }
        }
    }
    return NULL;
}

static fz_link *getlink (struct page *page, int x, int y)
{
    fz_point p;
    fz_matrix ctm;
    fz_link *link, *links;

    links = fz_load_links (state.ctx, page->fzpage);

    p.x = x;
    p.y = y;

    ctm = pagectm (page);
    fz_invert_matrix (&ctm, &ctm);
    fz_transform_point (&p, &ctm);

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
        fz_matrix ctm;
        fz_device *tdev;

        page->text = fz_new_stext_page (state.ctx);
        page->sheet = fz_new_stext_sheet (state.ctx);
        tdev = fz_new_stext_device (state.ctx, page->sheet, page->text);
        ctm = pagectm (page);
        fz_run_display_list (state.ctx, page->dlist,
                             tdev, &ctm, &fz_infinite_rect, NULL);
        qsort (page->text->blocks, page->text->len,
               sizeof (*page->text->blocks), compareblocks);
        fz_drop_device (state.ctx, tdev);
    }
}

CAMLprim value ml_find_page_with_links (value start_page_v, value dir_v)
{
    CAMLparam2 (start_page_v, dir_v);
    CAMLlocal1 (ret_v);
    int i, dir = Int_val (dir_v);
    int start_page = Int_val (start_page_v);
    int end_page = dir > 0 ? state.pagecount : -1;
    pdf_document *pdf = pdf_specifics (state.ctx, state.doc);

    fz_var (end_page);
    ret_v = Val_int (0);
    lock (__func__);
    for (i = start_page + dir; i != end_page; i += dir) {
        int found;

        fz_var (found);
        if (pdf) {
            pdf_page *page = NULL;

            fz_var (page);
            fz_try (state.ctx) {
                page = pdf_load_page (state.ctx, pdf, i);
                found = !!page->links || !!page->annots;
            }
            fz_catch (state.ctx) {
                found = 0;
            }
            if (page) {
                fz_drop_page (state.ctx, &page->super);
            }
        }
        else {
            fz_page *page = fz_load_page (state.ctx, state.doc, i);
            found = !!fz_load_links (state.ctx, page);
            fz_drop_page (state.ctx, page);
        }

        if (found) {
            ret_v = caml_alloc_small (1, 1);
            Field (ret_v, 0) = Val_int (i);
            goto unlock;
        }
    }
 unlock:
    unlock (__func__);
    CAMLreturn (ret_v);
}

enum { dir_first, dir_last };
enum { dir_first_visible, dir_left, dir_right, dir_down, dir_up };

CAMLprim value ml_findlink (value ptr_v, value dir_v)
{
    CAMLparam2 (ptr_v, dir_v);
    CAMLlocal2 (ret_v, pos_v);
    struct page *page;
    int dirtag, i, slinkindex;
    struct slink *found = NULL ,*slink;
    char *s = String_val (ptr_v);

    page = parse_pointer (__func__, s);
    ret_v = Val_int (0);
    /* This is scary we are not taking locks here ensureslinks does
       not modify state and given that we obtained the page it can not
       disappear under us either */
    lock (__func__);
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

    unlock (__func__);
    CAMLreturn (ret_v);
}

enum { uuri, ugoto, utext, uunexpected, ulaunch,
       unamed, uremote, uremotedest, uannot };

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
                fz_transform_point (&p, &pdim->lctm);                   \
                if (p.y < 0) p.y = 0;                                   \
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
        {                                                               \
            int rty;                                                    \
                                                                        \
            str_v = caml_copy_string (link->dest.ld.gotor.file_spec);   \
            pageno = link->dest.ld.gotor.page;                          \
            if (pageno == -1) {                                         \
                gr_v = caml_copy_string (link->dest.ld.gotor.dest);     \
                rty = uremotedest;                                      \
            }                                                           \
            else {                                                      \
                gr_v = Val_int (pageno);                                \
                rty = uremote;                                          \
            }                                                           \
            tup_v = caml_alloc_tuple (2);                               \
            ret_v = caml_alloc_small (1, rty);                          \
            Field (tup_v, 0) = str_v;                                   \
            Field (tup_v, 1) = gr_v;                                    \
            Field (ret_v, 0) = tup_v;                                   \
        }                                                               \
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
    CAMLlocal4 (ret_v, tup_v, str_v, gr_v);
    fz_link *link;
    struct page *page;
    struct pagedim *pdim;
    char *s = String_val (ptr_v);
    struct slink *slink;

    /* See ml_findlink for caveat */

    ret_v = Val_int (0);
    page = parse_pointer (__func__, s);
    ensureslinks (page);
    pdim = &state.pagedims[page->pdimno];
    slink = &page->slinks[Int_val (n_v)];
    if (slink->tag == SLINK) {
        link = slink->u.link;
        LINKTOVAL;
    }
    else {
        ret_v = caml_alloc_small (1, uannot);
        tup_v = caml_alloc_tuple (2);
        Field (ret_v, 0) = tup_v;
        Field (tup_v, 0) = ptr_v;
        Field (tup_v, 1) = n_v;
    }

    CAMLreturn (ret_v);
}

CAMLprim value ml_getannotcontents (value ptr_v, value n_v)
{
    CAMLparam2 (ptr_v, n_v);
    pdf_document *pdf = pdf_specifics (state.ctx, state.doc);
    if (pdf) {
        char *s = String_val (ptr_v);
        struct page *page;
        struct slink *slink;

        page = parse_pointer (__func__, s);
        slink = &page->slinks[Int_val (n_v)];
        CAMLreturn (caml_copy_string (
                        pdf_annot_contents (state.ctx, pdf,
                                            (pdf_annot *) slink->u.annot)));
    }
    else {
        CAMLreturn (caml_copy_string (""));
    }
}

CAMLprim value ml_getlinkcount (value ptr_v)
{
    CAMLparam1 (ptr_v);
    struct page *page;
    char *s = String_val (ptr_v);

    page = parse_pointer (__func__, s);
    CAMLreturn (Val_int (page->slinkcount));
}

CAMLprim value ml_getlinkrect (value ptr_v, value n_v)
{
    CAMLparam2 (ptr_v, n_v);
    CAMLlocal1 (ret_v);
    struct page *page;
    struct slink *slink;
    char *s = String_val (ptr_v);
    /* See ml_findlink for caveat */

    page = parse_pointer (__func__, s);
    ret_v = caml_alloc_tuple (4);
    ensureslinks (page);

    slink = &page->slinks[Int_val (n_v)];
    Field (ret_v, 0) = Val_int (slink->bbox.x0);
    Field (ret_v, 1) = Val_int (slink->bbox.y0);
    Field (ret_v, 2) = Val_int (slink->bbox.x1);
    Field (ret_v, 3) = Val_int (slink->bbox.y1);
    unlock (__func__);
    CAMLreturn (ret_v);
}

CAMLprim value ml_whatsunder (value ptr_v, value x_v, value y_v)
{
    CAMLparam3 (ptr_v, x_v, y_v);
    CAMLlocal4 (ret_v, tup_v, str_v, gr_v);
    fz_link *link;
    struct annot *annot;
    struct page *page;
    char *ptr = String_val (ptr_v);
    int x = Int_val (x_v), y = Int_val (y_v);
    struct pagedim *pdim;

    ret_v = Val_int (0);
    if (trylock (__func__)) {
        goto done;
    }

    page = parse_pointer (__func__, ptr);
    pdim = &state.pagedims[page->pdimno];
    x += pdim->bounds.x0;
    y += pdim->bounds.y0;

    {
        annot = getannot (page, x, y);
        if (annot) {
            int i, n = -1;

            ensureslinks (page);
            for (i = 0; i < page->slinkcount; ++i) {
                if (page->slinks[i].tag == SANNOT
                    && page->slinks[i].u.annot == annot->annot) {
                    n = i;
                    break;
                }
            }
            ret_v = caml_alloc_small (1, uannot);
            tup_v = caml_alloc_tuple (2);
            Field (ret_v, 0) = tup_v;
            Field (tup_v, 0) = ptr_v;
            Field (tup_v, 1) = Val_int (n);
            goto unlock;
        }
    }

    link = getlink (page, x, y);
    if (link) {
        LINKTOVAL;
    }
    else {
        fz_rect *b;
        fz_page_block *pageb;
        fz_stext_block *block;

        ensuretext (page);
        for (pageb = page->text->blocks;
             pageb < page->text->blocks + page->text->len;
             ++pageb) {
            fz_stext_line *line;
            if (pageb->type != FZ_PAGE_BLOCK_TEXT) continue;
            block = pageb->u.text;

            b = &block->bbox;
            if (!(x >= b->x0 && x <= b->x1 && y >= b->y0 && y <= b->y1))
                continue;

            for (line = block->lines;
                 line < block->lines + block->len;
                 ++line) {
                fz_stext_span *span;

                b = &line->bbox;
                if (!(x >= b->x0 && x <= b->x1 && y >= b->y0 && y <= b->y1))
                    continue;

                for (span = line->first_span; span; span = span->next) {
                    int charnum;

                    b = &span->bbox;
                    if (!(x >= b->x0 && x <= b->x1 && y >= b->y0 && y <= b->y1))
                        continue;

                    for (charnum = 0; charnum < span->len; ++charnum) {
                        fz_rect bbox;
                        fz_stext_char_bbox (state.ctx, &bbox, span, charnum);
                        b = &bbox;

                        if (x >= b->x0 && x <= b->x1
                            && y >= b->y0 && y <= b->y1) {
                            fz_stext_style *style = span->text->style;
                            const char *n2 =
                                style->font
                                ? style->font->name
                                : "Span has no font name"
                                ;
                            FT_FaceRec *face = style->font->ft_face;
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
                            if (str_v == Val_unit) {
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
    unlock (__func__);

 done:
    CAMLreturn (ret_v);
}

enum { mark_page, mark_block, mark_line, mark_word };

static int uninteresting (int c)
{
    return c == ' ' || c == '\n' || c == '\t' || c == '\n' || c == '\r'
        || ispunct (c);
}

CAMLprim value ml_clearmark (value ptr_v)
{
    CAMLparam1 (ptr_v);
    char *s = String_val (ptr_v);
    struct page *page;

    if (trylock (__func__)) {
        goto done;
    }

    page = parse_pointer (__func__, s);
    page->fmark.span = NULL;
    page->lmark.span = NULL;
    page->fmark.i = 0;
    page->lmark.i = 0;

    unlock (__func__);
 done:
    CAMLreturn (Val_unit);
}

CAMLprim value ml_markunder (value ptr_v, value x_v, value y_v, value mark_v)
{
    CAMLparam4 (ptr_v, x_v, y_v, mark_v);
    CAMLlocal1 (ret_v);
    fz_rect *b;
    struct page *page;
    fz_stext_line *line;
    fz_page_block *pageb;
    fz_stext_block *block;
    struct pagedim *pdim;
    int mark = Int_val (mark_v);
    char *s = String_val (ptr_v);
    int x = Int_val (x_v), y = Int_val (y_v);

    ret_v = Val_bool (0);
    if (trylock (__func__)) {
        goto done;
    }

    page = parse_pointer (__func__, s);
    pdim = &state.pagedims[page->pdimno];

    ensuretext (page);

    if (mark == mark_page) {
        int i;
        fz_page_block *pb1 = NULL, *pb2 = NULL;

        for (i = 0; i < page->text->len; ++i) {
            if (page->text->blocks[i].type == FZ_PAGE_BLOCK_TEXT) {
                pb1 = &page->text->blocks[i];
                break;
            }
        }
        if (!pb1) goto unlock;

        for (i = page->text->len - 1; i >= 0; --i) {
            if (page->text->blocks[i].type == FZ_PAGE_BLOCK_TEXT) {
                pb2 = &page->text->blocks[i];
                break;
            }
        }
        if (!pb2) goto unlock;

        block = pb1->u.text;

        page->fmark.i = 0;
        page->fmark.span = block->lines->first_span;

        block = pb2->u.text;
        line = &block->lines[block->len - 1];
        page->lmark.i = line->last_span->len - 1;
        page->lmark.span = line->last_span;
        ret_v = Val_bool (1);
        goto unlock;
    }

    x += pdim->bounds.x0;
    y += pdim->bounds.y0;

    for (pageb = page->text->blocks;
         pageb < page->text->blocks + page->text->len;
         ++pageb) {
        if (pageb->type != FZ_PAGE_BLOCK_TEXT) continue;
        block = pageb->u.text;

        b = &block->bbox;
        if (!(x >= b->x0 && x <= b->x1 && y >= b->y0 && y <= b->y1))
            continue;

        if (mark == mark_block) {
            page->fmark.i = 0;
            page->fmark.span = block->lines->first_span;

            line = &block->lines[block->len - 1];
            page->lmark.i = line->last_span->len - 1;
            page->lmark.span = line->last_span;
            ret_v = Val_bool (1);
            goto unlock;
        }

        for (line = block->lines;
             line < block->lines + block->len;
             ++line) {
            fz_stext_span *span;

            b = &line->bbox;
            if (!(x >= b->x0 && x <= b->x1 && y >= b->y0 && y <= b->y1))
                continue;

            if (mark == mark_line) {
                page->fmark.i = 0;
                page->fmark.span = line->first_span;

                page->lmark.i = line->last_span->len - 1;
                page->lmark.span = line->last_span;
                ret_v = Val_bool (1);
                goto unlock;
            }

            for (span = line->first_span; span; span = span->next) {
                int charnum;

                b = &span->bbox;
                if (!(x >= b->x0 && x <= b->x1 && y >= b->y0 && y <= b->y1))
                    continue;

                for (charnum = 0; charnum < span->len; ++charnum) {
                    fz_rect bbox;
                    fz_stext_char_bbox (state.ctx, &bbox, span, charnum);
                    b = &bbox;

                    if (x >= b->x0 && x <= b->x1 && y >= b->y0 && y <= b->y1) {
                        /* unicode ftw */
                        int charnum2, charnum3 = -1, charnum4 = -1;

                        if (uninteresting (span->text[charnum].c)) goto unlock;

                        for (charnum2 = charnum; charnum2 >= 0; --charnum2) {
                            if (uninteresting (span->text[charnum2].c)) {
                                charnum3 = charnum2 + 1;
                                break;
                            }
                        }
                        if (charnum3 == -1) charnum3 = 0;

                        charnum4 = charnum;
                        for (charnum2 = charnum + 1;
                             charnum2 < span->len;
                             ++charnum2) {
                            if (uninteresting (span->text[charnum2].c)) break;
                            charnum4 = charnum2;
                        }

                        page->fmark.i = charnum3;
                        page->fmark.span = span;

                        page->lmark.i = charnum4;
                        page->lmark.span = span;
                        ret_v = Val_bool (1);
                        goto unlock;
                    }
                }
            }
        }
    }
 unlock:
    if (!Bool_val (ret_v)) {
        page->fmark.span = NULL;
        page->lmark.span = NULL;
        page->fmark.i = 0;
        page->lmark.i = 0;
    }
    unlock (__func__);

 done:
    CAMLreturn (ret_v);
}

CAMLprim value ml_rectofblock (value ptr_v, value x_v, value y_v)
{
    CAMLparam3 (ptr_v, x_v, y_v);
    CAMLlocal2 (ret_v, res_v);
    fz_rect *b = NULL;
    struct page *page;
    fz_page_block *pageb;
    struct pagedim *pdim;
    char *s = String_val (ptr_v);
    int x = Int_val (x_v), y = Int_val (y_v);

    ret_v = Val_int (0);
    if (trylock (__func__)) {
        goto done;
    }

    page = parse_pointer (__func__, s);
    pdim = &state.pagedims[page->pdimno];
    x += pdim->bounds.x0;
    y += pdim->bounds.y0;

    ensuretext (page);

    for (pageb = page->text->blocks;
         pageb < page->text->blocks + page->text->len;
         ++pageb) {
        switch (pageb->type) {
        case FZ_PAGE_BLOCK_TEXT:
            b = &pageb->u.text->bbox;
            break;

        case FZ_PAGE_BLOCK_IMAGE:
            b = &pageb->u.image->bbox;
            break;

        default:
            continue;
        }

        if (x >= b->x0 && x <= b->x1 && y >= b->y0 && y <= b->y1)
            break;
        b = NULL;
    }
    if (b) {
        res_v = caml_alloc_small (4 * Double_wosize, Double_array_tag);
        ret_v = caml_alloc_small (1, 1);
        Store_double_field (res_v, 0, b->x0);
        Store_double_field (res_v, 1, b->x1);
        Store_double_field (res_v, 2, b->y0);
        Store_double_field (res_v, 3, b->y1);
        Field (ret_v, 0) = res_v;
    }
    unlock (__func__);

 done:
    CAMLreturn (ret_v);
}

CAMLprim value ml_seltext (value ptr_v, value rect_v)
{
    CAMLparam2 (ptr_v, rect_v);
    fz_rect b;
    struct page *page;
    struct pagedim *pdim;
    char *s = String_val (ptr_v);
    int i, x0, x1, y0, y1, fi, li;
    fz_stext_line *line;
    fz_page_block *pageb;
    fz_stext_block *block;
    fz_stext_span *span, *fspan, *lspan;

    if (trylock (__func__)) {
        goto done;
    }

    page = parse_pointer (__func__, s);
    ensuretext (page);

    pdim = &state.pagedims[page->pdimno];
    x0 = Int_val (Field (rect_v, 0)) + pdim->bounds.x0;
    y0 = Int_val (Field (rect_v, 1)) + pdim->bounds.y0;
    x1 = Int_val (Field (rect_v, 2)) + pdim->bounds.x0;
    y1 = Int_val (Field (rect_v, 3)) + pdim->bounds.y0;

    if (y0 > y1) {
        int t = y0;
        y0 = y1;
        y1 = t;
        x0 = x1;
        x1 = t;
    }

    fi = page->fmark.i;
    fspan = page->fmark.span;

    li = page->lmark.i;
    lspan = page->lmark.span;

    for (pageb = page->text->blocks;
         pageb < page->text->blocks + page->text->len;
         ++pageb) {
        if (pageb->type != FZ_PAGE_BLOCK_TEXT) continue;
        block = pageb->u.text;
        for (line = block->lines;
             line < block->lines + block->len;
             ++line) {

            for (span = line->first_span; span; span = span->next) {
                for (i = 0; i < span->len; ++i) {
                    fz_stext_char_bbox (state.ctx, &b, span, i);

                    if (x0 >= b.x0 && x0 <= b.x1
                        && y0 >= b.y0 && y0 <= b.y1) {
                        fspan = span;
                        fi = i;
                    }
                    if (x1 >= b.x0 && x1 <= b.x1
                        && y1 >= b.y0 && y1 <= b.y1) {
                        lspan = span;
                        li = i;
                    }
                }
            }
        }
    }
    if (x1 < x0 && fspan == lspan) {
        i = fi;
        span = fspan;

        fi = li;
        fspan = lspan;

        li = i;
        lspan = span;
    }

    page->fmark.i = fi;
    page->fmark.span = fspan;

    page->lmark.i = li;
    page->lmark.span = lspan;

    unlock (__func__);

 done:
    CAMLreturn (Val_unit);
}

static int UNUSED_ATTR pipespan (FILE *f, fz_stext_span *span, int a, int b)
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
CAMLprim value ml_spawn (value UNUSED_ATTR u1, value UNUSED_ATTR u2)
{
    caml_failwith ("ml_popen not implemented under Cygwin");
}
#else
CAMLprim value ml_spawn (value command_v, value fds_v)
{
    CAMLparam2 (command_v, fds_v);
    CAMLlocal2 (l_v, tup_v);
    int ret, ret1;
    pid_t pid;
    char *msg = NULL;
    value earg_v = Nothing;
    posix_spawnattr_t attr;
    posix_spawn_file_actions_t fa;
    char *argv[] = { "/bin/sh", "-c", NULL, NULL };

    argv[2] = String_val (command_v);

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

    if ((ret = posix_spawn (&pid, "/bin/sh", &fa, &attr, argv, environ))) {
        msg = "posix_spawn";
        goto fail;
    }

 fail:
    if ((ret1 = posix_spawnattr_destroy (&attr)) != 0) {
        fprintf (stderr, "posix_spawnattr_destroy: %s\n", strerror (ret1));
    }

 fail1:
    if ((ret1 = posix_spawn_file_actions_destroy (&fa)) != 0) {
        fprintf (stderr, "posix_spawn_file_actions_destroy: %s\n",
                 strerror (ret1));
    }

    if (msg)
        unix_error (ret, msg, earg_v);

    CAMLreturn (Val_int (pid));
}
#endif

CAMLprim value ml_hassel (value ptr_v)
{
    CAMLparam1 (ptr_v);
    CAMLlocal1 (ret_v);
    struct page *page;
    char *s = String_val (ptr_v);

    ret_v = Val_bool (0);
    if (trylock (__func__)) {
        goto done;
    }

    page = parse_pointer (__func__, s);
    ret_v = Val_bool (page->fmark.span && page->lmark.span);
    unlock (__func__);
 done:
    CAMLreturn (ret_v);
}

CAMLprim value ml_copysel (value fd_v, value ptr_v)
{
    CAMLparam2 (fd_v, ptr_v);
    FILE *f;
    int seen = 0;
    struct page *page;
    fz_stext_line *line;
    fz_page_block *pageb;
    fz_stext_block *block;
    int fd = Int_val (fd_v);
    char *s = String_val (ptr_v);

    if (trylock (__func__)) {
        goto done;
    }

    page = parse_pointer (__func__, s);

    if (!page->fmark.span || !page->lmark.span) {
        fprintf (stderr, "nothing to copy on page %d\n", page->pageno);
        goto unlock;
    }

    f = fdopen (fd, "w");
    if (!f) {
        fprintf (stderr, "failed to fdopen sel pipe (from fd %d): %s\n",
                 fd, strerror (errno));
        f = stdout;
    }

    for (pageb = page->text->blocks;
         pageb < page->text->blocks + page->text->len;
         ++pageb) {
        if (pageb->type != FZ_PAGE_BLOCK_TEXT) continue;
        block = pageb->u.text;
        for (line = block->lines;
             line < block->lines + block->len;
             ++line) {
            fz_stext_span *span;

            for (span = line->first_span; span; span = span->next) {
                int a, b;

                seen |= span == page->fmark.span || span == page->lmark.span;
                a = span == page->fmark.span ? page->fmark.i : 0;
                b = span == page->lmark.span ? page->lmark.i : span->len - 1;

                if (seen) {
                    if (pipespan (f, span, a, b)) {
                        goto close;
                    }
                    if (span == page->lmark.span) {
                        goto close;
                    }
                    if (span == line->last_span) {
                        if (putc ('\n', f) == EOF) {
                            fprintf (stderr,
                                     "failed break line on sel pipe: %s\n",
                                     strerror (errno));
                            goto close;
                        }
                    }
                }
            }
        }
    }
 close:
    if (f != stdout) {
        int ret = fclose (f);
        fd = -1;
        if (ret == -1) {
            if (errno != ECHILD) {
                fprintf (stderr, "failed to close sel pipe: %s\n",
                         strerror (errno));
            }
        }
    }
 unlock:
    unlock (__func__);

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
    if (trylock (__func__)) {
        box = fz_empty_rect;
    }
    else {
        box = state.pagedims[pagedimno].mediabox;
        unlock (__func__);
    }

    Store_double_field (ret_v, 0, box.x0);
    Store_double_field (ret_v, 1, box.x1);
    Store_double_field (ret_v, 2, box.y0);
    Store_double_field (ret_v, 3, box.y1);

    CAMLreturn (ret_v);
}

CAMLprim value ml_zoom_for_height (value winw_v, value winh_v,
                                   value dw_v, value cols_v)
{
    CAMLparam4 (winw_v, winh_v, dw_v, cols_v);
    CAMLlocal1 (ret_v);
    int i;
    double zoom = -1.;
    double maxh = 0.0;
    struct pagedim *p;
    double winw = Int_val (winw_v);
    double winh = Int_val (winh_v);
    double dw = Int_val (dw_v);
    double cols = Int_val (cols_v);
    double pw = 1.0, ph = 1.0;

    if (trylock (__func__)) {
        goto done;
    }

    for (i = 0, p = state.pagedims; i < state.pagedimcount; ++i, ++p) {
        double w = p->pagebox.x1 / cols;
        double h = p->pagebox.y1;
        if (h > maxh) {
            maxh = h;
            ph = h;
            if (state.fitmodel != FitProportional) pw = w;
        }
        if ((state.fitmodel == FitProportional) && w > pw) pw = w;
    }

    zoom = (((winh / ph) * pw) + dw) / winw;
    unlock (__func__);
 done:
    ret_v = caml_copy_double (zoom);
    CAMLreturn (ret_v);
}

CAMLprim value ml_getmaxw (value unit_v)
{
    CAMLparam1 (unit_v);
    CAMLlocal1 (ret_v);
    int i;
    double maxw = -1.;
    struct pagedim *p;

    if (trylock (__func__)) {
        goto done;
    }

    for (i = 0, p = state.pagedims; i < state.pagedimcount; ++i, ++p) {
        double w = p->pagebox.x1;
        maxw = MAX (maxw, w);
    }

    unlock (__func__);
 done:
    ret_v = caml_copy_double (maxw);
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
    fz_rect rect;
    fz_irect bbox;
    fz_matrix ctm;
    fz_device *dev;
    char *s = String_val (opaque_v);
    struct page *page = parse_pointer (__func__, s);

    ret_v = caml_alloc_tuple (4);
    dev = fz_new_bbox_device (state.ctx, &rect);
    dev->hints |= FZ_IGNORE_SHADE;

    ctm = pagectm (page);
    fz_run_page (state.ctx, page->fzpage, dev, &ctm, NULL);

    fz_drop_device (state.ctx, dev);
    fz_round_rect (&bbox, &rect);
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

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wvariadic-macros"
#include <X11/Xlib.h>
#include <X11/cursorfont.h>
#pragma GCC diagnostic pop

#ifdef USE_EGL
#include <EGL/egl.h>
#else
#include <GL/glx.h>
#endif

static const int shapes[] = {
    XC_left_ptr, XC_hand2, XC_exchange, XC_fleur, XC_xterm
};

#define CURS_COUNT (sizeof (shapes) / sizeof (shapes[0]))

static struct {
    Window wid;
    Display *dpy;
#ifdef USE_EGL
    EGLContext ctx;
    EGLConfig conf;
    EGLSurface win;
    EGLDisplay *edpy;
#else
    GLXContext ctx;
#endif
    XVisualInfo *visual;
    Cursor curs[CURS_COUNT];
} glx;

#ifdef VISAVIS
static VisualID initvisual (void)
{
    /* On this system with: `Haswell-ULT Integrated Graphics
       Controller' and Mesa 11.0.6; perf stat reports [1] that when
       using glX chosen visual and auto scrolling some document in
       fullscreen the power/energy-gpu is more than 1 joule bigger
       than when using hand picked visual that stands alone in glxinfo
       output: it's dead last in the list and it's the only one with
       `visual dep' (sic) of 32

       No clue what's going on here...

       [1] perf stat -a -I 1200 -e "power/energy-gpu/"
     */
    XVisualInfo info;
    int ret = 1;

    info.depth = 32;
    info.class = TrueColor;
    glx.visual = XGetVisualInfo (glx.dpy, VisualDepthMask | VisualClassMask,
                                 &info, &ret);
    if (!ret || !glx.visual) {
        XCloseDisplay (glx.dpy);
        caml_failwith ("XGetVisualInfo");
    }
    return glx.visual->visualid;
}
#endif

static void initcurs (void)
{
    for (size_t n = 0; n < CURS_COUNT; ++n) {
        glx.curs[n] = XCreateFontCursor (glx.dpy, shapes[n]);
    }
}

CAMLprim void ml_setbgcol (value color_v)
{
    CAMLparam1 (color_v);
    XSetWindowBackground (glx.dpy, glx.wid, Int_val (color_v));
    CAMLreturn0;
}

#ifdef USE_EGL
CAMLprim value ml_glxinit (value display_v, value wid_v, value screen_v)
{
    CAMLparam3 (display_v, wid_v, screen_v);
    int major, minor;
    int num_conf;
    EGLint visid;
    EGLint attribs[] = {
#ifdef VISAVIS
        EGL_NATIVE_VISUAL_ID, 0,
#else
        EGL_DEPTH_SIZE, 24,
#endif
        EGL_SURFACE_TYPE, EGL_WINDOW_BIT,
        EGL_RENDERABLE_TYPE, EGL_OPENGL_BIT,
        EGL_NONE
    };
    EGLConfig conf;

    glx.dpy = XOpenDisplay (String_val (display_v));
    if (!glx.dpy) {
        caml_failwith ("XOpenDisplay");
    }

    eglBindAPI (EGL_OPENGL_API);

    glx.edpy = eglGetDisplay (glx.dpy);
    if (glx.dpy == EGL_NO_DISPLAY) {
        caml_failwith ("eglGetDisplay");
    }

    if (!eglInitialize (glx.edpy, &major, &minor)) {
        caml_failwith ("eglInitialize");
    }

#ifdef VISAVIS
    attribs[1] = visid = initvisual ();
#endif

    if (!eglChooseConfig (glx.edpy, attribs, &conf, 1, &num_conf) ||
        !num_conf) {
        caml_failwith ("eglChooseConfig");
    }

    glx.conf = conf;
#ifndef VISAVIS
    if (!eglGetConfigAttrib (glx.edpy, glx.conf,
                             EGL_NATIVE_VISUAL_ID, &visid)) {
        caml_failwith ("eglGetConfigAttrib");
    }
#endif
    initcurs ();

    glx.wid = Int_val (wid_v);
    CAMLreturn (Val_int (visid));
}

CAMLprim value ml_glxcompleteinit (value unit_v)
{
    CAMLparam1 (unit_v);

    glx.ctx = eglCreateContext (glx.edpy, glx.conf, EGL_NO_CONTEXT, NULL);
    if (!glx.ctx) {
        caml_failwith ("eglCreateContext");
    }

    glx.win = eglCreateWindowSurface (glx.edpy, glx.conf,
                                      glx.wid, NULL);
    if (glx.win == EGL_NO_SURFACE) {
        caml_failwith ("eglCreateWindowSurface");
    }

    XFree (glx.visual);
    if (!eglMakeCurrent (glx.edpy, glx.win, glx.win, glx.ctx)) {
        glx.ctx = NULL;
        caml_failwith ("eglMakeCurrent");
    }
    CAMLreturn (Val_unit);
}
#else
CAMLprim value ml_glxinit (value display_v, value wid_v, value screen_v)
{
    CAMLparam3 (display_v, wid_v, screen_v);

    glx.dpy = XOpenDisplay (String_val (display_v));
    if (!glx.dpy) {
        caml_failwith ("XOpenDisplay");
    }

#ifdef VISAVIS
    initvisual ();
#else
    int attribs[] = { GLX_RGBA, GLX_DOUBLEBUFFER, None };
    glx.visual = glXChooseVisual (glx.dpy, Int_val (screen_v), attribs);
    if (!glx.visual) {
        XCloseDisplay (glx.dpy);
        caml_failwith ("glXChooseVisual");
    }
#endif
    initcurs ();

    glx.wid = Int_val (wid_v);
    CAMLreturn (Val_int (glx.visual->visualid));
}

CAMLprim value ml_glxcompleteinit (value unit_v)
{
    CAMLparam1 (unit_v);

    glx.ctx = glXCreateContext (glx.dpy, glx.visual, NULL, True);
    if (!glx.ctx) {
        caml_failwith ("glXCreateContext");
    }

    XFree (glx.visual);
    glx.visual = NULL;

    if (!glXMakeCurrent (glx.dpy, glx.wid, glx.ctx)) {
        glXDestroyContext (glx.dpy, glx.ctx);
        glx.ctx = NULL;
        caml_failwith ("glXMakeCurrent");
    }
    CAMLreturn (Val_unit);
}
#endif

CAMLprim value ml_setcursor (value cursor_v)
{
    CAMLparam1 (cursor_v);
    size_t cursn = Int_val (cursor_v);

    if (cursn >= CURS_COUNT) caml_failwith ("cursor index out of range");
    XDefineCursor (glx.dpy, glx.wid, glx.curs[cursn]);
    XFlush (glx.dpy);
    CAMLreturn (Val_unit);
}

CAMLprim value ml_swapb (value unit_v)
{
    CAMLparam1 (unit_v);
#ifdef USE_EGL
    if (!eglSwapBuffers (glx.edpy, glx.win)) {
        caml_failwith ("eglSwapBuffers");
    }
#else
    glXSwapBuffers (glx.dpy, glx.wid);
#endif
    CAMLreturn (Val_unit);
}

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

enum { piunknown, pilinux, piosx, pisun, pibsd, picygwin };

CAMLprim value ml_platform (value unit_v)
{
    CAMLparam1 (unit_v);
    CAMLlocal2 (tup_v, arr_v);
    int platid = piunknown;
    struct utsname buf;

#if defined __linux__
    platid = pilinux;
#elif defined __CYGWIN__
    platid = picygwin;
#elif defined __DragonFly__ || defined __FreeBSD__
    || defined __OpenBSD__ || defined __NetBSD__
    platid = pibsd;
#elif defined __sun__
    platid = pisun;
#elif defined __APPLE__
    platid = piosx;
#endif
    if (uname (&buf)) err (1, "uname");

    tup_v = caml_alloc_tuple (2);
    {
        char const *sar[] = {
            buf.sysname,
            buf.release,
            buf.version,
            buf.machine,
            NULL
        };
        arr_v = caml_copy_string_array (sar);
    }
    Field (tup_v, 0) = Val_int (platid);
    Field (tup_v, 1) = arr_v;
    CAMLreturn (tup_v);
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

CAMLprim value ml_getpbo (value w_v, value h_v, value cs_v)
{
    CAMLparam2 (w_v, h_v);
    CAMLlocal1 (ret_v);
    struct bo *pbo;
    int w = Int_val (w_v);
    int h = Int_val (h_v);
    int cs = Int_val (cs_v);

    if (state.bo_usable) {
        pbo = calloc (sizeof (*pbo), 1);
        if (!pbo) {
            err (1, "calloc pbo");
        }

        switch (cs) {
        case 0:
        case 1:
            pbo->size = w*h*4;
            break;
        case 2:
            pbo->size = w*h*2;
            break;
        default:
            errx (1, "%s: invalid colorspace %d", __func__, cs);
        }

        state.glGenBuffersARB (1, &pbo->id);
        state.glBindBufferARB (GL_PIXEL_UNPACK_BUFFER_ARB, pbo->id);
        state.glBufferDataARB (GL_PIXEL_UNPACK_BUFFER_ARB, pbo->size,
                               NULL, GL_STREAM_DRAW);
        pbo->ptr = state.glMapBufferARB (GL_PIXEL_UNPACK_BUFFER_ARB,
                                         GL_READ_WRITE);
        state.glBindBufferARB (GL_PIXEL_UNPACK_BUFFER_ARB, 0);
        if (!pbo->ptr) {
            fprintf (stderr, "glMapBufferARB failed: %#x\n", glGetError ());
            state.glDeleteBuffersARB (1, &pbo->id);
            free (pbo);
            ret_v = caml_copy_string ("0");
        }
        else {
            int res;
            char *s;

            res = snprintf (NULL, 0, "%" FMT_ptr, FMT_ptr_cast (pbo));
            if (res < 0) {
                err (1, "snprintf %" FMT_ptr " failed", FMT_ptr_cast (pbo));
            }
            s = malloc (res+1);
            if (!s) {
                err (1, "malloc %d bytes failed", res+1);
            }
            res = sprintf (s, "%" FMT_ptr, FMT_ptr_cast (pbo));
            if (res < 0) {
                err (1, "sprintf %" FMT_ptr " failed", FMT_ptr_cast (pbo));
            }
            ret_v = caml_copy_string (s);
            free (s);
        }
    }
    else {
        ret_v = caml_copy_string ("0");
    }
    CAMLreturn (ret_v);
}

CAMLprim value ml_freepbo (value s_v)
{
    CAMLparam1 (s_v);
    char *s = String_val (s_v);
    struct tile *tile = parse_pointer (__func__, s);

    if (tile->pbo) {
        state.glDeleteBuffersARB (1, &tile->pbo->id);
        tile->pbo->id = -1;
        tile->pbo->ptr = NULL;
        tile->pbo->size = -1;
    }
    CAMLreturn (Val_unit);
}

CAMLprim value ml_unmappbo (value s_v)
{
    CAMLparam1 (s_v);
    char *s = String_val (s_v);
    struct tile *tile = parse_pointer (__func__, s);

    if (tile->pbo) {
        state.glBindBufferARB (GL_PIXEL_UNPACK_BUFFER_ARB, tile->pbo->id);
        if (state.glUnmapBufferARB (GL_PIXEL_UNPACK_BUFFER_ARB) == GL_FALSE) {
            errx (1, "glUnmapBufferARB failed: %#x\n", glGetError ());
        }
        tile->pbo->ptr = NULL;
        state.glBindBufferARB (GL_PIXEL_UNPACK_BUFFER_ARB, 0);
    }
    CAMLreturn (Val_unit);
}

static void setuppbo (void)
{
#ifdef USE_EGL
#define GGPA(n) (*(void (**) ()) &state.n = eglGetProcAddress (#n))
#else
#define GGPA(n) (*(void (**) ()) &state.n = glXGetProcAddress ((GLubyte *) #n))
#endif
    state.bo_usable = GGPA (glBindBufferARB)
        && GGPA (glUnmapBufferARB)
        && GGPA (glMapBufferARB)
        && GGPA (glBufferDataARB)
        && GGPA (glGenBuffersARB)
        && GGPA (glDeleteBuffersARB);
#undef GGPA
}

CAMLprim value ml_bo_usable (value unit_v)
{
    CAMLparam1 (unit_v);
    CAMLreturn (Val_bool (state.bo_usable));
}

CAMLprim value ml_unproject (value ptr_v, value x_v, value y_v)
{
    CAMLparam3 (ptr_v, x_v, y_v);
    CAMLlocal2 (ret_v, tup_v);
    struct page *page;
    char *s = String_val (ptr_v);
    int x = Int_val (x_v), y = Int_val (y_v);
    struct pagedim *pdim;
    fz_point p;
    fz_matrix ctm;

    page = parse_pointer (__func__, s);
    pdim = &state.pagedims[page->pdimno];

    ret_v = Val_int (0);
    if (trylock (__func__)) {
        goto done;
    }

    if (pdf_specifics (state.ctx, state.doc)) {
        trimctm ((pdf_page *) page->fzpage, page->pdimno);
        ctm = state.pagedims[page->pdimno].tctm;
    }
    else {
        ctm = fz_identity;
    }
    p.x = x + pdim->bounds.x0;
    p.y = y + pdim->bounds.y0;

    fz_concat (&ctm, &pdim->tctm, &pdim->ctm);
    fz_invert_matrix (&ctm, &ctm);
    fz_transform_point (&p, &ctm);

    tup_v = caml_alloc_tuple (2);
    ret_v = caml_alloc_small (1, 1);
    Field (tup_v, 0) = Val_int (p.x);
    Field (tup_v, 1) = Val_int (p.y);
    Field (ret_v, 0) = tup_v;

    unlock (__func__);
 done:
    CAMLreturn (ret_v);
}

CAMLprim value ml_project (value ptr_v, value pageno_v, value pdimno_v,
                           value x_v, value y_v)
{
    CAMLparam5 (ptr_v, pageno_v, pdimno_v, x_v, y_v);
    CAMLlocal1 (ret_v);
    struct page *page;
    char *s = String_val (ptr_v);
    int pageno = Int_val (pageno_v);
    int pdimno = Int_val (pdimno_v);
    double x = Double_val (x_v), y = Double_val (y_v);
    struct pagedim *pdim;
    fz_point p;
    fz_matrix ctm;

    ret_v = Val_int (0);
    lock (__func__);

    if (!*s) {
        page = loadpage (pageno, pdimno);
    }
    else {
        page = parse_pointer (__func__, s);
    }
    pdim = &state.pagedims[pdimno];

    if (pdf_specifics (state.ctx, state.doc)) {
        trimctm ((pdf_page *) page->fzpage, page->pdimno);
        ctm = state.pagedims[page->pdimno].tctm;
    }
    else {
        ctm = fz_identity;
    }
    p.x = x + pdim->bounds.x0;
    p.y = y + pdim->bounds.y0;

    fz_concat (&ctm, &pdim->tctm, &pdim->ctm);
    fz_transform_point (&p, &ctm);

    ret_v = caml_alloc_tuple (2);
    Field (ret_v, 0) = caml_copy_double (p.x);
    Field (ret_v, 1) = caml_copy_double (p.y);

    if (!*s) {
        freepage (page);
    }
    unlock (__func__);
    CAMLreturn (ret_v);
}

CAMLprim value ml_addannot (value ptr_v, value x_v, value y_v,
                            value contents_v)
{
    CAMLparam4 (ptr_v, x_v, y_v, contents_v);
    pdf_document *pdf = pdf_specifics (state.ctx, state.doc);

    if (pdf) {
        pdf_annot *annot;
        struct page *page;
        fz_point p;
        char *s = String_val (ptr_v);

        page = parse_pointer (__func__, s);
        annot = pdf_create_annot (state.ctx, pdf,
                                  (pdf_page *) page->fzpage, FZ_ANNOT_TEXT);
        p.x = Int_val (x_v);
        p.y = Int_val (y_v);
        pdf_set_annot_contents (state.ctx, pdf, annot, String_val (contents_v));
        pdf_set_text_annot_position (state.ctx, pdf, annot, p);
        state.dirty = 1;
    }
    CAMLreturn (Val_unit);
}

CAMLprim value ml_delannot (value ptr_v, value n_v)
{
    CAMLparam2 (ptr_v, n_v);
    pdf_document *pdf = pdf_specifics (state.ctx, state.doc);

    if (pdf) {
        struct page *page;
        char *s = String_val (ptr_v);
        struct slink *slink;

        page = parse_pointer (__func__, s);
        slink = &page->slinks[Int_val (n_v)];
        pdf_delete_annot (state.ctx, pdf,
                          (pdf_page *) page->fzpage,
                          (pdf_annot *) slink->u.annot);
        state.dirty = 1;
    }
    CAMLreturn (Val_unit);
}

CAMLprim value ml_modannot (value ptr_v, value n_v, value str_v)
{
    CAMLparam3 (ptr_v, n_v, str_v);
    pdf_document *pdf = pdf_specifics (state.ctx, state.doc);

    if (pdf) {
        struct page *page;
        char *s = String_val (ptr_v);
        struct slink *slink;

        page = parse_pointer (__func__, s);
        slink = &page->slinks[Int_val (n_v)];
        pdf_set_annot_contents (state.ctx, pdf, (pdf_annot *) slink->u.annot,
                                String_val (str_v));
        state.dirty = 1;
    }
    CAMLreturn (Val_unit);
}

CAMLprim value ml_hasunsavedchanges (value unit_v)
{
    CAMLparam1 (unit_v);
    CAMLreturn (Val_bool (state.dirty));
}

CAMLprim value ml_savedoc (value path_v)
{
    CAMLparam1 (path_v);
    pdf_document *pdf = pdf_specifics (state.ctx, state.doc);

    if (pdf) {
        pdf_save_document (state.ctx, pdf, String_val (path_v), NULL);
    }
    CAMLreturn (Val_unit);
}

static void makestippletex (void)
{
    const char pixels[] = "\xff\xff\0\0";
    glGenTextures (1, &state.stid);
    glBindTexture (GL_TEXTURE_1D, state.stid);
    glTexParameteri (GL_TEXTURE_1D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri (GL_TEXTURE_1D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexImage1D (
        GL_TEXTURE_1D,
        0,
        GL_ALPHA,
        4,
        0,
        GL_ALPHA,
        GL_UNSIGNED_BYTE,
        pixels
        );
}

CAMLprim value ml_fz_version (value UNUSED_ATTR unit_v)
{
    return caml_copy_string (FZ_VERSION);
}

#ifdef USE_FONTCONFIG
static struct {
    int inited;
    FcConfig *config;
} fc;

static fz_font *fc_load_system_font_func (fz_context *ctx,
                                          const char *name,
                                          int bold,
                                          int italic,
                                          int UNUSED_ATTR needs_exact_metrics)
{
    char *buf;
    size_t i, size;
    fz_font *font;
    FcChar8 *path;
    FcResult result;
    FcPattern *pat, *pat1;

    lprintf ("looking up %s bold:%d italic:%d needs_exact_metrics:%d\n",
             name, bold, italic, needs_exact_metrics);
    if (!fc.inited) {
        fc.inited = 1;
        fc.config = FcInitLoadConfigAndFonts ();
        if (!fc.config) {
            lprintf ("FcInitLoadConfigAndFonts failed\n");
            return NULL;
        }
    }
    if (!fc.config) return NULL;

    size = strlen (name);
    if (bold) size += sizeof (":bold") - 1;
    if (italic) size += sizeof (":italic") - 1;
    size += 1;

    buf = malloc (size);
    if (!buf) {
        err (1, "malloc %zu failed", size);
    }

    strcpy (buf, name);
    if (bold && italic) {
        strcat (buf, ":bold:italic");
    }
    else {
        if (bold) strcat (buf, ":bold");
        if (italic) strcat (buf, ":italic");
    }
    for (i = 0; i < size; ++i) {
        if (buf[i] == ',' || buf[i] == '-') buf[i] = ':';
    }

    lprintf ("fcbuf=%s\n", buf);
    pat = FcNameParse ((FcChar8 *) buf);
    if (!pat) {
        printd ("emsg FcNameParse failed\n");
        free (buf);
        return NULL;
    }

    if (!FcConfigSubstitute (fc.config, pat, FcMatchPattern)) {
        printd ("emsg FcConfigSubstitute failed\n");
        free (buf);
        return NULL;
    }
    FcDefaultSubstitute (pat);

    pat1 = FcFontMatch (fc.config, pat, &result);
    if (!pat1) {
        printd ("emsg FcFontMatch failed\n");
        FcPatternDestroy (pat);
        free (buf);
        return NULL;
    }

    if (FcPatternGetString (pat1, FC_FILE, 0, &path) != FcResultMatch) {
        printd ("emsg FcPatternGetString failed\n");
        FcPatternDestroy (pat);
        FcPatternDestroy (pat1);
        free (buf);
        return NULL;
    }

#if 0
    printd ("emsg name=%s path=%s\n", name, path);
#endif
    font = fz_new_font_from_file (ctx, name, (char *) path, 0, 0);
    FcPatternDestroy (pat);
    FcPatternDestroy (pat1);
    free (buf);
    return font;
}
#endif

CAMLprim value ml_init (value csock_v, value params_v)
{
    CAMLparam2 (csock_v, params_v);
    CAMLlocal2 (trim_v, fuzz_v);
    int ret;
    int texcount;
    char *fontpath;
    int colorspace;
    int mustoresize;
    int haspboext;

    state.csock         = Int_val (csock_v);
    state.rotate        = Int_val (Field (params_v, 0));
    state.fitmodel      = Int_val (Field (params_v, 1));
    trim_v              = Field (params_v, 2);
    texcount            = Int_val (Field (params_v, 3));
    state.sliceheight   = Int_val (Field (params_v, 4));
    mustoresize         = Int_val (Field (params_v, 5));
    colorspace          = Int_val (Field (params_v, 6));
    fontpath            = String_val (Field (params_v, 7));

    if (caml_string_length (Field (params_v, 8)) > 0) {
        state.trimcachepath = strdup (String_val (Field (params_v, 8)));

        if (!state.trimcachepath) {
            fprintf (stderr, "failed to strdup trimcachepath: %s\n",
                     strerror (errno));
        }
    }
    haspboext           = Bool_val (Field (params_v, 9));

    state.ctx = fz_new_context (NULL, NULL, mustoresize);
    fz_register_document_handlers (state.ctx);

#ifdef USE_FONTCONFIG
    if (Bool_val (Field (params_v, 10))) {
        fz_install_load_system_font_funcs (
            state.ctx, fc_load_system_font_func, NULL
            );
    }
#endif

    state.trimmargins = Bool_val (Field (trim_v, 0));
    fuzz_v            = Field (trim_v, 1);
    state.trimfuzz.x0 = Int_val (Field (fuzz_v, 0));
    state.trimfuzz.y0 = Int_val (Field (fuzz_v, 1));
    state.trimfuzz.x1 = Int_val (Field (fuzz_v, 2));
    state.trimfuzz.y1 = Int_val (Field (fuzz_v, 3));

    set_tex_params (colorspace);

    if (*fontpath) {
#ifndef USE_FONTCONFIG
        state.face = load_font (fontpath);
#else
        FcChar8 *path;
        FcResult result;
        char *buf = fontpath;
        FcPattern *pat, *pat1;

        fc.inited = 1;
        fc.config = FcInitLoadConfigAndFonts ();
        if (!fc.config) {
            errx (1, "FcInitLoadConfigAndFonts failed");
        }

        pat = FcNameParse ((FcChar8 *) buf);
        if (!pat) {
            errx (1, "FcNameParse failed");
        }

        if (!FcConfigSubstitute (fc.config, pat, FcMatchPattern)) {
            errx (1, "FcConfigSubstitute failed");
        }
        FcDefaultSubstitute (pat);

        pat1 = FcFontMatch (fc.config, pat, &result);
        if (!pat1) {
            errx (1, "FcFontMatch failed");
        }

        if (FcPatternGetString (pat1, FC_FILE, 0, &path) != FcResultMatch) {
            errx (1, "FcPatternGetString failed");
        }

        state.face = load_font ((char *) path);
        FcPatternDestroy (pat);
        FcPatternDestroy (pat1);
#endif
    }
    else {
        int len;
        const char *data = pdf_lookup_substitute_font (state.ctx, 0, 0,
                                                       0, 0, &len);
        state.face = load_builtin_font (data, len);
    }
    if (!state.face) _exit (1);

    realloctexts (texcount);

    if (haspboext) {
        setuppbo ();
    }

    makestippletex ();

    ret = pthread_create (&state.thread, NULL, mainloop, NULL);
    if (ret) {
        errx (1, "pthread_create: %s", strerror (ret));
    }

    CAMLreturn (Val_unit);
}

/* lots of code c&p-ed directly from mupdf */
#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <winsock2.h>
#define fionread_arg long
#define ssize_t int
#define FMT_ss "%d"
#ifdef _WIN64
#define FMT_s "%i64u"
#else
#define FMT_s "%u"
#endif
#pragma warning (disable:4244)
#pragma warning (disable:4996)
#pragma warning (disable:4995)
#endif

#ifdef _MSC_VER
#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
static void __declspec (noreturn) err (int exitcode, const char *fmt, ...)
{
    va_list ap;
    int errcode;

    errcode = errno;
    va_start (ap, fmt);
    vfprintf (stderr, fmt, ap);
    va_end (ap);
    fprintf (stderr, ": %s\n", strerror (errno));
    exit (exitcode);
}
static void __declspec (noreturn) errx (int exitcode, const char *fmt, ...)
{
    va_list ap;

    va_start (ap, fmt);
    vfprintf (stderr, fmt, ap);
    va_end (ap);
    fputc ('\n', stderr);
    exit (exitcode);
}
static void __declspec (noreturn) sockerr (int exitcode, const char *fmt, ...)
{
    va_list ap;

    va_start (ap, fmt);
    vfprintf (stderr, fmt, ap);
    va_end (ap);
    fprintf (stderr, ": wsaerror 0x%x\n", WSAGetLastError ());
    exit (exitcode);
}
#else
#define FMT_ss "%zd"
#define FMT_s "%zu"
#define fionread_arg int
#define ioctlsocket ioctl
#define _GNU_SOURCE
#include <err.h>
#define sockerr err
#endif
#include <regex.h>
#include <errno.h>
#include <ctype.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#ifndef _WIN32
#include <pthread.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#endif

#ifdef __APPLE__
#include <OpenGL/gl.h>
#else
#include <GL/gl.h>
#endif

#ifndef GL_TEXTURE_RECTANGLE_ARB
#define GL_TEXTURE_RECTANGLE_ARB          0x84F5
#endif

#include <caml/fail.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/unixsupport.h>

#include <fitz.h>
#include <mupdf.h>

#if 0
#define lprintf printf
#else
#define lprintf(...)
#endif

#ifdef FT_FREETYPE_H
#include FT_FREETYPE_H
#endif

#define ARSERT(cond) for (;;) {                         \
    if (!(cond)) {                                      \
        errx (1, "%s:%d " #cond, __FILE__, __LINE__);   \
    }                                                   \
    break;                                              \
}

struct slice {
    int texindex;
    int w, h;
};

struct pagedim {
    int pageno;
    int rotate;
    int left;
    fz_rect box;
    fz_bbox bbox;
    fz_matrix ctm, ctm1;
};

struct page {
    int pageno;
    int slicecount;
    fz_text_span *text;
    fz_pixmap *pixmap;
    pdf_page *drawpage;
    struct pagedim pagedim;
    struct mark {
        int i;
        fz_text_span *span;
    } fmark, lmark;
    struct slice slices[];
};

#if !defined _WIN32 && !defined __APPLE__
#define USE_XSEL
#endif

struct {
    int sock;
    int sliceheight;
    struct page *pig;
    struct pagedim *pagedims;
    int pagecount;
    int pagedimcount;
    pdf_xref *xref;
    fz_glyph_cache *cache;
    int w, h;

    int texindex;
    int texcount;
    GLuint *texids;

    GLenum texform;
    GLenum texty;

    struct {
        int w, h;
        struct slice *slice;
    } *texowners;

    int rotate;
    int proportional;
    int needoutline;

#ifdef _WIN32
    HANDLE thread;
#else
    pthread_t thread;
#endif
    FILE *xselpipe;
} state;

#ifdef _WIN32
static CRITICAL_SECTION critsec;

static void lock (void *unused)
{
    (void) unused;
    EnterCriticalSection (&critsec);
}

static void unlock (void *unused)
{
    (void) unused;
    LeaveCriticalSection (&critsec);
}

static int trylock (void *unused)
{
    return TryEnterCriticalSection (&critsec) == 0;
}
#else
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
#endif

static void *parse_pointer (const char *cap, const char *s)
{
    int ret;
    void *ptr;

    ret = sscanf (s, "%p", &ptr);
    if (ret != 1) {
        errx (1, "%s: cannot parse pointer in `%s'", cap, s);
    }
    return ptr;
}

static int hasdata (int sock)
{
    int ret;
    fionread_arg avail;
    ret = ioctlsocket (sock, FIONREAD, &avail);
    if (ret) sockerr (1, "hasdata: FIONREAD error ret=%d", ret);
    return avail > 0;
}

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

    n = recv (fd, p, size, 0);
    if (n - size) {
        if (!n) errx (1, "EOF while reading");
        sockerr (1, "recv (req %d, ret " FMT_ss ")", size, n);
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

    n = send (fd, buf, 4, 0);
    if (n != 4) {
        if (!n) errx (1, "EOF while writing length");
        sockerr (1, "send " FMT_ss, n);
    }

    n = send (fd, p, size, 0);
    if (n - size) {
        if (!n) errx (1, "EOF while writing data");
        sockerr (1, "send (req %d, ret " FMT_ss ")", size, n);
    }
}

static void
#ifdef __GNUC__
__attribute__ ((format (printf, 2, 3)))
#endif
    printd (int fd, const char *fmt, ...)
{
    int size = 200, len;
    va_list ap;
    char *buf;

    buf = malloc (size);
    for (;;) {
        if (!buf) err (errno, "malloc for temp buf (%d bytes) failed", size);

        va_start (ap, fmt);
        len = vsnprintf (buf, size, fmt, ap);
        va_end (ap);

        if (len > -1 && len < size) {
            writedata (fd, buf, len);
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

static void die (fz_error error)
{
    fz_catch (error, "aborting");
    if (state.xref)
       pdf_free_xref (state.xref);
    exit (1);
}

static void openxref (char *filename, char *password)
{
    int i;
    fz_error error;

    for (i = 0; i < state.texcount; ++i)  {
        state.texowners[i].slice = NULL;
    }

    if (state.cache) {
        fz_free_glyph_cache (state.cache);
    }

    state.cache = fz_new_glyph_cache ();
    if (!state.cache) {
        errx (1, "fz_newglyph_cache failed");
    }

   if (state.xref) {
        if (state.xref->store) {
            pdf_free_store (state.xref->store);
            state.xref->store = NULL;
        }
        pdf_free_xref (state.xref);
        state.xref = NULL;
    }

    if (state.pagedims) {
        free (state.pagedims);
        state.pagedims = NULL;
    }
    state.pagedimcount = 0;

    error = pdf_open_xref (&state.xref, filename, password);
    if (error) {
        die (error);
    }

    error = pdf_load_page_tree (state.xref);
    if (error) {
        die (error);
    }

    state.pagecount = pdf_count_pages (state.xref);
}

static int readlen (int fd)
{
    ssize_t n;
    unsigned char p[4];

    n = recv (fd, p, 4, 0);
    if (n != 4) {
        if (!n) errx (1, "EOF while reading length");
        sockerr (1, "recv " FMT_ss, n);
    }

    return (p[0] << 24) | (p[1] << 16) | (p[2] << 8) | p[3];
}

static void unlinkpage (struct page *page)
{
    int i;

    for (i = 0; i < page->slicecount; ++i) {
        struct slice *s = &page->slices[i];
        if (s->texindex != -1) {
            if (state.texowners[s->texindex].slice == s) {
                state.texowners[s->texindex].slice = NULL;
                ARSERT (state.texowners[s->texindex].w == s->w);
                ARSERT (state.texowners[s->texindex].h >= s->h);
            }
        }
    }
}

static void freepage (struct page *page)
{
    fz_drop_pixmap (page->pixmap);

    unlinkpage (page);

    if (page->text) {
        fz_free_text_span (page->text);
    }
    if (page->drawpage) {
        pdf_free_page (page->drawpage);
    }

    free (page);
}

static void subdivide (struct page *p)
{
    int i;
    int h = p->pixmap->h;
    int th = MIN (h, state.sliceheight);

    for (i = 0; i < p->slicecount; ++i) {
        struct slice *s = &p->slices[i];
        s->texindex = -1;
        s->h = MIN (th, h);
        s->w = p->pixmap->w;
        h -= th;
    }
}

static int compatpdims (struct pagedim *p1, struct pagedim *p2)
{
    return p1->rotate == p2->rotate
        && !memcmp (&p1->bbox, &p2->bbox, sizeof (p1->bbox))
        && !memcmp (&p1->ctm, &p2->ctm, sizeof (p1->ctm));
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

static void __attribute__ ((optimize ("O"))) clearpixmap (fz_pixmap *pixmap)
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
    else fz_clear_pixmap_with_color (pixmap, 0xff);
}
#else
#define clearpixmap(p) fz_clear_pixmap_with_color (p, 0xff)
#endif

static void *render (int pageno, int pindex)
{
    fz_error error;
    int slicecount;
    struct page *page = NULL;
    double start, end;
    pdf_page *drawpage;
    fz_device *idev;
    struct pagedim *pagedim;

    start = now ();
    printd (state.sock, "V rendering %d", pageno);

    pagedim = &state.pagedims[pindex];
    slicecount = (pagedim->bbox.y1 - pagedim->bbox.y0
                  + state.sliceheight - 1) / state.sliceheight;
    slicecount += slicecount == 0;

    if (state.pig) {
        if (compatpdims (&state.pig->pagedim, pagedim)) {
            page = state.pig;
            if (page->text) {
                fz_free_text_span (page->text);
                page->text = NULL;
            }
            if (page->drawpage) {
                pdf_free_page (page->drawpage);
                page->drawpage = NULL;
            }
        }
        else {
            freepage (state.pig);
        }
    }
    if (!page) {
        page = calloc (sizeof (*page)
                       + (slicecount * sizeof (struct slice)), 1);
        if (!page) {
            err (1, "calloc page %d\n", pageno);
        }
        page->pixmap = fz_new_pixmap_with_rect (fz_device_rgb, pagedim->bbox);
    }

    page->slicecount = slicecount;

    error = pdf_load_page (&drawpage, state.xref, pageno - 1);
    if (error)
        die (error);

    clearpixmap (page->pixmap);

    idev = fz_new_draw_device (state.cache, page->pixmap);
    if (!idev)
        die (fz_throw ("fz_newdrawdevice failed"));
    error = pdf_run_page (state.xref, drawpage, idev, pagedim->ctm);
    if (error)
        die (fz_rethrow (error, "pdf_runpage failed"));
    fz_free_device (idev);

    page->drawpage = drawpage;
    page->pagedim = *pagedim;
    page->pageno = pageno;
    subdivide (page);
    end = now ();

    pdf_age_store (state.xref->store, 3);

    printd (state.sock, "V rendering %d took %f sec", pageno, end - start);
    state.pig = NULL;
    return page;
}

static void initpdims (void)
{
    int pageno;
    double start, end;

    start = now ();
    for (pageno = 0; pageno < state.pagecount; ++pageno) {
        int rotate;
        fz_rect box;
        struct pagedim *p;
        fz_obj *obj, *pageobj;

        pageobj = state.xref->page_objs[pageno];

        obj = fz_dict_gets (pageobj, "CropBox");
        if (!fz_is_array (obj)) {
            obj = fz_dict_gets (pageobj, "MediaBox");
            if (!fz_is_array (obj)) {
                die (fz_throw ("cannot find page bounds %d (%d Rd)",
                               fz_to_num (pageobj), fz_to_gen (pageobj)));
            }
        }
        box = pdf_to_rect (obj);

        obj = fz_dict_gets (pageobj, "Rotate");
        if (fz_is_int (obj)) {
            rotate = fz_to_int (obj);
        }
        else  {
            rotate = 0;
        }
        rotate += state.rotate;

        p = &state.pagedims[state.pagedimcount - 1];
        if ((state.pagedimcount == 0)
            || (p->rotate != rotate || memcmp (&p->box, &box, sizeof (box)))) {
            size_t size;

            size = (state.pagedimcount + 1) * sizeof (*state.pagedims);
            state.pagedims = realloc (state.pagedims, size);
            if (!state.pagedims) {
                err (1, "realloc pagedims to " FMT_s " (%d elems)",
                     size, state.pagedimcount + 1);
            }
            p = &state.pagedims[state.pagedimcount++];
            p->rotate = rotate;
            p->box = box;
            p->pageno = pageno;
        }
    }
    end = now ();
    printd (state.sock, "T Processed %d pages in %f seconds",
            state.pagecount, end - start);
}

static void layout (void)
{
    int pindex;
    fz_matrix ctm;
    fz_rect box, box2;
    double zoom, w, maxw;
    struct pagedim *p = state.pagedims;

    if (state.proportional) {
        for (pindex = 0; pindex < state.pagedimcount; ++pindex, ++p) {
            box.x0 = MIN (p->box.x0, p->box.x1);
            box.y0 = MIN (p->box.y0, p->box.y1);
            box.x1 = MAX (p->box.x0, p->box.x1);
            box.y1 = MAX (p->box.y0, p->box.y1);

            ctm = fz_identity;
            ctm = fz_concat (ctm, fz_translate (0, -box.y1));
            ctm = fz_concat (ctm, fz_rotate (p->rotate));
            box2 = fz_transform_rect (ctm, box);
            w = box2.x1 - box2.x0;
            maxw = MAX (w, maxw);
        }
    }

    p = state.pagedims;
    for (pindex = 0; pindex < state.pagedimcount; ++pindex, ++p) {
        box.x0 = MIN (p->box.x0, p->box.x1);
        box.y0 = MIN (p->box.y0, p->box.y1);
        box.x1 = MAX (p->box.x0, p->box.x1);
        box.y1 = MAX (p->box.y0, p->box.y1);

        ctm = fz_identity;
        ctm = fz_concat (ctm, fz_translate (0, -box.y1));
        ctm = fz_concat (ctm, fz_rotate (p->rotate));
        box2 = fz_transform_rect (ctm, box);
        w = box2.x1 - box2.x0;

        if (state.proportional) {
            double scale = w / maxw;
            zoom = (state.w / w) * scale;
        }
        else {
            zoom = state.w / w;
        }
        ctm = fz_identity;
        ctm = fz_concat (ctm, fz_translate (0, -box.y1));
        ctm = fz_concat (ctm, fz_scale (zoom, -zoom));
        memcpy (&p->ctm1, &ctm, sizeof (ctm));
        ctm = fz_concat (ctm, fz_rotate (p->rotate));
        p->bbox = fz_round_rect (fz_transform_rect (ctm, box));
        p->left = state.proportional ? ((maxw - w) * zoom) / 2.0 : 0;
        memcpy (&p->ctm, &ctm, sizeof (ctm));
    }

    while (p-- != state.pagedims)  {
        printd (state.sock, "l %d %d %d %d",
                p->pageno, p->bbox.x1 - p->bbox.x0, p->bbox.y1 - p->bbox.y0,
                p->left);
    }
}

static void recurse_outline (pdf_outline *outline, int level)
{
    while (outline) {
        int i;
        fz_obj *obj;
        int pageno = -1;
        int top = 0, h = 0;

        if (!outline->link) goto next;

        obj = outline->link->dest;
        if (fz_is_indirect (obj)) {
            obj = fz_resolve_indirect (obj);
        }
        if (fz_is_array (obj)) {
            fz_obj *obj2;

            obj2 = fz_array_get (obj, 0);
            if (fz_is_int (obj2)) {
                pageno = fz_to_int (obj2);
            }
            else {
                pageno = pdf_find_page_number (state.xref, obj2);
            }

            if (fz_array_len (obj) > 3) {
                fz_point p;
                fz_obj *xo, *yo;

                xo = fz_array_get (obj, 2);
                yo = fz_array_get (obj, 3);
                if (!fz_is_null (xo) && !fz_is_null (yo)) {
                    struct pagedim *pagedim = state.pagedims;

                    for (i = 0; i < state.pagedimcount; ++i) {
                        if (state.pagedims[i].pageno > pageno)
                            break;
                        pagedim = &state.pagedims[i];
                    }
                    p.x = fz_to_int (xo);
                    p.y = fz_to_int (yo);
                    p = fz_transform_point (pagedim->ctm, p);
                    h = pagedim->bbox.y1 - pagedim->bbox.y0;
                    top = p.y;
                }
            }
        }
        else {
            pageno = pdf_find_page_number (state.xref, outline->link->dest);
        }

        lprintf ("%*c%s %d\n", level, ' ', outline->title, pageno);
        printd (state.sock, "o %d %d %d %d %s",
                level, pageno, top, h, outline->title);
    next:
        if (outline->child) {
            recurse_outline (outline->child, level + 1);
        }
        outline = outline->next;
    }
}

static void process_outline (void)
{
    pdf_outline *outline;

    if (!state.needoutline) return;

    state.needoutline = 0;
    outline = pdf_load_outline (state.xref);
    if (outline) {
        recurse_outline (outline, 0);
        pdf_free_outline (outline);
    }
}

static int comparespans (const void *l, const void *r)
{
    fz_text_span const *const*ls = l;
    fz_text_span const *const*rs = r;
    return (*ls)->text->bbox.y0 - (*rs)->text->bbox.y0;
}

/* wishful thinking function */
static void search (regex_t *re, int pageno, int y, int forward)
{
    int i, j;
    int ret;
    char *p;
    char buf[256];
    fz_matrix ctm;
    fz_error error;
    fz_device *tdev;
    pdf_page *drawpage;
    fz_text_span *text, *span, **pspan;
    struct pagedim *pdim, *pdimprev;
    int stop = 0;
    int niters = 0;
    int nspans;
    double start, end;

    start = now ();
    while (pageno >= 0 && pageno < state.pagecount && !stop) {
        if (niters++ == 5) {
            pdf_age_store (state.xref->store, 3);
            niters = 0;
            if (hasdata (state.sock)) {
                printd (state.sock, "T attention requested aborting search at %d",
                        pageno);
                stop = 1;
            }
            else {
                printd (state.sock, "T searching in page %d", pageno);
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

        error = pdf_load_page (&drawpage, state.xref, pageno);
        if (error)
            die (error);

        ctm = fz_rotate (pdim->rotate);

        text = fz_new_text_span ();
        tdev = fz_new_text_device (text);
        error = pdf_run_page (state.xref, drawpage, tdev, pdim->ctm1);
        if (error) die (error);
        fz_free_device (tdev);

        nspans = 0;
        for (span = text; span; span = span->next) {
            nspans++;
        }
        pspan = malloc (sizeof (void *) * nspans);
        if (!pspan) {
            err (1, "malloc span pointers " FMT_s, sizeof (void *) * nspans);
        }
        for (i = 0, span = text; span; span = span->next, ++i) {
            pspan[i] = span;
        }
        qsort (pspan, nspans, sizeof (fz_text_span *), comparespans);

        j = forward ? 0 : nspans - 1;
        while (nspans--) {
            regmatch_t rm;

            span = pspan[j];
            j += forward ? 1 : -1;
            p = buf;
            for (i = 0; i < MIN (span->len, (int) sizeof (buf) - 1); ++i) {
                if (forward) {
                    if (span->text[i].bbox.y0 < y + 1) {
                        continue;
                    }
                }
                else {
                    if (span->text[i].bbox.y0 > y - 1) {
                        continue;
                    }
                }
                if (span->text[i].c < 256) {
                    *p++ = span->text[i].c;
                }
                else  {
                    *p++ = '?';
                }
            }
            if (p == buf) {
                continue;
            }
            *p++ = 0;

            ret = regexec (re, buf, 1, &rm, 0);
            if (ret)  {
                if (ret != REG_NOMATCH) {
                    size_t size;
                    char errbuf[80];
                    size = regerror (ret, re, errbuf, sizeof (errbuf));
                    printd (state.sock,
                            "T regexec error `%.*s'",
                            (int) size, errbuf);
                    fz_free_text_span (text);
                    pdf_free_page (drawpage);
                    free (pspan);
                    return;
                }
            }
            else  {
                int xoff, yoff;
                fz_bbox *sb, *eb;
                fz_point p1, p2, p3, p4;

                xoff = pdim->left - pdim->bbox.x0;
                yoff = -pdim->bbox.y0;

                sb = &span->text[rm.rm_so].bbox;
                eb = &span->text[rm.rm_eo - 1].bbox;

                p1.x = sb->x0;
                p1.y = sb->y0;
                p2.x = eb->x1;
                p2.y = sb->y0;
                p3.x = eb->x1;
                p3.y = eb->y1;
                p4.x = sb->x0;
                p4.y = eb->y1;

                p1 = fz_transform_point (ctm, p1);
                p2 = fz_transform_point (ctm, p2);
                p3 = fz_transform_point (ctm, p3);
                p4 = fz_transform_point (ctm, p4);

                if (!stop) {
                    printd (state.sock, "F %d %d %f %f %f %f %f %f %f %f",
                            pageno, 1,
                            p1.x + xoff, p1.y + yoff,
                            p2.x + xoff, p2.y + yoff,
                            p3.x + xoff, p3.y + yoff,
                            p4.x + xoff, p4.y + yoff);

                    printd (state.sock, "T found at %d `%.*s' in %f sec",
                            pageno, rm.rm_eo - rm.rm_so, &buf[rm.rm_so],
                            now () - start);
                }
                else  {
                    printd (state.sock, "R %d %d %f %f %f %f %f %f %f %f",
                            pageno, 2,
                            p1.x + xoff, p1.y + yoff,
                            p2.x + xoff, p2.y + yoff,
                            p3.x + xoff, p3.y + yoff,
                            p4.x + xoff, p4.y + yoff);
                }
                stop = 1;
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
        fz_free_text_span (text);
        pdf_free_page (drawpage);
        free (pspan);
    }
    end = now ();
    if (!stop)  {
        printd (state.sock, "T no matches %f sec", end - start);
    }
    printd (state.sock, "D");
}

static
#ifdef _WIN32
DWORD _stdcall
#else
void *
#endif
mainloop (void *unused)
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
            fz_obj *obj;
            size_t filenamelen;
            char *password;
            char *filename = p + 5;
            char *p2 ;
            int angle, proportional;

            filenamelen = strlen (filename);
            password = filename + filenamelen + 1;
            p2 = password + strlen (password) + 1;

            ret = sscanf (p2, " %d %d", &angle, &proportional);
            if (ret != 2) {
                errx (1, "malformed open `%*s' ret=%d",
                      len - (p2 - p), p2, ret);
            }

            state.rotate = angle;
            state.proportional = proportional;

            openxref (filename, password);
            initpdims ();

            obj = fz_dict_gets (state.xref->trailer, "Info");
            if (obj) {
                char *s;

                obj = fz_dict_gets (obj, "Title");
                s = pdf_to_utf8 (obj);
                if (*s) {
                    printd (state.sock, "t %s", s);
                }
                fz_free (s);
            }

            state.needoutline = 1;
        }
        else if (!strncmp ("free", p, 4)) {
            void *ptr;

            ret = sscanf (p + 4, " %p", &ptr);
            if (ret != 1) {
                errx (1, "malformed free `%.*s' ret=%d", len, p, ret);
            }
            unlinkpage (ptr);
            state.pig = ptr;
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
                printd (state.sock, "T regcomp failed `%.*s'",
                        (int) size, errbuf);
            }
            else  {
                search (&re, pageno, y, forward);
                regfree (&re);
            }
        }
        else if (!strncmp ("geometry", p, 8)) {
            int w, h;

            printd (state.sock, "c");
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
            unlock ("geometry");
            printd (state.sock, "C %d", state.pagecount);
        }
        else if (!strncmp ("reinit", p, 6)) {
            float rotate;
            int proportional;

            printd (state.sock, "c");
            ret = sscanf (p + 6, " %f %d", &rotate, &proportional);
            if (ret != 2) {
                errx (1, "bad rotate line `%.*s' ret=%d", len, p, ret);
            }
            lock ("reinit");
            state.rotate = rotate;
            state.proportional = proportional;
            state.pagedimcount = 0;
            free (state.pagedims);
            state.pagedims = NULL;
            initpdims ();
            layout ();
            process_outline ();
            if (state.pig) {
                freepage (state.pig);
                state.pig = NULL;
            }
            unlock ("reinit");
            printd (state.sock, "C %d", state.pagecount);
        }
        else if (!strncmp ("render", p, 6)) {
            int pageno, pindex, w, h, ret;
            struct page *page;

            ret = sscanf (p + 6, " %d %d %d %d", &pageno, &pindex, &w, &h);
            if (ret != 4) {
                errx (1, "bad render line `%.*s' ret=%d", len, p, ret);
            }

            lock ("render");
            page = render (pageno, pindex);
            unlock ("render");

            printd (state.sock, "r %d %d %d %d %d %d %p",
                    pageno,
                    state.w,
                    state.h,
                    state.rotate,
                    state.proportional,
                    w * h * 4,
                    page);
        }
        else if (!strncmp ("interrupt", p, 9)) {
            printd (state.sock, "V interrupted");
        }
        else {
            errx (1, "unknown command %.*s", len, p);
        }
    }
    return 0;
}

static void showsel (struct page *page, int oy)
{
    int ox;
    fz_bbox bbox;
    fz_text_span *span;
    struct mark first, last;

    first = page->fmark;
    last = page->lmark;

    if (!first.span || !last.span) return;

    glEnable (GL_BLEND);
    glBlendFunc (GL_SRC_ALPHA, GL_SRC_ALPHA);
    glColor4f (0.5f, 0.5f, 0.0f, 0.6f);

    ox = -page->pixmap->x + page->pagedim.left;
    oy = -page->pixmap->y + oy;
    for (span = first.span; span; span = span->next) {
        int i, j, k;

        bbox.x0 = bbox.y0 = bbox.x1 = bbox.y1 = 0;

        j = 0;
        k = span->len - 1;

        if (span == page->fmark.span && span == page->lmark.span) {
            j = MIN (first.i, last.i);
            k = MAX (first.i, last.i);
        }
        else if (span == first.span) {
            j = first.i;
        }
        else if (span == last.span) {
            k = last.i;
        }

        for (i = j; i <= k; ++i) {
            bbox = fz_union_bbox (bbox, span->text[i].bbox);
        }
        lprintf ("%d %d %d %d oy=%d ox=%d\n",
                 bbox.x0,
                 bbox.y0,
                 bbox.x1,
                 bbox.y1,
                 oy, ox);

        glRecti (bbox.x0 + ox, bbox.y0 + oy, bbox.x1 + ox, bbox.y1 + oy);

        if (span == last.span) break;
    }
    glDisable (GL_BLEND);
}

static void highlightlinks (struct page *page, int yoff)
{
    pdf_link *link;
    int xoff;

    glPolygonMode (GL_FRONT_AND_BACK, GL_LINE);
    glEnable (GL_LINE_STIPPLE);
    glLineStipple (0.5, 0xcccc);

    xoff = -page->pixmap->x;
    yoff -= page->pixmap->y;

    glBegin (GL_QUADS);
    for (link = page->drawpage->links; link; link = link->next) {
        fz_point p1, p2, p3, p4;
        fz_matrix ctm = page->pagedim.ctm;

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

        switch (link->kind) {
        case PDF_LINK_GOTO: glColor3ub (255, 0, 0); break;
        case PDF_LINK_URI: glColor3ub (0, 0, 255); break;
        default: glColor3ub (0, 0, 0); break;
        }

        glVertex2f (p1.x + xoff, p1.y + yoff);
        glVertex2f (p2.x + xoff, p2.y + yoff);
        glVertex2f (p3.x + xoff, p3.y + yoff);
        glVertex2f (p4.x + xoff, p4.y + yoff);
    }
    glEnd ();

    glPolygonMode (GL_FRONT_AND_BACK, GL_FILL);
    glDisable (GL_LINE_STIPPLE);
}

static void upload2 (struct page *page, int slicenum, const char *cap)
{
    int i;
    int w;
    double start, end;
    struct slice *slice = &page->slices[slicenum];

    w = page->pixmap->w;

    ARSERT (w == slice->w);
    if (slice->texindex != -1
        && state.texowners[slice->texindex].slice == slice) {
        glBindTexture (GL_TEXTURE_RECTANGLE_ARB, state.texids[slice->texindex]);
    }
    else  {
        int subimage = 0;
        int index = (state.texindex++ % state.texcount);
        size_t offset = 0;

        for (i = 0; i < slicenum; ++i) {
            offset += w * page->slices[i].h * 4;
        }

        if (state.texowners[index].w == slice->w) {
            if (state.texowners[index].h >= slice->h ) {
                subimage = 1;
            }
            else {
                state.texowners[index].h = slice->h;
            }
        }
        else  {
            state.texowners[index].h = slice->h;
        }

        state.texowners[index].slice = slice;
        state.texowners[index].w = slice->w;
        slice->texindex = index;

        glBindTexture (GL_TEXTURE_RECTANGLE_ARB, state.texids[slice->texindex]);
        start = now ();
        if (subimage) {
            {
                GLenum err = glGetError ();
                if (err != GL_NO_ERROR) {
                    printf ("\033[0;31mERROR1 %d %d %#x\033[0m\n", w, slice->h, err);
                    abort ();
                }
            }
            glTexSubImage2D (GL_TEXTURE_RECTANGLE_ARB,
                             0,
                             0,
                             0,
                             w,
                             slice->h,
                             state.texform,
                             state.texty,
                             page->pixmap->samples + offset
                );
            {
                GLenum err = glGetError ();
                if (err != GL_NO_ERROR) {
                    printf ("\033[0;31mERROR %d %d %#x\033[0m\n", w, slice->h, err);
                    abort ();
                }
            }
        }
        else {
            glTexImage2D (GL_TEXTURE_RECTANGLE_ARB,
                          0,
                          GL_RGBA8,
                          w,
                          slice->h,
                          0,
                          state.texform,
                          state.texty,
                          page->pixmap->samples + offset
                );
        }

        end = now ();
        (void) start;
        (void) end;
        lprintf ("%s[%d] slice=%d(%d,%d) texid=%d %f sec\n",
                 subimage ? "sub" : "img",
                 page->pageno, slicenum,
                 slice->w, slice->h,
                 state.texids[slice->texindex],
                 end - start);
    }
}

CAMLprim value ml_draw (value args_v, value ptr_v)
{
    CAMLparam2 (args_v, ptr_v);
    int dispy = Int_val (Field (args_v, 0));
    int w = Int_val (Field (args_v, 1));
    int h = Int_val (Field (args_v, 2));
    int py = Int_val (Field (args_v, 3));
    int hlinks = Bool_val (Field (args_v, 4));
    char *s = String_val (ptr_v);
    int ret;
    void *ptr;
    struct page *page;
    int slicenum = 0;
    int yoff = dispy - py;

    ret = sscanf (s, "%p", &ptr);
    if (ret != 1) {
        errx (1, "cannot parse pointer `%s'", s);
    }
    page = ptr;

    w = page->pixmap->w;

    ARSERT (h >= 0 && "ml_draw wrong h");
    ARSERT (py <= page->pixmap->h && "ml_draw wrong py");

    glEnable (GL_TEXTURE_RECTANGLE_ARB);

    for (slicenum = 0; slicenum < page->slicecount; ++slicenum) {
        struct slice *slice = &page->slices[slicenum];
        if (slice->h > py) {
            break;
        }
        py -= slice->h;
    }

    h = MIN (state.h, h);
    while (h) {
        int th, left;
        struct slice *slice = &page->slices[slicenum];

        ARSERT (slicenum < page->slicecount && "ml_draw wrong slicenum");

        th = MIN (h, slice->h - py);
        upload2 (page, slicenum, "upload");

        left = page->pagedim.left;
        glBegin (GL_QUADS);
        {
            glTexCoord2i (0, py);
            glVertex2i (left, dispy);

            glTexCoord2i (w, py);
            glVertex2i (left+w, dispy);

            glTexCoord2i (w, py+th);
            glVertex2i (left+w, dispy + th);

            glTexCoord2i (0, py+th);
            glVertex2i (left, dispy + th);
        }
        glEnd ();

        h -= th;
        py = 0;
        dispy += th;
        slicenum += 1;
    }

    showsel (page, yoff);
    if (hlinks) highlightlinks (page, yoff);
    glDisable (GL_TEXTURE_RECTANGLE_ARB);

    CAMLreturn (Val_unit);
}

static pdf_link *getlink (struct page *page, int x, int y)
{
    fz_point p;
    fz_matrix ctm;
    pdf_link *link;

    p.x = x + page->pixmap->x;
    p.y = y + page->pixmap->y;

    ctm = fz_invert_matrix (page->pagedim.ctm);
    p = fz_transform_point (ctm, p);

    for (link = page->drawpage->links; link; link = link->next) {
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
    if (!page->text) {
        fz_error error;
        fz_device *tdev;

        page->text = fz_new_text_span ();
        tdev = fz_new_text_device (page->text);
        error = pdf_run_page (state.xref, page->drawpage, tdev,
                             page->pagedim.ctm);
        if (error) die (error);
        fz_free_device (tdev);
    }
}

CAMLprim value ml_whatsunder (value ptr_v, value x_v, value y_v)
{
    CAMLparam3 (ptr_v, x_v, y_v);
    CAMLlocal3 (ret_v, tup_v, str_v);
    pdf_link *link;
    struct page *page;
    char *s = String_val (ptr_v);

    ret_v = Val_int (0);
    if (trylock ("ml_whatsunder")) {
        goto done;
    }

    page = parse_pointer ("ml_whatsunder", s);
    link = getlink (page, Int_val (x_v), Int_val (y_v));
    if (link) {
        switch (link->kind) {
        case PDF_LINK_GOTO:
            {
                int pageno;
                fz_point p;
                fz_obj *obj;

                pageno = -1;
                p.x = 0;
                p.y = 0;

                if (fz_is_array (link->dest)) {
                    obj = fz_array_get (link->dest, 0);
                    if (fz_is_indirect (obj)) {
                        pageno = pdf_find_page_number (state.xref, obj);
                    }
                    else if (fz_is_int (obj)) {
                        pageno = fz_to_int (obj);
                    }

                    if (fz_array_len (link->dest) > 3) {
                        fz_obj *xo, *yo;

                        xo = fz_array_get (link->dest, 2);
                        yo = fz_array_get (link->dest, 3);
                        if (!fz_is_null (xo) && !fz_is_null (yo)) {
                            p.x = fz_to_int (xo);
                            p.y = fz_to_int (yo);
                            p = fz_transform_point (page->pagedim.ctm, p);
                        }
                    }
                }
                else {
                    pageno = pdf_find_page_number (state.xref, link->dest);
                }
                tup_v = caml_alloc_tuple (2);
                ret_v = caml_alloc_small (1, 1);
                Field (tup_v, 0) = Val_int (pageno);
                Field (tup_v, 1) = Val_int (p.y);
                Field (ret_v, 0) = tup_v;
            }
            break;

        case PDF_LINK_URI:
            str_v = caml_copy_string (fz_to_str_buf (link->dest));
            ret_v = caml_alloc_small (1, 0);
            Field (ret_v, 0) = str_v;
            break;

        default:
            printd (state.sock, "T unhandled link kind %d", link->kind);
            break;
        }
    }
    else {
        int i, x, y;
        fz_text_span *span;

        ensuretext (page);
        x = Int_val (x_v) + page->pixmap->x;
        y = Int_val (y_v) + page->pixmap->y;

        for (span = page->text; span; span = span->next) {
            for (i = 0; i < span->len; ++i) {
                fz_bbox *b;
                b = &span->text[i].bbox;
                if ((x >= b->x0 && x <= b->x1 && y >= b->y0 && y <= b->y1)) {
                    const char *n2 =
                        span->font && span->font->name
                        ? span->font->name
                        : "Span has no font name"
                        ;
#ifdef FT_FREETYPE_H
                    FT_FaceRec *face = span->font->ft_face;
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
#else
                    str_v = caml_copy_string (n2);
#endif
                    ret_v = caml_alloc_small (1, 2);
                    Field (ret_v, 0) = str_v;
                    goto unlock;
                }
            }
        }
    }
 unlock:
    unlock ("ml_whatsunder");

 done:
    CAMLreturn (ret_v);
}

CAMLprim value ml_seltext (value ptr_v, value rect_v, value oy_v)
{
    CAMLparam4 (ptr_v, rect_v, oy_v, rect_v);
    fz_bbox *b;
    struct page *page;
    fz_text_span *span;
    struct mark first, last;
    int i, x0, x1, y0, y1, oy, left;
    char *s = String_val (ptr_v);

    if (trylock ("ml_seltext")) {
        goto done;
    }

    page = parse_pointer ("ml_seltext", s);
    ensuretext (page);

    oy = Int_val (oy_v);
    x0 = Int_val (Field (rect_v, 0));
    y0 = Int_val (Field (rect_v, 1));
    x1 = Int_val (Field (rect_v, 2));
    y1 = Int_val (Field (rect_v, 3));

    left = page->pagedim.left;
    if (0) {
        glPolygonMode (GL_FRONT_AND_BACK, GL_LINE);
        glColor3ub (128, 128, 128);
        glRecti (x0, y0, x1, y1);
        glPolygonMode (GL_FRONT_AND_BACK, GL_FILL);
    }

    x0 += page->pixmap->x - left;
    y0 += page->pixmap->y - oy;
    x1 += page->pixmap->x - left;
    y1 += page->pixmap->y - oy;

    first.span = NULL;
    last.span = NULL;

    last.i = first.i = 0;
    first.span = page->text;
    for (span = page->text; span; span = span->next) {
        for (i = 0; i < span->len; ++i) {
            b = &span->text[i].bbox;
            int selected = 0;

            if (x0 >= b->x0 && x0 <= b->x1 && y0 >= b->y0 && y0 <= b->y1) {
                first.i = i;
                first.span = span;
                selected = 1;
            }
            if (x1 >= b->x0 && x1 <= b->x1 && y1 >= b->y0 && y1 <= b->y1) {
                last.i = i;
                last.span = span;
                selected = 1;
            }
            if (0 && selected) {
                glPolygonMode (GL_FRONT_AND_BACK, GL_LINE);
                glColor3ub (128, 128, 128);
                glRecti (b->x0+left, b->y0, b->x1+left, b->y1);
                glPolygonMode (GL_FRONT_AND_BACK, GL_FILL);
            }
        }
    }

    if (y1 < y0 || x1 < x0) {
        int swap = 0;

        if (first.span == last.span)  {
            swap = 1;
        }
        else {
            if (y1 < y0) {
                for (span = first.span; span && span != last.span;
                     span = span->next) {
                    if (span->eol) {
                        swap = 1;
                        break;
                    }
                }
            }
        }

        if (swap) {
            i = first.i;
            span = first.span;
            first.i = last.i;
            first.span = last.span;
            last.i = i;
            last.span = span;
        }
    }

    page->fmark = first;
    page->lmark = last;

    unlock ("ml_seltext");

 done:
    CAMLreturn (Val_unit);
}

static int pipespan (FILE *f, fz_text_span *span, int a, int b)
{
    char buf[4];
    int i, len, ret;

    for (i = a; i <= b; ++i) {
        len = runetochar (buf, &span->text[i].c);
        ret = fwrite (buf, len, 1, f);

        if (ret != 1) {
            printd (state.sock, "T failed to write %d bytes ret=%d: %s",
                    len, ret, strerror (errno));
            return -1;
        }
    }
    return 0;
}

CAMLprim value ml_copysel (value ptr_v)
{
    CAMLparam1 (ptr_v);
    FILE *f;
    struct page *page;
    char *s = String_val (ptr_v);

    if (trylock ("ml_copysel")) {
        goto done;
    }

    if (!*s)  {
    close:
#ifdef USE_XSEL
        if (state.xselpipe) {
            int ret = pclose (state.xselpipe);
            if (ret)  {
                printd (state.sock, "T failed to close xsel pipe `%s'",
                        strerror (errno));
            }
            state.xselpipe = NULL;
        }
#else
        printf ("========================================\n");
#endif
    }
    else {
        fz_text_span *span;

        page = parse_pointer ("ml_sopysel", s);

        if (!page->fmark.span || !page->lmark.span) {
            printd (state.sock, "T nothing to copy");
            goto unlock;
        }

        f = stdout;
#ifdef USE_XSEL
        if (!state.xselpipe) {
            state.xselpipe = popen ("xsel -i", "w");
            if (!state.xselpipe) {
                printd (state.sock, "T failed to open xsel pipe `%s'",
                        strerror (errno));
            }
            else {
                f = state.xselpipe;
            }
        }
        else  {
            f = state.xselpipe;
        }
#endif

        for (span = page->fmark.span;
             span && span != page->lmark.span->next;
             span = span->next) {
            int a = span == page->fmark.span ? page->fmark.i : 0;
            int b = span == page->lmark.span ? page->lmark.i : span->len - 1;
            if (pipespan (f, span, a, b))  {
                goto close;
            }
            if (span->eol)  {
                if (putc ('\n', f) == EOF) {
                    printd (state.sock, "T failed break line on xsel pipe `%s'",
                            strerror (errno));
                    goto close;
                }
            }
        }
        page->lmark.span = NULL;
        page->fmark.span = NULL;
    }

 unlock:
    unlock ("ml_copysel");

 done:
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
        box = state.pagedims[pagedimno].box;
        unlock ("ml_getpdimrect");
    }

    Store_double_field (ret_v, 0, box.x0);
    Store_double_field (ret_v, 1, box.x1);
    Store_double_field (ret_v, 2, box.y0);
    Store_double_field (ret_v, 3, box.y1);

    CAMLreturn (ret_v);
}

CAMLprim value ml_zoom_for_height (value winw_v, value winh_v, value dw_v)
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
    double pw, ph, num, den;

    if (trylock ("ml_zoom_for_height")) {
        goto done;
    }

    if (state.proportional) {
        for (i = 0, p = state.pagedims; i < state.pagedimcount; ++i, ++p) {
            double x0, x1, w;

            x0 = MIN (p->box.x0, p->box.x1);
            x1 = MAX (p->box.x0, p->box.x1);

            w = x1 - x0;
            maxw = MAX (w, maxw);
        }
    }

    for (i = 0, p = state.pagedims; i < state.pagedimcount; ++i, ++p) {
        double x0, x1, y0, y1, w, h, scaledh, scale;

        x0 = MIN (p->box.x0, p->box.x1);
        y0 = MIN (p->box.y0, p->box.y1);
        x1 = MAX (p->box.x0, p->box.x1);
        y1 = MAX (p->box.y0, p->box.y1);

        w = x1 - x0;
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

CAMLprim value ml_init (value sock_v)
{
#ifndef _WIN32
    int ret;
#endif
    CAMLparam1 (sock_v);

    state.texcount = 256;
    state.sliceheight = 24;
    state.texform = GL_RGBA;
    state.texty = GL_UNSIGNED_BYTE;

    fz_accelerate ();
    state.texids = calloc (state.texcount * sizeof (*state.texids), 1);
    if (!state.texids) {
        err (1, "calloc texids " FMT_s,
             state.texcount * sizeof (*state.texids));
    }

    state.texowners = calloc (state.texcount * sizeof (*state.texowners), 1);
    if (!state.texowners) {
        err (1, "calloc texowners " FMT_s,
             state.texcount * sizeof (*state.texowners));
    }

    glGenTextures (state.texcount, state.texids);

#ifdef _WIN32
    state.sock = Socket_val (sock_v);
#else
    state.sock = Int_val (sock_v);
#endif

#ifdef _WIN32
    InitializeCriticalSection (&critsec);
    state.thread = CreateThread (NULL, 0, mainloop, NULL, 0, NULL);
    if (state.thread == INVALID_HANDLE_VALUE) {
        errx (1, "CreateThread failed: %lx", GetLastError ());
    }
#else
    ret = pthread_create (&state.thread, NULL, mainloop, NULL);
    if (ret) {
        errx (1, "pthread_create: %s", strerror (ret));
    }
#endif

    CAMLreturn (Val_unit);
}

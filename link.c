/* lot's of code c&p-ed directly from mupdf */
#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <winsock2.h>
#define ssize_t int
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
    int errcode;

    errcode = errno;
    va_start (ap, fmt);
    vfprintf (stderr, fmt, ap);
    va_end (ap);
    fputc ('\n', stderr);
    exit (exitcode);
}
static void __declspec (noreturn) sockerr (int exitcode, const char *fmt, ...)
{
    va_list ap;
    int errcode;

    errcode = errno;
    va_start (ap, fmt);
    vfprintf (stderr, fmt, ap);
    va_end (ap);
    fprintf (stderr, ": wsaerror %lx\n", WSAGetLastError ());
    exit (exitcode);
}
#else
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
#ifndef _WIN32
#include <pthread.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/select.h>
#endif

/* fugly as hell and GCC specific but... */
#ifdef _BIG_ENDIAN
#define GL_GLEXT_PROTOTYPES
#endif

#include <GL/gl.h>
#ifndef _WIN32
#include <GL/glext.h>
#else
#define GL_TEXTURE_RECTANGLE_ARB          0x84F5
#define GL_FRAGMENT_SHADER_ATI            0x8920
#define GL_UNSIGNED_INT_8_8_8_8           0x8035
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

struct page {
    int pageno;
    int slicecount;
    fz_textspan *text;
    fz_pixmap *pixmap;
    pdf_page *drawpage;
    struct pagedim *pagedim;
    struct page *prev;
    struct slice slices[];
};

struct pagedim {
    int pageno;
    int rotate;
    fz_rect box;
    fz_bbox bbox;
    fz_matrix ctm;
};

struct {
    int sock;
    int sliceheight;
    struct page *pages;
    struct pagedim *pagedims;
    int pagecount;
    int pagedimcount;
    pdf_xref *xref;
    fz_glyphcache *cache;
    int w, h;

    int useatifs;

    int texindex;
    int texcount;
    GLuint *texids;

    GLenum texform;
    GLenum texty;

    int lotsamemory;

    int *pagetbl;
    struct {
        int w, h;
        struct slice *slice;
    } *texowners;

#ifdef _WIN32
    HANDLE thread;
#else
    pthread_t thread;
#endif
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
    int n;
    fd_set s;
    struct timeval tv;
    FD_ZERO (&s);
    FD_SET (sock, &s);
    tv.tv_sec = 0;
    tv.tv_usec = 0;
    n = select (sock + 1, &s, NULL, NULL, &tv);
    if (n == 0) return 0;
    if (n == 1) return 1;
    sockerr (1, "hasdata: select error ret=%d", n);
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
        sockerr (1, "recv (req %d, ret %zd)", size, n);
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
        sockerr (1, "send %zd", n);
    }

    n = send (fd, p, size, 0);
    if (n - size) {
        if (!n) errx (1, "EOF while writing data");
        sockerr (1, "send (req %d, ret %zd)", size, n);
    }
}

static void
#ifdef __GNUC__
__attribute__ ((format (printf, 2, 3)))
#endif
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

static void die (fz_error error)
{
    fz_catch (error, "aborting");
    if (state.xref)
       pdf_closexref (state.xref);
    exit (1);
}

static void openxref (char *filename)
{
    int fd;
    fz_error error;
    fz_stream *file;

    fd = open (filename, O_BINARY | O_RDONLY, 0666);
    if (fd < 0)
        die (fz_throw ("cannot open file '%s'", filename));

    file = fz_openfile (fd);
    state.xref = pdf_openxref (file);
    if (!state.xref)
        die (fz_throw ("cannot open PDF file '%s'", filename));
    fz_dropstream (file);

    if (pdf_needspassword (state.xref)) {
        die (fz_throw ("password protected"));
    }

    error = pdf_loadpagetree (state.xref);
    if (error) {
        die (fz_throw ("cannot load page tree"));
    }

    state.pagecount = pdf_getpagecount (state.xref);
    state.pagetbl = stat_alloc (state.pagecount * sizeof (*state.pagetbl));
}

static int readlen (int fd)
{
    ssize_t n;
    char p[4];

    n = recv (fd, p, 4, 0);
    if (n != 4) {
        if (!n) errx (1, "EOF while reading length");
        sockerr (1, "read %zd", n);
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
    if (page->text) {
        fz_freetextspan (page->text);
    }
    if (page->drawpage) {
        pdf_freepage (page->drawpage);
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

static void *render (int pageno, int pindex)
{
    fz_error error;
    int slicecount;
    fz_obj *pageobj;
    struct page *page;
    double start, end;
    pdf_page *drawpage;
    fz_device *idev;
    struct pagedim *pagedim;

    start = now ();
    printd (state.sock, "V rendering %d", pageno);
    pdf_flushxref (state.xref, 0);

    pagedim = &state.pagedims[pindex];
    slicecount = (pagedim->bbox.y1 - pagedim->bbox.y0
                  + state.sliceheight - 1) / state.sliceheight;
    slicecount += slicecount == 0;

    page = calloc (sizeof (*page)
                   + (slicecount * sizeof (struct slice)), 1);
    if (!page) {
        err (1, "calloc page %d\n", pageno);
    }
    page->slicecount = slicecount;
    page->prev = state.pages;
    state.pages = page;

    pageobj = pdf_getpageobject (state.xref, pageno);
    if (!pageobj)
        die (fz_throw ("cannot retrieve info from page %d", pageno));

    error = pdf_loadpage (&drawpage, state.xref, pageobj);
    if (error)
        die (error);

    page->pixmap = fz_newpixmapwithrect (pdf_devicergb, pagedim->bbox);
    if (error)
        die (error);
    fz_clearpixmap (page->pixmap, 0xFF);

    idev = fz_newdrawdevice (state.cache, page->pixmap);
    if (!idev)
        die (fz_throw ("fz_newdrawdevice failed"));
    error = pdf_runcontentstream (idev, pagedim->ctm, state.xref,
                                  drawpage->resources,
                                  drawpage->contents);
    fz_freedevice (idev);

    page->drawpage = drawpage;
    page->pagedim = pagedim;
    page->pageno = pageno;
    subdivide (page);
    end = now ();

    if (!state.lotsamemory) {
        pdf_agestoreditems (state.xref->store);
        pdf_evictageditems (state.xref->store);
    }

    printd (state.sock, "V rendering %d took %f sec", pageno, end - start);
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

        pageobj = pdf_getpageobject (state.xref, pageno + 1);

        obj = fz_dictgets (pageobj, "CropBox");
        if (!fz_isarray (obj)) {
            obj = fz_dictgets (pageobj, "MediaBox");
            if (!fz_isarray (obj)) {
                die (fz_throw ("cannot find page bounds %d (%d Rd)",
                               fz_tonum (pageobj), fz_togen (pageobj)));
            }
        }
        box = pdf_torect (obj);

        obj = fz_dictgets (pageobj, "Rotate");
        if (fz_isint (obj)) {
            rotate = fz_toint (obj);
        }
        else  {
            rotate = 0;
        }

        state.pagetbl[pageno] = fz_tonum (state.xref->pagerefs[pageno]);
        p = &state.pagedims[state.pagedimcount - 1];
        if ((state.pagedimcount == 0)
            || (p->rotate != rotate || memcmp (&p->box, &box, sizeof (box)))) {
            size_t size;

            size = (state.pagedimcount + 1) * sizeof (*state.pagedims);
            state.pagedims = realloc (state.pagedims, size);
            if (!state.pagedims) {
                err (1, "realloc pagedims to %zu (%d elems)",
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
    double zoom, w;
    struct pagedim *p = state.pagedims;

    pindex = 0;
    for (pindex = 0; pindex < state.pagedimcount; ++pindex, ++p) {
        box.x0 = MIN (p->box.x0, p->box.x1);
        box.y0 = MIN (p->box.y0, p->box.y1);
        box.x1 = MAX (p->box.x0, p->box.x1);
        box.y1 = MAX (p->box.y0, p->box.y1);

        ctm = fz_identity ();
        ctm = fz_concat (ctm, fz_translate (0, -box.y1));
        ctm = fz_concat (ctm, fz_rotate (p->rotate));
        box2 = fz_transformrect (ctm, box);
        w = box2.x1 - box2.x0;

        zoom = (state.w / w);
        ctm = fz_identity ();
        ctm = fz_concat (ctm, fz_translate (0, -box.y1));
        ctm = fz_concat (ctm, fz_scale (zoom, -zoom));
        ctm = fz_concat (ctm, fz_rotate (p->rotate));
        p->bbox = fz_roundrect (fz_transformrect (ctm, box));
        memcpy (&p->ctm, &ctm, sizeof (ctm));
    }

    while (p-- != state.pagedims)  {
        printd (state.sock, "l %d %d %d",
                p->pageno, p->bbox.x1 - p->bbox.x0, p->bbox.y1 - p->bbox.y0);
    }
}

static void recurse_outline (pdf_outline *outline, int level)
{
    while (outline) {
        int i, num;
        fz_obj *obj;
        int top = 0;
        int pageno = -1;

        if (!outline->link) goto next;

        obj = outline->link->dest;
        if (fz_isindirect (obj)) {
            num = fz_tonum (obj);

            for (i = 0; i < state.pagecount; ++i)  {
                if (state.pagetbl[i] == num) {
                    pageno = i;
                    break;
                }
            }
        }
        else if (fz_isarray (obj)) {
            fz_obj *obj2;

            obj2 = fz_arrayget (obj, 0);
            if (fz_isint (obj2)) {
                pageno = fz_toint (obj2);
            }
            else {
                num = fz_tonum (obj2);
                for (i = 0; i < state.pagecount; ++i)  {
                    if (state.pagetbl[i] == num) {
                        pageno = i;
                        break;
                    }
                }
            }

            if (fz_arraylen (obj) > 3) {
                fz_point p;
                struct pagedim *pagedim = state.pagedims;

                for (i = 0; i < state.pagedimcount; ++i) {
                    if (state.pagedims[i].pageno > pageno)
                        break;
                    pagedim = &state.pagedims[i];
                }

                p.x = fz_toint (fz_arrayget (obj, 2));
                p.y = fz_toint (fz_arrayget (obj, 3));
                p = fz_transformpoint (pagedim->ctm, p);
                top = p.y;
            }
        }

        lprintf ("%*c%s %d\n", level, ' ', outline->title, pageno);
        printd (state.sock, "o %d %d %d %s",
                level, pageno, top, outline->title);
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

    outline = pdf_loadoutline (state.xref);
    if (outline) {
        recurse_outline (outline, 0);
        pdf_freeoutline (outline);
    }
}

static int comparespans (const void *l, const void *r)
{
#ifdef _MSC_VER
    fz_textspan const**ls = l;
    fz_textspan const**rs = r;
#else
    fz_textspan *const*ls = l;
    fz_textspan *const*rs = r;
#endif
    return (*ls)->text->bbox.y0 - (*rs)->text->bbox.y0;
}

/* wishful thinking function */
static void search (regex_t *re, int pageno, int y, int forward)
{
    int i, j;
    int ret;
    char *p;
    char buf[256];
    fz_error error;
    fz_obj *pageobj;
    fz_device *tdev;
    pdf_page *drawpage;
    fz_textspan *text, *span, **pspan;
    struct pagedim *pdim, *pdimprev;
    int stop = 0;
    int niters = 0;
    int nspans;
    double start, end;

    start = now ();
    while (pageno >= 0 && pageno < state.pagecount && !stop) {
        if (niters++ == 5) {
            if (!state.lotsamemory) {
                pdf_agestoreditems (state.xref->store);
                pdf_evictageditems (state.xref->store);
            }
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

        pageobj = pdf_getpageobject (state.xref, pageno + 1);
        if (!pageobj)
            die (fz_throw ("cannot retrieve info from page %d", pageno));

        error = pdf_loadpage (&drawpage, state.xref, pageobj);
        if (error)
            die (error);

        text = fz_newtextspan ();
        tdev = fz_newtextdevice (text);
        error = pdf_runcontentstream (tdev, pdim->ctm, state.xref,
                                      drawpage->resources,
                                      drawpage->contents);
        if (error) die (error);
        fz_freedevice (tdev);

        nspans = 0;
        for (span = text; span; span = span->next) {
            nspans++;
        }
        pspan = malloc (sizeof (void *) * nspans);
        if (!pspan) {
            err (1, "malloc span pointers %zu", sizeof (void *) * nspans);
        }
        for (i = 0, span = text; span; span = span->next, ++i) {
            pspan[i] = span;
        }
        qsort (pspan, nspans, sizeof (fz_textspan *), comparespans);

        j = forward ? 0 : nspans - 1;
        while (nspans--) {
            regmatch_t rm;

            span = pspan[j];
            j += forward ? 1 : -1;
            p = buf;
            /* XXX: spans are not sorted "visually" */
            for (i = 0; i < MIN (span->len, sizeof (buf) - 1); ++i) {
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
                    fz_freetextspan (text);
                    pdf_freepage (drawpage);
                    free (pspan);
                    return;
                }
            }
            else  {
                fz_rect r;

                r.x0 = span->text[rm.rm_so].bbox.x0 - pdim->bbox.x0;
                r.y0 = span->text[rm.rm_so].bbox.y0;
                r.x1 = span->text[rm.rm_eo - 1].bbox.x1 - pdim->bbox.x0;
                r.y1 = span->text[rm.rm_eo - 1].bbox.y1;

                if (!stop) {
                    printd (state.sock, "F %d %d %f %f %f %f",
                            pageno, 1,
                            r.x0, r.y0,
                            r.x1, r.y1);
                }
                else  {
                    printd (state.sock, "R %d %d %f %f %f %f",
                            pageno, 2,
                            r.x0, r.y0,
                            r.x1, r.y1);
                }
                printd (state.sock, "T found at %d `%.*s' %f in %f sec",
                        pageno, rm.rm_eo - rm.rm_so, &buf[rm.rm_so],
                        span->text[0].bbox.y0 - drawpage->mediabox.y0,
                        now () - start);
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
        fz_freetextspan (text);
        pdf_freepage (drawpage);
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
            char *filename = p + 5;

            openxref (filename);
            initpdims ();

            obj = fz_dictgets (state.xref->trailer, "Info");
            if (obj) {
                obj = fz_dictgets (obj, "Title");
                if (obj) {
                    printd (state.sock, "t %s", pdf_toutf8 (obj));
                }
            }
        }
        else if (!strncmp ("free", p, 4)) {
            void *ptr;

            ret = sscanf (p + 4, " %p", &ptr);
            if (ret != 1) {
                errx (1, "malformed free `%.*s' ret=%d", len, p, ret);
            }
            lock ("free");
            freepage (ptr);
            unlock ("free");
            printd (state.sock, "d");
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
            state.h = h;
            if (w != state.w) {
                int i;
                state.w = w;
                for (i = 0; i < state.texcount; ++i)  {
                    state.texowners[i].slice = NULL;
                }
            }
            lock ("geometry");
            layout ();
            process_outline ();
            unlock ("geometry");
            printd (state.sock, "C %d", state.pagecount);
        }
        else if (!strncmp ("render", p, 6)) {
            int pageno, pindex, w, h, ret;
            struct page *page;

            ret = sscanf (p + 6, " %d %d %d %d", &pageno, &pindex, &w, &h);
            if (ret != 4) {
                errx (1, "bad render line `%.*s' ret=%d", len, p, ret);
            }

            page = render (pageno, pindex);
            printd (state.sock, "r %d %d %d %p\n",
                    pageno,
                    state.w,
                    state.h,
                    page);
        }
        else {
            errx (1, "unknown command %.*s", len, p);
        }
    }
    return 0;
}

static void upload2 (struct page *page, int slicenum, const char *cap)
{
    int i;
    int w, h;
    double start, end;
    struct slice *slice = &page->slices[slicenum];

    w = page->pixmap->w;
    h = page->pixmap->h;

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
        lprintf ("%s[%d] slice=%d(%d,%d) texid=%d %f sec\n",
                 subimage ? "sub" : "img",
                 page->pageno, slicenum,
                 slice->w, slice->h,
                 state.texids[slice->texindex],
                 end - start);
    }
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
    int ret;
    void *ptr;
    struct page *page;
    int slicenum = 0;

    if (trylock ("ml_draw")) {
        goto done;
    }

    ret = sscanf (s, "%p", &ptr);
    if (ret != 1) {
        errx (1, "cannot parse pointer `%s'", s);
    }
    page = ptr;

    w = page->pixmap->w;

    ARSERT (h >= 0 && "ml_draw wrong h");

    glEnable (GL_TEXTURE_RECTANGLE_ARB);
    if (state.useatifs) {
        glEnable (GL_FRAGMENT_SHADER_ATI);
    }

    for (slicenum = 0; slicenum < page->slicecount; ++slicenum) {
        struct slice *slice = &page->slices[slicenum];
        if (slice->h > py) {
            break;
        }
        py -= slice->h;
    }

    h = MIN (state.h, h);
    while (h) {
        int th;
        struct slice *slice = &page->slices[slicenum];

        ARSERT (slicenum < page->slicecount && "ml_draw wrong slicenum");

        th = MIN (h, slice->h - py);
        upload2 (page, slicenum, "upload");

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
        slicenum += 1;
    }

    glDisable (GL_TEXTURE_RECTANGLE_ARB);
    if (state.useatifs) {
        glDisable (GL_FRAGMENT_SHADER_ATI);
    }

    unlock ("ml_draw");
 done:
    CAMLreturn (Val_unit);
}

static pdf_link *getlink (struct page *page, int x, int y)
{
    fz_point p;
    fz_matrix ctm;
    pdf_link *link;

    p.x = x;
    p.y = y;

    ctm = fz_invertmatrix (page->pagedim->ctm);
    p = fz_transformpoint (ctm, p);

    for (link = page->drawpage->links; link; link = link->next) {
        if (p.x >= link->rect.x0 && p.x <= link->rect.x1) {
            if (p.y >= link->rect.y0 && p.y <= link->rect.y1) {
                if (link->kind == PDF_LGOTO) {
                    return link;
                }
            }
        }
    }
    return NULL;
}

CAMLprim value ml_checklink (value ptr_v, value x_v, value y_v)
{
    CAMLparam3 (ptr_v, x_v, y_v);
    char *s = String_val (ptr_v);
    int ret;

    if (trylock ("ml_checklink")) {
        ret = 0;
    }
    else {
        ret = NULL != getlink (parse_pointer ("ml_checklink", s),
                               Int_val (x_v), Int_val (y_v));
        unlock ("ml_checklink");
    }
    CAMLreturn (Val_bool (ret));
}

CAMLprim value ml_getlink (value ptr_v, value x_v, value y_v)
{
    CAMLparam3 (ptr_v, x_v, y_v);
    CAMLlocal2 (ret_v, tup_v);
    pdf_link *link;
    struct page *page;
    char *s = String_val (ptr_v);

    if (trylock ("ml_getlink")) {
        ret_v = Val_int (0);
        goto done;
    }

    page = parse_pointer ("ml_getlink", s);

    link = getlink (page, Int_val (x_v), Int_val (y_v));
    if (link) {
        int pageno;
        fz_point p;
        fz_obj *obj;

        pageno = -1;
        obj = fz_arrayget (link->dest, 0);
        if (fz_isindirect (obj)) {
            pageno = pdf_findpageobject (state.xref, obj) - 1;
        }
        else if (fz_isint (obj)) {
            pageno = fz_toint (obj);
        }

        if (fz_arraylen (link->dest) > 3) {
            p.x = fz_toint (fz_arrayget (link->dest, 2));
            p.y = fz_toint (fz_arrayget (link->dest, 3));
            p = fz_transformpoint (page->pagedim->ctm, p);
        }
        else  {
            p.x = 0.0;
            p.y = 0.0;
        }

        tup_v = caml_alloc_tuple (2);
        ret_v = caml_alloc_small (1, 1);
        Field (tup_v, 0) = Val_int (pageno);
        Field (tup_v, 1) = Val_int (p.y);
        Field (ret_v, 0) = tup_v;
    }
    else {
        ret_v = Val_int (0);
    }
    unlock ("ml_getlink");

 done:
    CAMLreturn (ret_v);
}

CAMLprim value ml_gettext (value ptr_v, value rect_v, value oy_v, value rectsel_v)
{
    CAMLparam4 (ptr_v, rect_v, oy_v, rect_v);
    fz_matrix ctm;
    fz_point p1, p2;
    struct page *page;
    fz_textspan *span;
    char *s = String_val (ptr_v);
    int rectsel = Bool_val (rectsel_v);
    int i, bx0, bx1, by0, by1, x0, x1, y0, y1, oy;

    /* stop GCC from complaining about uninitialized variables */
#ifdef __GNUC__
    int rx0 = rx0, rx1 = rx1, ry0 = ry0, ry1 = ry1;
#else
    int rx0, rx1, ry0, ry1;
#endif

    if (trylock ("ml_gettext")) {
        goto done;
    }

    page = parse_pointer ("ml_gettext", s);

    oy = Int_val (oy_v);
    p1.x = Int_val (Field (rect_v, 0));
    p1.y = Int_val (Field (rect_v, 1));
    p2.x = Int_val (Field (rect_v, 2));
    p2.y = Int_val (Field (rect_v, 3));

    if (0) {
        glEnable (GL_BLEND);
        glPolygonMode (GL_FRONT_AND_BACK, GL_LINE);
        glBlendFunc (GL_DST_ALPHA, GL_SRC_ALPHA);
        glColor4f (0, 0, 0, 0.2);
        glRecti (p1.x, p1.y, p2.x, p2.y);
        glPolygonMode (GL_FRONT_AND_BACK, GL_FILL);
        glDisable (GL_BLEND);
    }

    ctm = page->pagedim->ctm;
    if (!page->text) {
        fz_error error;
        fz_device *tdev;

        page->text = fz_newtextspan ();
        tdev = fz_newtextdevice (page->text);
        error = pdf_runcontentstream (tdev, page->pagedim->ctm, state.xref,
                                      page->drawpage->resources,
                                      page->drawpage->contents);
        if (error) die (error);
        fz_freedevice (tdev);
    }

    printf ("\033c");

    printf ("BBox %f %f %f %f\n", p1.x, p1.y, p2.x, p2.y);
    p1.x += page->pixmap->x;
    p1.y += page->pixmap->y;
    p2.x += page->pixmap->x;
    p2.y += page->pixmap->y;
    x0 = p1.x;
    y0 = p1.y;
    x1 = p2.x;
    y1 = p2.y;
    printf ("BBox %d %d %d %d %d %d\n", x0, y0, x1, y1, oy, page->pageno);

    for (span = page->text; span; span = span->next) {
        int seen = 0;

        /* fz_debugtextspanxml (span); */
        for (i = 0; i < span->len; ++i) {
            long c;

            bx0 = span->text[i].bbox.x0;
            bx1 = span->text[i].bbox.x1;
            by0 = span->text[i].bbox.y0 + oy;
            by1 = span->text[i].bbox.y1 + oy;

            if ((bx1 >= x0 && bx0 <= x1 && by1 >= y0 && by0 <= y1)) {
                if (!seen) {
                    rx0 = bx0 - page->pixmap->x;
                    rx1 = bx1 - page->pixmap->x;
                    ry0 = by0;
                    ry1 = by1;
                }

                seen = 1;
                c = span->text[i].c;
                if (c < 256) {
                    if ((isprint (c) && !isspace (c))) {
                        if (!rectsel) {
                            bx0 -= page->pixmap->x;
                            bx1 -= page->pixmap->x;
                            glEnable (GL_BLEND);
                            glPolygonMode (GL_FRONT_AND_BACK, GL_FILL);
                            glBlendFunc (GL_DST_ALPHA, GL_SRC_ALPHA);
                            glColor4f (0.5, 0.5, 0.0, 0.6);
                            glRecti (bx0, by0, bx1, by1);
                            glPolygonMode (GL_FRONT_AND_BACK, GL_FILL);
                            glDisable (GL_BLEND);
                        }
                        if (isprint (c) || c ==' ') {
                            rx1 = bx1;
                            ry1 = by1;
                        }
                    }
                    putc (c, stdout);
                }
                else  {
                    putc ('?', stdout);
                }
            }
        }

        if (rectsel) {
            if (seen) {
                glEnable (GL_BLEND);
                glPolygonMode (GL_FRONT_AND_BACK, GL_FILL);
                glBlendFunc (GL_DST_ALPHA, GL_SRC_ALPHA);
                glColor4f (0.5, 0.5, 0.0, 0.6);
                glRecti (rx0, ry0, rx1, ry1);
                glPolygonMode (GL_FRONT_AND_BACK, GL_FILL);
                glDisable (GL_BLEND);
            }
        }

        if (seen && span->eol) {
            x0 = page->pixmap->x;
            putc ('\n', stdout);
        }
    }
    unlock ("ml_gettext");

 done:
    CAMLreturn (Val_unit);
}

CAMLprim value ml_getpagewh (value pagedimno_v)
{
    CAMLparam1 (pagedimno_v);
    CAMLlocal1 (ret_v);
    int pagedimno = Int_val (pagedimno_v);

    ret_v = caml_alloc_small (4 * Double_wosize, Double_array_tag);
    Store_double_field (ret_v, 0, state.pagedims[pagedimno].box.x0);
    Store_double_field (ret_v, 1, state.pagedims[pagedimno].box.x1);
    Store_double_field (ret_v, 2, state.pagedims[pagedimno].box.y0);
    Store_double_field (ret_v, 3, state.pagedims[pagedimno].box.y1);
    CAMLreturn (ret_v);
}

static void initgl (void)
{
#ifdef _BIG_ENDIAN
    if (strstr ((char *) glGetString (GL_EXTENSIONS),
                "GL_ATI_fragment_shader")) {
        /* Here, with MESA, rv280, powerpc32: BGRA(rev) is slow while
           ABGR is fast, so fix things in the shader */
        state.texform = GL_ABGR_EXT;
        state.texty = GL_UNSIGNED_INT_8_8_8_8;

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
        state.useatifs = 1;
    }
    else {
        state.texform = GL_BGRA_EXT;
        state.texty = GL_UNSIGNED_INT_8_8_8_8_REV;
    }
#else
    state.texform = GL_BGRA_EXT;
    state.texty = GL_UNSIGNED_INT_8_8_8_8;
#endif
}

CAMLprim value ml_init (value sock_v)
{
    int ret;
    CAMLparam1 (sock_v);

    state.texcount = 256;
    state.sliceheight = 24;

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

#ifdef _WIN32
    state.sock = Socket_val (sock_v);
#else
    state.sock = Int_val (sock_v);
#endif
    initgl ();

    state.cache = fz_newglyphcache ();
    if (!state.cache) {
        errx (1, "fz_newglyphcache failed");
    }

#ifdef _WIN32
    InitializeCriticalSection (&critsec);
    state.thread = CreateThread (NULL, 0, mainloop, NULL, 0, NULL);
    if (state.thread == INVALID_HANDLE_VALUE) {
        errx (1, "CreateThread failed: %lx", GetLastError ());
    }
#else
    ret = pthread_create (&state.thread, NULL, mainloop, NULL);
    if (ret) {
        errx (1, "pthread_create: %s", strerror (errno));
    }
#endif

    CAMLreturn (Val_unit);
}

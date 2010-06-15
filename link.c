/* lot's of code c&p-ed directly from mupdf */

#define _GNU_SOURCE
#include <err.h>
#include <regex.h>
#include <errno.h>
#include <ctype.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <sys/poll.h>
#include <sys/time.h>

/* fugly as hell and GCC specific but... */
#ifdef _BIG_ENDIAN
#define GL_GLEXT_PROTOTYPES
#endif

#include <GL/gl.h>
#include <GL/glext.h>

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
    pthread_t thread;
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
} state;

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

    ret = sscanf (s, "%p", &ptr);
    if (ret != 1) {
        errx (1, "%s: cannot parse pointer in `%s'", cap, s);
    }
    return ptr;
}

static int hasdata (int sock)
{
    int ret;
    struct pollfd pfd;

    pfd.fd = sock;
    pfd.events = POLLIN;
    ret = poll (&pfd, 1, 0);
    if (ret == 0) {
        return 0;
    }
    if (ret != 1) {
        err (1, "poll");
    }
    return pfd.revents & POLLIN;
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

    state.pagecount = pdf_getpagecount (state.xref);
    state.pagetbl = stat_alloc (state.pagecount * sizeof (*state.pagetbl));
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
        pdf_droppage (page->drawpage);
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
    fz_displaylist *list;
    fz_device *idev, *mdev;
    struct pagedim *pagedim;

    start = now ();
    /* printd (state.sock, "T rendering %d", pageno); */
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

    list = fz_newdisplaylist ();
    if (!list)
        die (fz_throw ("fz_newdisplaylist failed"));

    mdev = fz_newlistdevice (list);
    error = pdf_runcontentstream (mdev, fz_identity (), state.xref,
                                  drawpage->resources,
                                  drawpage->contents);
    if (error)
        die (error);
    fz_freedevice (mdev);

    idev = fz_newdrawdevice (state.cache, page->pixmap);
    if (!idev)
        die (fz_throw ("fz_newdrawdevice failed"));
    fz_executedisplaylist (list, idev, pagedim->ctm);
    fz_freedevice (idev);

    fz_freedisplaylist (list);

    page->drawpage = drawpage;
    page->pagedim = pagedim;
    page->pageno = pageno;
    subdivide (page);
    end = now ();

    if (!state.lotsamemory) {
        pdf_agestoreditems (state.xref->store);
        pdf_evictageditems (state.xref->store);
    }

    /* printd (state.sock, "T rendering %d took %f sec", pageno, end - start); */
    return page;
}

/* almost verbatim copy of pdf_getpagecountimp */
struct stuff
{
    fz_obj *resources;
    fz_obj *mediabox;
    fz_obj *cropbox;
    fz_obj *rotate;
};

static void
recurse_page (fz_obj *node, int bias, int *pagesp, struct stuff inherit)
{
    fz_obj *type;
    fz_obj *kids;
    fz_obj *count;
    char *typestr;
    int pages = 0;
    int i;

    if (!fz_isdict(node))
    {
        fz_warn("pagetree node is missing, igoring missing pages...");
        return;
    }

    type = fz_dictgets(node, "Type");
    kids = fz_dictgets(node, "Kids");
    count = fz_dictgets(node, "Count");

    if (fz_isname(type))
        typestr = fz_toname(type);
    else
    {
        fz_warn("pagetree node (%d %d R) lacks required type", fz_tonum(node), fz_togen(node));

        kids = fz_dictgets(node, "Kids");
        if (kids)
        {
            fz_warn("guessing it may be a pagetree node, continuing...");
            typestr = "Pages";
        }
        else
        {
            fz_warn("guessing it may be a page, continuing...");
            typestr = "Page";
        }
    }

    if (!strcmp(typestr, "Page")) {
        int rotate;
        fz_obj *obj;
        fz_rect box;
        struct pagedim *p;
        int pageno = *pagesp;

        state.pagetbl[pageno + bias] = fz_tonum (node);

        obj = fz_dictgets (node, "CropBox");
        if (!obj) obj = inherit.cropbox;
        if (!fz_isarray (obj)) {
            obj = fz_dictgets (node, "MediaBox");
            if (!obj) obj = inherit.mediabox;

            if (!fz_isarray (obj)) {
                die (fz_throw ("cannot find page bounds %d (%d Rd)",
                               fz_tonum (node), fz_togen (node)));
            }
        }
        box = pdf_torect (obj);

        obj = fz_dictgets (node, "Rotate");
        if (!obj) obj = inherit.rotate;
        if (fz_isint (obj)) {
            rotate = fz_toint (obj);
        }
        else  {
            rotate = 0;
        }

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
            p->pageno = pageno + bias;
        }
        (*pagesp)++;
    }
    else if (!strcmp(typestr, "Pages"))
    {
        fz_obj *inh;

        if (!fz_isarray(kids))
            fz_warn("page tree node contains no pages");

        pdf_logpage("subtree (%d %d R) {\n", fz_tonum(node), fz_togen(node));

        inh = fz_dictgets(node, "Resources");
        if (inh) inherit.resources = inh;

        inh = fz_dictgets(node, "MediaBox");
        if (inh) inherit.mediabox = inh;

        inh = fz_dictgets(node, "CropBox");
        if (inh) inherit.cropbox = inh;

        inh = fz_dictgets(node, "Rotate");
        if (inh) inherit.rotate = inh;

        for (i = 0; i < fz_arraylen(kids); i++)
        {
            fz_obj *obj = fz_arrayget(kids, i);

            /* prevent infinite recursion possible in maliciously crafted PDFs */
            if (obj == node)
            {
                fz_warn("cyclic page tree");
                return;
            }

            recurse_page (obj, *pagesp + bias, &pages, inherit);
        }

        if (pages != fz_toint(count))
        {
            fz_warn("page tree node contains incorrect number of pages, continuing...");
            count = fz_newint(pages);
            fz_dictputs(node, "Count", count);
            fz_dropobj(count);
        }

        pdf_logpage("%d pages\n", pages);

        (*pagesp) += pages;

        pdf_logpage("}\n");
    }
}

static void initpdims (void)
{
    fz_obj *catalog;
    fz_obj *pages;
    int count;
    double start, end;
    struct stuff inherit;

    start = now ();
    catalog = fz_dictgets (state.xref->trailer, "Root");
    pages = fz_dictgets (catalog, "Pages");

    inherit.resources = nil;
    inherit.mediabox = nil;
    inherit.cropbox = nil;
    inherit.rotate = nil;

    count = 0;
    recurse_page (pages, 0, &count, inherit);
    end = now ();
    printd (state.sock, "T Processed %d pages in %f seconds",
            count, end - start);
}

static void layout (void)
{
    int pindex;
    fz_matrix ctm;
    fz_rect box, box2;
    double zoom, w;
    struct pagedim *p = state.pagedims;

    pindex = 0;
    printd (state.sock, "c");
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

    printd (state.sock, "C %d", state.pagecount);
}

static void recurse_outline (pdf_outline *outline, int level)
{
    while (outline) {
        fz_obj *obj;
        int top = 0;
        int pageno = -1;

        if (!outline->link) goto next;

        obj = outline->link->dest;
        if (fz_isarray (obj)) {
            int i;
            int num;
            fz_obj *obj2;
            struct pagedim *pagedim = state.pagedims;

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

            for (i = 0; i < state.pagedimcount; ++i) {
                if (state.pagedims[i].pageno > pageno)
                    break;
                pagedim = &state.pagedims[i];
            }

            if (fz_arraylen (obj) > 3) {
                fz_point p;

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
        pdf_dropoutline (outline);
    }
}

static int comparespans (const void *l, const void *r)
{
    fz_textspan *const*ls = l;
    fz_textspan *const*rs = r;

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
                    pdf_droppage (drawpage);
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
        pdf_droppage (drawpage);
        free (pspan);
    }
    end = now ();
    if (!stop)  {
        printd (state.sock, "T no matches %f sec", end - start);
    }
    printd (state.sock, "D");
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

            openxref (filename);
            initpdims ();
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
            printd (state.sock, "d");
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
    return NULL;
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
                    printf ("\e[0;31mERROR1 %d %d %#x\e[0m\n", w, slice->h, err);
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
                    printf ("\e[0;31mERROR %d %d %#x\e[0m\n", w, slice->h, err);
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

CAMLprim value ml_preload (value ptr_v)
{
    int i;
    int ret;
    void *ptr;
    CAMLparam1 (ptr_v);
    char *s = String_val (ptr_v);
    struct page *page;

    if (trylock ("ml_preload")) {
        goto done;
    }
    ret = sscanf (s, "%p", &ptr);
    if (ret != 1) {
        errx (1, "cannot parse pointer `%s'", s);
    }

    page = ptr;
    for (i = 0; i < page->slicecount; ++i) {
        upload2 (ptr, i, "preload");
    }

    unlock ("ml_preload");
 done:
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

    if (trylock ("ml_gettext")) {
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
    int rx0 = rx0, rx1 = rx1, ry0 = ry0, ry1 = ry1;

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

    printf ("\ec");

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

    state.texcount = 128;
    state.sliceheight = 64;

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
    if (!state.cache) {
        errx (1, "fz_newglyphcache failed");
    }

    ret = pthread_create (&state.thread, NULL, mainloop, NULL);
    if (ret) {
        errx (1, "pthread_create: %s", strerror (errno));
    }

    CAMLreturn (Val_unit);
}

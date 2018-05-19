#include <ctype.h>
#include <stdio.h>
#include <errno.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <inttypes.h>
#include <sys/time.h>
#include <sys/ioctl.h>

#include "cutils.h"

void NORETURN_ATTR GCC_FMT_ATTR (2, 3) err (int exitcode, const char *fmt, ...)
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

void NORETURN_ATTR GCC_FMT_ATTR (2, 3) errx (int exitcode, const char *fmt, ...)
{
    va_list ap;

    va_start (ap, fmt);
    vfprintf (stderr, fmt, ap);
    va_end (ap);
    fputc ('\n', stderr);
    fflush (stderr);
    _exit (exitcode);
}

void *parse_pointer (const char *cap, const char *s)
{
    int ret;
    void *ptr;

    ret = sscanf (s, "%" SCN_ptr, (uintptr_t *) &ptr);
    if (ret != 1) {
        errx (1, "%s: cannot parse pointer in `%s'", cap, s);
    }
    return ptr;
}

double now (void)
{
    struct timeval tv;
    gettimeofday (&tv, NULL);
    return tv.tv_sec + tv.tv_usec*1e-6;
}

/* slightly tweaked fmt_ulong by D.J. Bernstein */
void fmt_linkn (char *s, unsigned int u)
{
  unsigned int len; unsigned int q;
  unsigned int zma = 'z' - 'a' + 1;
  len = 1; q = u;
  while (q > zma - 1) { ++len; q /= zma; }
  if (s) {
    s += len;
    do { *--s = (char)('a' + (u % zma) - (u < zma && len > 1));
        u /= zma; } while(u);
    /* handles u == 0 */
  }
  s[len] = 0;
}

char *ystrdup (const char *s)
{
    size_t len = strlen (s);
    if (len > 0) {
        char *r = malloc (len+1);
        if (!r) errx (1, "malloc %zu", len+1);
        memcpy (r, s, len+1);
        return r;
    }
    return NULL;
}

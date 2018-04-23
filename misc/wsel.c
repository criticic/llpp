#define WIN32_LEAN_AND_MEAN
#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <windows.h>

#ifdef _MSC_VER
#define NORETURN __declspec (noreturn)
#elif defined __GNUC__
#define NORETURN __attribute__ ((noreturn))
#else
#define NORETURN
#endif

static void NORETURN winerr (int exitcode, const char *fmt, ...)
{
    va_list ap;

    va_start (ap, fmt);
    vfprintf (stderr, fmt, ap);
    va_end (ap);
    fprintf (stderr, ": winerror 0x%lx\n", GetLastError ());
    exit (exitcode);
}

int main (void)
{
    HANDLE h;
    LPWSTR tp;
    size_t n, m, k;
    char buf[4096];

    if (!OpenClipboard (NULL)) winerr (1, "OpenClipboard");
    EmptyClipboard ();

    n = fread (buf, 1, 4096, stdin);
    if (n < 0) {
        fprintf (stderr, "fread failed: %s\n", strerror (errno));
        return 2;
    }
    if (!n) return 3;

    m = MultiByteToWideChar (
        CP_UTF8,
        MB_ERR_INVALID_CHARS,
        buf,
        n,
        NULL,
        0
        );
    if (m <= 0) winerr (4, "MultiByteToWideChar %d", m);

    h = GlobalAlloc (GMEM_MOVEABLE, (m+1) * sizeof (*tp));
    if (!h) winerr (5, "GlobalAlloc");

    tp = GlobalLock (h);
    k = MultiByteToWideChar (
        CP_UTF8,
        MB_ERR_INVALID_CHARS,
        buf,
        n,
        tp,
        m
        );
    if (k != m) winerr (6, "MultiByteToWideChar %d %d", k, m);
    tp[k] = 0;
    GlobalUnlock (h);
    if (!SetClipboardData (CF_UNICODETEXT, h)) winerr (7, "SetClipboardData");
    if (GlobalFree (h)) winerr (8, "GlobalFree");
    if (!CloseClipboard ()) winerr (9, "CloseClipboard");
    return 0;
}

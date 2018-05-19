#ifndef CUTILS_H
#define CUTILS_H

#define SCN_ptr SCNxPTR
#define FMT_ptr PRIxPTR

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

extern void NORETURN_ATTR GCC_FMT_ATTR (2, 3)
    err (int exitcode, const char *fmt, ...);
extern void NORETURN_ATTR GCC_FMT_ATTR (2, 3)
    errx (int exitcode, const char *fmt, ...);
extern void *parse_pointer (const char *cap, const char *s);
extern double now (void);
extern void fmt_linkn (char *s, unsigned int u);
extern char *ystrdup (const char *s);

#endif

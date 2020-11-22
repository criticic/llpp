#ifndef CUTILS_H
#define CUTILS_H

#if defined __GNUC__
#define UNUSED_ATTR __attribute__ ((unused))
#if !defined __clang__
#define NO_OPTIMIZE_ATTR __attribute__ ((optimize ("O0")))
#else
#define NO_OPTIMIZE_ATTR __attribute__ ((optnone))
#endif
#define GCC_FMT_ATTR(a, b) __attribute__ ((format (printf, a, b)))
#else
#error Stringent C compiler requirements not satisfied
#endif

extern _Noreturn void GCC_FMT_ATTR (2, 3)
    err (int exitcode, const char *fmt, ...);
extern _Noreturn void GCC_FMT_ATTR (2, 3)
    errx (int exitcode, const char *fmt, ...);
extern void *parse_pointer (const char *cap, const char *s);
extern double now (void);
extern void fmt_linkn (char *s, const char *c, unsigned int l, int n);

#endif

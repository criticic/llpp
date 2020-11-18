#ifdef __clang__
#if __clang_major__ >= 10
#pragma GCC diagnostic error "-Weverything"
#pragma GCC diagnostic ignored "-Wpadded"
#pragma GCC diagnostic ignored "-Wsign-conversion"
#pragma GCC diagnostic ignored "-Wdocumentation-unknown-command"
#pragma GCC diagnostic ignored "-Wdocumentation"
#pragma GCC diagnostic ignored "-Wdouble-promotion"
#pragma GCC diagnostic ignored "-Wimplicit-int-float-conversion"
#endif
#else
#pragma GCC diagnostic error "-Wcast-qual"
#endif

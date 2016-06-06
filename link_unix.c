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

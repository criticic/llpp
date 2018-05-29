#define CAML_NAME_SPACE

#include <X11/Xlib.h>
#include <X11/cursorfont.h>

#include <GL/glx.h>

#include <caml/fail.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/unixsupport.h>

static const int shapes[] = {
    XC_left_ptr, XC_hand2, XC_exchange, XC_fleur, XC_xterm
};

#define CURS_COUNT (sizeof (shapes) / sizeof (shapes[0]))

static struct {
    Window wid;
    Display *dpy;
    GLXContext ctx;
    XVisualInfo *visual;
    Cursor curs[CURS_COUNT];
} glx;

static void initcurs (void)
{
    for (size_t n = 0; n < CURS_COUNT; ++n) {
        glx.curs[n] = XCreateFontCursor (glx.dpy, shapes[n]);
    }
}

CAMLprim value ml_glxinit (value display_v, value wid_v, value screen_v)
{
    CAMLparam3 (display_v, wid_v, screen_v);

    glx.dpy = XOpenDisplay (String_val (display_v));
    if (!glx.dpy) {
        caml_failwith ("XOpenDisplay");
    }

    int attribs[] = { GLX_RGBA, GLX_DOUBLEBUFFER, None };
    glx.visual = glXChooseVisual (glx.dpy, Int_val (screen_v), attribs);
    if (!glx.visual) {
        XCloseDisplay (glx.dpy);
        caml_failwith ("glXChooseVisual");
    }

    initcurs ();

    glx.wid = Int_val (wid_v);
    CAMLreturn (Val_int (glx.visual->visualid));
}

CAMLprim void ml_glxcompleteinit (void)
{
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
}

CAMLprim void ml_setcursor (value cursor_v)
{
    CAMLparam1 (cursor_v);
    size_t cursn = Int_val (cursor_v);

    if (cursn >= CURS_COUNT) caml_failwith ("cursor index out of range");
    XDefineCursor (glx.dpy, glx.wid, glx.curs[cursn]);
    XFlush (glx.dpy);
    CAMLreturn0;
}

CAMLprim void ml_swapb (void)
{
    glXSwapBuffers (glx.dpy, glx.wid);
}

void (*wsigladdr (const char *name)) (void)
{
    return glXGetProcAddress ((const GLubyte *) name);
}

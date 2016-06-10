#include <Cocoa/Cocoa.h>
#include <OpenGL/gl.h>

#define CAML_NAME_SPACE

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/callback.h>

@interface MyDelegate : NSObject <NSApplicationDelegate, NSWindowDelegate>

- (void)focus;
- (void)swapb;
- (int)getw;
- (int)geth;
- (void)setbgcol:(long)col;
- (void)applicationWillFinishLaunching:(NSNotification *)not;
- (void)applicationDidFinishLaunching:(NSNotification *)not;
- (BOOL)applicationShouldTerminateAfterLastWindowClosed:(NSApplication *)theApplication;
- (void)waitForData:(int)fd;

@end

@interface MyWindow : NSWindow

- (void)keyDown:(NSEvent *) event;
- (void)reshapeWidth:(int)w height:(int)h;
- (void)mouseUp:(NSEvent *)event;
- (void)mouseDown:(NSEvent *)event;
- (void)reshapeWidth:(int)w height:(int)h;

@end

@interface MyView : NSView
{
  MyWindow *window;
}

- (instancetype)initWithFrame:(NSRect)frame andWindow:(MyWindow *)myWindow;

@end

CAMLprim value ml_waitfordata (value fd)
{
  CAMLparam1(fd);
  [[NSApp delegate] waitForData:Int_val(fd)];
  CAMLreturn(Val_unit);
}

CAMLprim value ml_focus (value unit)
{
  CAMLparam1(unit);
  [[NSApp delegate] focus];
  CAMLreturn(Val_unit);
}

CAMLprim value ml_swapb (value unit)
{
  [[NSApp delegate] swapb];
  return Val_unit;
}

CAMLprim value ml_getw (value unit)
{
  return Val_int([[NSApp delegate] getw]);
}

CAMLprim value ml_geth (value unit)
{
  return Val_int([[NSApp delegate] geth]);
}

CAMLprim value ml_setbgcol (value col)
{
  NSLog(@"ml_setbgcol");
  [[NSApp delegate] setbgcol:Int_val(col)];
  return Val_unit;
}

CAMLprim value stub_set_title (value title)
{
  [[NSApp delegate] setTitle:[NSString stringWithUTF8String:String_val(title)]];
  return Val_unit;
}

CAMLprim value stub_reshape (value w, value h)
{
  [[NSApp delegate] reshapeWidth:Int_val (w) height:(Int_val (h))];
  return Val_unit;
}

CAMLprim value stub_fullscreen (value unit)
{
  // if (([window styleMask] & NSFullScreenWindowMask) != NSFullScreenWindowMask) {
  //   [window toggleFullScreen:nil];
  // }
  return Val_unit;
}

@implementation MyView

- (instancetype)initWithFrame:(NSRect)frame andWindow:(MyWindow *)myWindow
{
  self = [super initWithFrame:frame];

  if (self != NULL) {
    window = myWindow;
  }

  return self;
}

- (void)drawRect:(NSRect)bounds
{
  NSLog(@"drawRect: %@", [NSValue valueWithRect:bounds]);

  value *cb = caml_named_value("llpp_display");
  if (cb != NULL) {
    caml_callback (*cb, Val_unit);
  }

  // glClearColor(0, 0, 0, 0);
  // glClear(GL_COLOR_BUFFER_BIT);
  // glColor3f(1, .85, .35);
  // glBegin(GL_TRIANGLES);
  // {
  //   glVertex3f(0, 0.6, 0);
  //   glVertex3f(-0.2, -0.3, 0);
  //   glVertex3f(.2, -.3, 0);
  // }
  // glEnd();
  // glFlush();
}

@end

@implementation MyWindow

- (void)reshapeWidth:(int)w height:(int)h
{
  NSRect frame = self.frame;
  [self setFrame:NSMakeRect(frame.origin.x, frame.origin.y, w, h) display:YES];
}

- (void)keyDown:(NSEvent *)event
{
  int key = [event keyCode];
  int mask = [event modifierFlags];
  caml_callback2(*caml_named_value("llpp_key_down"), Val_int(key), Val_int(mask));
}

- (void)keyUp:(NSEvent *)event
{
  int key = [event keyCode];
  int mask = [event modifierFlags];
  caml_callback2(*caml_named_value("llpp_key_up"), Val_int(key), Val_int(mask));
}

- (void)mouseDown:(NSEvent *)event
{
  int buttons = 0; // [event pressedMouseButtons];
  NSPoint loc = [event locationInWindow];
  int mask = [event modifierFlags];
  value args[] = {Val_int (buttons), Val_int(loc.x), Val_int(loc.y), Val_int(mask)};
  caml_callbackN(*caml_named_value ("llpp_mouse_down"), 4, args);
}

- (void)mouseUp:(NSEvent *)event
{
  int buttons = 0; // [event pressedMouseButtons];
  NSPoint loc = [event locationInWindow];
  int mask = [event modifierFlags];
  value args[] = {Val_int (buttons), Val_int(loc.x), Val_int(loc.y), Val_int(mask)};
  caml_callbackN(*caml_named_value("llpp_mouse_up"), 4, args);
}

- (void)mouseMoved:(NSEvent *)event
{
  NSPoint loc = [event locationInWindow];
  caml_callback2(*caml_named_value("llpp_mouse_moved"), Val_int(loc.x), Val_int(loc.y));
}

- (void)mouseEntered:(NSEvent *)theEvent
{
  NSPoint loc = [theEvent locationInWindow];
  caml_callback2(*caml_named_value("llpp_entered"), Val_int(loc.x), Val_int(loc.y));
}

- (void)mouseExited:(NSEvent *)theEvent
{
  caml_callback(*caml_named_value("llpp_left"), Val_unit);
}

@end

@implementation MyDelegate
{
  char **argv;
  MyWindow *window;
  NSOpenGLContext *glContext;
}

- (instancetype)initWithArgv:(char **)theArgv
{
  argv = theArgv;
  return [super init];
}

- (void)setTitle:(NSString *)title
{
  [window setTitle:title];
}

- (void)focus
{
  [glContext makeCurrentContext];
}

- (void)swapb
{
  [glContext flushBuffer];
}

- (int)getw
{
  return [window frame].size.width;
}

- (int)geth
{
  return [window frame].size.height;
}

- (void)setbgcol:(long)col
{
  int r = ((col >> 16) & 0xff) / 255;
  int g = ((col >> 8) & 0xff) / 255;
  int b = ((col >> 0) & 0xff) / 255;
  NSLog(@"setbgcol: %d %d %d", r, g, b);
  [window setBackgroundColor:[NSColor colorWithRed:r green:0 blue:0 alpha:1.0]];
}

- (void)waitForData:(int)fd
{
  [[[NSFileHandle alloc] initWithFileDescriptor:fd] waitForDataInBackgroundAndNotify];
}

- (void)reshapeWidth:(int)w height:(int)h
{
  NSLog(@"reshapeWidth:height");
  [window reshapeWidth:w height:h];
}

- (void)applicationWillFinishLaunching:(NSNotification *)not
{
  NSLog(@"applicationWillFinishLaunching");
  id menubar = [NSMenu new];
  id appMenuItem = [NSMenuItem new];
  [menubar addItem:appMenuItem];
  [NSApp setMainMenu:menubar];
  id appMenu = [NSMenu new];
  id appName = [[NSProcessInfo processInfo] processName];
  id quitTitle = [@"Quit " stringByAppendingString:appName];
  id quitMenuItem = [[NSMenuItem alloc] initWithTitle:quitTitle
                                               action:@selector(terminate:)
                                        keyEquivalent:@"q"];
  [appMenu addItem:quitMenuItem];
  [appMenuItem setSubmenu:appMenu];

  window = [[MyWindow alloc] initWithContentRect:NSMakeRect(0, 0, 400, 400)
                                       styleMask:(NSClosableWindowMask | NSTitledWindowMask | NSResizableWindowMask)
                                         backing:NSBackingStoreBuffered
                                           defer:NO];

  [window center];
  [window setAcceptsMouseMovedEvents:YES];
  [window makeKeyAndOrderFront:self];
  [window setDelegate:self];

  MyView *myView = [[MyView alloc] initWithFrame:[window frame] andWindow:window];

  [window setContentView:myView];

  NSOpenGLPixelFormatAttribute attrs[] =
    {
      NSOpenGLPFAAccelerated,
      NSOpenGLPFADoubleBuffer,
      NSOpenGLPFAColorSize, 24,
      NSOpenGLPFAAlphaSize, 8,
      NSOpenGLPFADepthSize, 24,
      NSOpenGLPFAStencilSize, 8,
      NSOpenGLPFAAccumSize, 0,
      0
// NSOpenGLPFADoubleBuffer,
//       NSOpenGLPFABackingStore,
//       NSOpenGLPFAColorSize, 24,
//       NSOpenGLPFAAlphaSize, 8,
//       NSOpenGLPFAOpenGLProfile,
//       0
    };
  NSOpenGLPixelFormat *pixFormat = [[NSOpenGLPixelFormat alloc] initWithAttributes:attrs];
  glContext = [[NSOpenGLContext alloc] initWithFormat:pixFormat shareContext:nil];
  NSLog (@"%@", glContext);
  [glContext makeCurrentContext];
  NSLog(@"OpenGL Version: %s", glGetString(GL_VERSION));
  [glContext setView:[window contentView]];

  NSTrackingArea *trackingArea =
    [[NSTrackingArea alloc] initWithRect:[[window contentView] bounds]
                                 options:(NSTrackingMouseEnteredAndExited | NSTrackingActiveInActiveApp)
                                   owner:window
                                userInfo:nil];
  [[window contentView] addTrackingArea:trackingArea];

  [[NSNotificationCenter defaultCenter]
    addObserver:self
       selector:@selector(applicationDidReceiveDataAvailable:)
           name:NSFileHandleDataAvailableNotification
         object:nil];
}

- (void)applicationDidReceiveDataAvailable:(NSNotification *)dict
{
  NSFileHandle *fh = [dict object];
  caml_callback(*caml_named_value("llpp_data_available"), Val_int([fh fileDescriptor]));
  [fh waitForDataInBackgroundAndNotify];
}

- (void)windowDidResize:(NSNotification *)notification
{
  caml_callback2(*caml_named_value("llpp_reshaped"),
                 Val_int(window.frame.size.width), Val_int(window.frame.size.height));
}

- (void)applicationWillTerminate:(NSDictionary *)userInfo
{
  caml_callback(*caml_named_value("llpp_quit"), Val_unit);
}

- (void)windowDidChangeOcclusionState:(NSNotification *)notification
{
}

- (void)applicationDidFinishLaunching:(NSNotification *)not
{
  NSLog(@"applicationDidFinishLaunching");
  caml_main(argv);
  NSLog(@"After caml_main");
}

- (BOOL)applicationShouldTerminateAfterLastWindowClosed:(NSApplication *)theApplication
{
  return YES;
}

@end

int main(int argc, char **argv)
{
  @autoreleasepool {
    NSLog(@"Main_OSX");
    [NSApplication sharedApplication];
    [NSApp setActivationPolicy:NSApplicationActivationPolicyRegular];
    id delegate = [[MyDelegate alloc] initWithArgv:argv];
    [NSApp setDelegate:delegate];
    [NSApp activateIgnoringOtherApps:YES];
    [NSApp run];
  }
  return EXIT_SUCCESS;
}

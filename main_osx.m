#include <Cocoa/Cocoa.h>
#include <OpenGL/gl.h>

#define CAML_NAME_SPACE

#include <sys/types.h>
#include <sys/socket.h>
#include <pthread.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/callback.h>

#define EVENT_EXPOSE 1
#define EVENT_RESHAPE 3
#define EVENT_KEYDOWN 7
#define EVENT_ENTER 8
#define EVENT_LEAVE 9
#define EVENT_QUIT 11

void *caml_main_thread (void *argv)
{
  caml_main (argv);
  pthread_exit (NULL);
}

@interface Connector : NSObject

- (instancetype)initWithFileDescriptor:(int)fd;
- (void)notifyReshapeWidth:(int)w height:(int)h;
- (void)notifyExpose;
- (void)notifyKeyDown:(uint32_t)key modifierFlags:(int)mask;
- (void)notifyQuit;
- (void)notifyMouseEnter:(NSPoint)loc;
- (void)notifyLeave;

@end

@implementation Connector
{
  NSFileHandle *fileHandle;
}

- (instancetype)initWithFileDescriptor:(int)fd
{
  self = [super init];
  fileHandle = [[NSFileHandle alloc] initWithFileDescriptor:fd];
  return self;
}

- (void)notifyReshapeWidth:(int)w height:(int)h
{
  char bytes[32];
  bytes[0] = EVENT_RESHAPE;
  *(uint16_t *) (bytes + 16) = w;
  *(uint16_t *) (bytes + 18) = h;
  NSData *data = [[NSData alloc] initWithBytesNoCopy:bytes length:32];
  [fileHandle writeData:data];
}

- (void)notifyExpose
{
  char bytes[32];
  bytes[0] = EVENT_EXPOSE;
  NSData *data = [[NSData alloc] initWithBytesNoCopy:bytes length:32];
  [fileHandle writeData:data];
}

- (void)notifyKeyDown:(uint32_t)key modifierFlags:(int)mask
{
  char bytes[32];
  bytes[0] = EVENT_KEYDOWN;
  *(uint32_t *) (bytes + 16) = key;
  *(uint32_t *) (bytes + 20) = mask;
  NSData *data = [[NSData alloc] initWithBytesNoCopy:bytes length:32];
  [fileHandle writeData:data];
}

- (void)notifyQuit
{
  char bytes[32];
  bytes[0] = EVENT_QUIT;
  NSData *data = [[NSData alloc] initWithBytesNoCopy:bytes length:32];
  [fileHandle writeData:data];
}

- (void)notifyMouseEnter:(NSPoint)loc
{
  char bytes[32];
  bytes[0] = EVENT_ENTER;
  *(int32_t *) (bytes + 16) = loc.x;
  *(int32_t *) (bytes + 20) = loc.y;
  NSData *data = [[NSData alloc] initWithBytesNoCopy:bytes length:32];
  [fileHandle writeData:data];
}

- (void)notifyLeave
{
  char bytes[32];
  bytes[0] = EVENT_LEAVE;
  NSData *data = [[NSData alloc] initWithBytesNoCopy:bytes length:32];
  [fileHandle writeData:data];
}

@end

@interface MyDelegate : NSObject <NSApplicationDelegate, NSWindowDelegate>

- (int)getw;
- (int)geth;
- (void)swapb;
- (void)setwinbgcol:(NSColor *)col;
- (void)applicationWillFinishLaunching:(NSNotification *)not;
- (void)applicationDidFinishLaunching:(NSNotification *)not;
- (BOOL)applicationShouldTerminateAfterLastWindowClosed:(NSApplication *)theApplication;
- (void)completeInit:(int)w height:(int) h;

@end

@interface MyWindow : NSWindow

@end

@interface MyView : NSView
{
  MyWindow *window;
  Connector *connector;
}

- (instancetype)initWithFrame:(NSRect)frame
                    andWindow:(MyWindow *)myWindow
                    connector:(Connector *)aConnector;

@end

@implementation MyView

- (instancetype)initWithFrame:(NSRect)frame
                    andWindow:(MyWindow *)myWindow
                    connector:(Connector *)aConnector
{
  self = [super initWithFrame:frame];

  if (self != NULL) {
    window = myWindow;
    connector = aConnector;
  }

  return self;
}

- (void)drawRect:(NSRect)bounds
{
  NSLog(@"drawRect: %@", [NSValue valueWithRect:bounds]);
  [connector notifyExpose];
}

- (void)viewWillMoveToWindow:(NSWindow *)newWindow {
  NSTrackingArea* trackingArea = [[NSTrackingArea alloc]
                                   initWithRect:[self bounds]
                                 options:(NSTrackingMouseEnteredAndExited | NSTrackingActiveInActiveApp | NSTrackingInVisibleRect)
                                          owner:self
                                       userInfo:nil];
  [self addTrackingArea:trackingArea];
}

- (void)keyDown:(NSEvent *)event // FIXME
{
  // int key = [event keyCode];
  int mask = [event modifierFlags];
  NSString *chars = [event charactersIgnoringModifiers];
  if ([chars length] > 0) {
    NSLog (@"keyDown: %@", chars);
    // NSRange r = [chars rangeOfComposedCharacterSequenceAtIndex:0];
    const char *data = [chars cStringUsingEncoding:NSUTF32LittleEndianStringEncoding];
    [connector notifyKeyDown:*(uint32_t *)data modifierFlags:0];
  }
}

- (void)mouseDown:(NSEvent *)event
{
  int buttons = 0; // [event pressedMouseButtons];
  NSPoint loc = [event locationInWindow];
  int mask = [event modifierFlags];
  value args[] = {Val_int (buttons), Val_int(loc.x), Val_int(loc.y), Val_int(mask)};
  // caml_callbackN(*caml_named_value ("llpp_mouse_down"), 4, args);
}

- (void)mouseUp:(NSEvent *)event
{
  int buttons = 0; // [event pressedMouseButtons];
  NSPoint loc = [event locationInWindow];
  int mask = [event modifierFlags];
  value args[] = {Val_int (buttons), Val_int(loc.x), Val_int(loc.y), Val_int(mask)};
  // caml_callbackN(*caml_named_value("llpp_mouse_up"), 4, args);
}

- (void)mouseMoved:(NSEvent *)event
{
  NSPoint loc = [event locationInWindow];
  // caml_callback2(*caml_named_value("llpp_mouse_moved"), Val_int(loc.x), Val_int(loc.y));
}

- (void)mouseEntered:(NSEvent *)theEvent
{
  NSPoint loc = [theEvent locationInWindow];
  [connector notifyMouseEnter:loc];
}

- (void)mouseExited:(NSEvent *)theEvent
{
  [connector notifyLeave];
}

@end

@implementation MyWindow

- (BOOL)canBecomeKeyWindow
{
  return YES;
}

@end

@implementation MyDelegate
{
  char **argv;
  MyWindow *window;
  NSOpenGLContext *glContext;
  pthread_t thread;
  Connector *connector;
}

- (instancetype)initWithArgv:(char **)theArgv fileDescriptor:(int)fd
{
  self = [super init];
  if (self != NULL) {
    argv = theArgv;
    connector = [[Connector alloc] initWithFileDescriptor:fd];
  }
  return self;
}

- (void)setTitle:(NSString *)title
{
  [window setTitle:title];
}

- (void)mapwin
{
  [window makeKeyAndOrderFront:self];
}

- (int)getw
{
  return window.contentView.frame.size.width;
}

- (int)geth
{
  return window.contentView.frame.size.height;
}

- (void)setwinbgcol:(NSColor *)col
{
  [window setBackgroundColor:col];
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
                                       styleMask:(NSBorderlessWindowMask | NSResizableWindowMask)// | NSClosableWindowMask | NSTitledWindowMask | NSResizableWindowMask)
                                         backing:NSBackingStoreBuffered
                                           defer:NO];

  [window setMovableByWindowBackground:YES];
  [window center];
  [window setAcceptsMouseMovedEvents:YES];
  [window setDelegate:self];

  MyView *myView = [[MyView alloc] initWithFrame:[[window contentView] bounds]
                                       andWindow:window
                                       connector:connector];

  [window setContentView:myView];
  [window makeFirstResponder:myView];

  // [myView setWantsBestResolutionOpenGLSurface:YES];

  NSOpenGLPixelFormatAttribute attrs[] =
    {
      NSOpenGLPFAAccelerated,
      NSOpenGLPFADoubleBuffer,
      NSOpenGLPFAColorSize, 24,
      NSOpenGLPFAAlphaSize, 8,
      NSOpenGLPFADepthSize, 24,
      0
    };
  NSOpenGLPixelFormat *pixFormat = [[NSOpenGLPixelFormat alloc] initWithAttributes:attrs];
  glContext = [[NSOpenGLContext alloc] initWithFormat:pixFormat shareContext:nil];
  GLint swapInt = 1;
  [glContext setValues:&swapInt forParameter:NSOpenGLCPSwapInterval];
  [glContext setView:myView];
}

- (void)setContentFrame:(NSValue *)val
{
  NSRect rect = [window frameRectForContentRect:[val rectValue]];
  [window setFrame:rect display:YES];
}

- (void)completeInit:(int)w height:(int)h
{
  NSLog (@"completeInit");
  [glContext makeCurrentContext];
  NSLog (@"OpenGL Version: %s", glGetString(GL_VERSION));
  [self performSelectorOnMainThread:@selector(setContentFrame:)
                         withObject:[NSValue valueWithRect:NSMakeRect(0, 0, w, h)]
                      waitUntilDone:YES];
}

- (void)swapb
{
  [glContext flushBuffer];
}

- (void)toggleFullScreen
{
  [window toggleFullScreen:self];
}

- (void)windowDidResize:(NSNotification *)notification
{
  [glContext update];
  [connector notifyReshapeWidth:window.frame.size.width height:window.frame.size.height];
}

- (void)windowDidMove:(NSNotification *)notification
{
  [glContext update];
}

- (void)applicationWillTerminate:(NSDictionary *)userInfo
{
  [connector notifyQuit];
  pthread_join (thread, NULL);
}

- (void)windowDidChangeOcclusionState:(NSNotification *)notification
{
}

- (void)applicationDidFinishLaunching:(NSNotification *)not
{
  NSLog(@"applicationDidFinishLaunching");
  int ret = pthread_create (&thread, NULL, caml_main_thread, argv);
  if (ret != 0) {
    NSLog (@"pthread_create: %s", strerror (ret));
  }
  NSLog(@"After caml_main");
}

- (BOOL)applicationShouldTerminateAfterLastWindowClosed:(NSApplication *)theApplication
{
  return YES;
}

@end

CAMLprim value ml_mapwin (value unit)
{
  CAMLparam1 (unit);
  [[NSApp delegate] performSelectorOnMainThread:@selector(mapwin)
                                     withObject:nil
                                  waitUntilDone:YES];
  CAMLreturn (Val_unit);
}

CAMLprim value ml_swapb (value unit)
{
  CAMLparam1 (unit);
  [[NSApp delegate] swapb];
  CAMLreturn (Val_unit);
}

CAMLprim value ml_getw (value unit)
{
  return Val_int([[NSApp delegate] getw]);
}

CAMLprim value ml_geth (value unit)
{
  return Val_int([[NSApp delegate] geth]);
}

CAMLprim value ml_completeinit (value w, value h)
{
  CAMLparam2 (w, h);
  NSLog (@"ml_completeinit: w %d h %d", Int_val (w), Int_val (h));
  [[NSApp delegate] completeInit:Int_val(w) height:Int_val(h)];
  CAMLreturn (Val_unit);
}

CAMLprim value ml_setwinbgcol (value col)
{
  CAMLparam1 (col);
  int r = ((col >> 16) & 0xff) / 255;
  int g = ((col >> 8) & 0xff) / 255;
  int b = ((col >> 0) & 0xff) / 255;
  NSColor *color = [NSColor colorWithRed:r green:0 blue:0 alpha:1.0];
  [[NSApp delegate] performSelectorOnMainThread:@selector(setwinbgcol:)
                                     withObject:color
                                  waitUntilDone:YES];
  CAMLreturn (Val_unit);
}

CAMLprim value ml_settitle (value title)
{
  CAMLparam1 (title);
  NSString *str = [NSString stringWithUTF8String:String_val(title)];
  [[NSApp delegate] performSelectorOnMainThread:@selector(setTitle:)
                                     withObject:str
                                  waitUntilDone:YES];
  CAMLreturn (Val_unit);
}

CAMLprim value ml_reshape (value w, value h)
{
  CAMLparam2 (w, h);
  [[NSApp delegate] performSelectorOnMainThread:@selector(setContentFrame:)
                                     withObject:[NSValue valueWithRect:NSMakeRect (0, 0, Int_val (w), Int_val (h))]
                                  waitUntilDone:YES];
  CAMLreturn (Val_unit);
}

CAMLprim value ml_fullscreen (value unit)
{
  CAMLparam1 (unit);
  [[NSApp delegate] performSelectorOnMainThread:@selector(toggleFullScreen)
                                     withObject:nil
                                  waitUntilDone:YES];
  CAMLreturn (Val_unit);
}

int main(int argc, char **argv)
{
  @autoreleasepool {
    int sv[2];
    int ret = socketpair (AF_UNIX, SOCK_STREAM, 0, sv);
    if (ret != 0) {
      NSLog (@"socketpair: %s", strerror (ret));
    }
    NSLog (@"socketpair sv0 %d sv1 %d", sv[0], sv[1]);
    char buf[10];
    sprintf (buf, "%d", sv[0]);
    setenv ("LLPP_DISPLAY", buf, 1);
    [NSApplication sharedApplication];
    [NSApp setActivationPolicy:NSApplicationActivationPolicyRegular];
    id delegate = [[MyDelegate alloc] initWithArgv:argv fileDescriptor:sv[1]];
    [NSApp setDelegate:delegate];
    [NSApp activateIgnoringOtherApps:YES];
    [NSApp run];
  }
  return EXIT_SUCCESS;
}

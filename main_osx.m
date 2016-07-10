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
#define EVENT_MOUSE 4
#define EVENT_MOTION 5
#define EVENT_PMOTION 6
#define EVENT_KEYDOWN 7
#define EVENT_ENTER 8
#define EVENT_LEAVE 9
#define EVENT_WINSTATE 10
#define EVENT_QUIT 11

#define BUTTON_LEFT 1
#define BUTTON_RIGHT 3
#define BUTTON_WHEEL_UP 4
#define BUTTON_WHEEL_DOWN 5

static int terminating = 0;
static pthread_mutex_t terminate_mutex = PTHREAD_MUTEX_INITIALIZER;

void *caml_main_thread (void *argv)
{
  caml_main (argv);
  pthread_mutex_lock (&terminate_mutex);
  if (terminating == 0) {
    terminating = 1;
    [NSApp performSelectorOnMainThread:@selector(terminate:)
                            withObject:nil
                         waitUntilDone:NO];
  }
  pthread_mutex_unlock (&terminate_mutex);
  pthread_exit (NULL);
}

NSCursor *GetCursor (int idx)
{
  static NSCursor *cursors[5];
  static BOOL initialised = NO;

  if (initialised == NO) {
    cursors[0] = [NSCursor arrowCursor];
    cursors[1] = [NSCursor pointingHandCursor];
    cursors[2] = [NSCursor arrowCursor];
    cursors[3] = [NSCursor closedHandCursor];
    cursors[4] = [NSCursor IBeamCursor];
    initialised = YES;
  }

  return cursors[idx];
}

@implementation NSWindow (CategoryNSWindow)

- (BOOL)isFullScreen
{
  return ([self styleMask] & NSFullScreenWindowMask) == NSFullScreenWindowMask;
}

@end

@implementation NSView (CategoryNSView)

- (NSPoint)locationFromEvent:(NSEvent *)event
{
  NSPoint loc = [self convertPoint:[event locationInWindow] fromView:nil];
  loc.y = [self bounds].size.height - loc.y;
  return loc;
}

@end

@implementation NSEvent (CategoryNSEvent)

- (int)deviceIndependentModifierFlags
{
  return [self modifierFlags] & NSDeviceIndependentModifierFlagsMask;
}

@end

@interface Connector : NSObject

- (instancetype)initWithFileDescriptor:(int)fd;
- (void)notifyReshapeWidth:(int)w height:(int)h;
- (void)notifyExpose;
- (void)keyDown:(uint32_t)key modifierFlags:(NSEventModifierFlags)mask;
- (void)notifyQuit;
- (void)mouseEntered:(NSPoint)loc;
- (void)mouseExited;
- (void)mouseMoved:(NSPoint)aPoint modifierFlags:(NSEventModifierFlags)flags;
- (void)mouseDown:(NSUInteger)buttons atPoint:(NSPoint)aPoint modifierFlags:(NSEventModifierFlags)flags;
- (void)mouseUp:(NSUInteger)buttons atPoint:(NSPoint)aPoint modifierFlags:(NSEventModifierFlags)flags;
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

- (void)keyDown:(uint32_t)key modifierFlags:(NSEventModifierFlags)mask
{
  char bytes[32];
  bytes[0] = EVENT_KEYDOWN;
  *(uint32_t *) (bytes + 16) = key;
  *(uint32_t *) (bytes + 20) = mask;
  NSData *data = [[NSData alloc] initWithBytesNoCopy:bytes length:32];
  [fileHandle writeData:data];
}

- (void)notifyWinstate:(BOOL)fullScreen
{
  char bytes[32];
  bytes[0] = EVENT_WINSTATE;
  *(uint32_t *) (bytes + 16) = fullScreen;
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

- (void)mouseEntered:(NSPoint)loc
{
  char bytes[32];
  bytes[0] = EVENT_ENTER;
  *(int16_t *) (bytes + 16) = loc.x;
  *(int16_t *) (bytes + 20) = loc.y;
  NSData *data = [[NSData alloc] initWithBytesNoCopy:bytes length:32];
  [fileHandle writeData:data];
}

- (void)mouseExited
{
  char bytes[32];
  bytes[0] = EVENT_LEAVE;
  NSData *data = [[NSData alloc] initWithBytesNoCopy:bytes length:32];
  [fileHandle writeData:data];
}

- (void)mouseDragged:(NSPoint)aPoint modifierFlags:(NSEventModifierFlags)flags
{
  char bytes[32];
  bytes[0] = EVENT_MOTION;
  *(int16_t *) (bytes + 16) = aPoint.x;
  *(int16_t *) (bytes + 20) = aPoint.y;
  *(uint32_t *) (bytes + 24) = flags;
  NSData *data = [[NSData alloc] initWithBytesNoCopy:bytes length:32];
  [fileHandle writeData:data];
}

- (void)mouseMoved:(NSPoint)aPoint modifierFlags:(NSEventModifierFlags)flags
{
  char bytes[32];
  bytes[0] = EVENT_PMOTION;
  *(int16_t *) (bytes + 16) = aPoint.x;
  *(int16_t *) (bytes + 20) = aPoint.y;
  *(uint32_t *) (bytes + 24) = flags;
  NSData *data = [[NSData alloc] initWithBytesNoCopy:bytes length:32];
  [fileHandle writeData:data];
}

- (void)mouseDown:(NSUInteger)buttons atPoint:(NSPoint)aPoint modifierFlags:(NSEventModifierFlags)flags
{
  char bytes[32];
  bytes[0] = EVENT_MOUSE;
  *(int16_t *) (bytes + 10) = 1;
  *(int32_t *) (bytes + 12) = buttons;
  *(int16_t *) (bytes + 16) = aPoint.x;
  *(int16_t *) (bytes + 20) = aPoint.y;
  *(uint32_t *) (bytes + 24) = flags;
  NSData *data = [[NSData alloc] initWithBytesNoCopy:bytes length:32];
  [fileHandle writeData:data];
}

- (void)mouseUp:(NSUInteger)buttons atPoint:(NSPoint)aPoint modifierFlags:(NSEventModifierFlags)flags
{
  char bytes[32];
  bytes[0] = EVENT_MOUSE;
  *(int16_t *) (bytes + 10) = 0;
  *(int32_t *) (bytes + 12) = buttons;
  *(int16_t *) (bytes + 16) = aPoint.x;
  *(int16_t *) (bytes + 20) = aPoint.y;
  *(uint32_t *) (bytes + 24) = flags;
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
  Connector *connector;
  NSCursor *cursor;
}

- (instancetype)initWithFrame:(NSRect)frame connector:(Connector *)aConnector;
- (void)setCursor:(NSCursor *)aCursor;

@end

@implementation MyView

- (instancetype)initWithFrame:(NSRect)frame connector:(Connector *)aConnector
{
  self = [super initWithFrame:frame];

  if (self != NULL) {
    connector = aConnector;
    cursor = [NSCursor arrowCursor];
    self.acceptsTouchEvents = YES;
  }

  return self;
}

- (void)setCursor:(NSCursor *)aCursor
{
  cursor = aCursor;
}

-(void)resetCursorRects
{
  [self addCursorRect:[self bounds] cursor:cursor];
}

- (void)drawRect:(NSRect)bounds
{
  // NSLog(@"drawRect: %@", [NSValue valueWithRect:bounds]);
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

- (void)keyDown:(NSEvent *)event
{
  // int key = [event keyCode];
  NSEventModifierFlags mask = [event deviceIndependentModifierFlags];
  NSString *chars = [event charactersIgnoringModifiers];
  const uint32_t *c = (uint32_t *) [chars cStringUsingEncoding:NSUTF32LittleEndianStringEncoding];
  while (*c) {
    if (*c == 0x7f && !(mask & NSFunctionKeyMask)) {
      [connector keyDown:0x8 modifierFlags:mask];
    } else {
      [connector keyDown:*c modifierFlags:mask];
    }
    c++;
  }
}

- (void)flagsChanged:(NSEvent *)event
{
  NSEventModifierFlags mask = [event deviceIndependentModifierFlags];
  NSLog (@"flagsChanged: 0x%lx", mask);
  if (mask != 0) {
    [connector keyDown:0 modifierFlags:mask];
  }
}

- (void)mouseDown:(NSEvent *)event
{
  NSPoint loc = [self locationFromEvent:event];
  NSEventModifierFlags mask = [event deviceIndependentModifierFlags];
  [connector mouseDown:BUTTON_LEFT atPoint:loc modifierFlags:mask];
}

- (void)mouseUp:(NSEvent *)event
{
  NSPoint loc = [self locationFromEvent:event];
  NSEventModifierFlags mask = [event deviceIndependentModifierFlags];
  [connector mouseUp:BUTTON_LEFT atPoint:loc modifierFlags:mask];
}

- (void)rightMouseDown:(NSEvent *)event
{
  NSPoint loc = [self locationFromEvent:event];
  NSEventModifierFlags mask = [event deviceIndependentModifierFlags];
  [connector mouseDown:BUTTON_RIGHT atPoint:loc modifierFlags:mask];
}

- (void)rightMouseUp:(NSEvent *)event
{
  NSPoint loc = [self locationFromEvent:event];
  NSEventModifierFlags mask = [event deviceIndependentModifierFlags];
  [connector mouseUp:BUTTON_RIGHT atPoint:loc modifierFlags:mask];
}

- (void)rightMouseDragged:(NSEvent *)event
{
  NSPoint loc = [self locationFromEvent:event];
  NSEventModifierFlags mask = [event deviceIndependentModifierFlags];
  [connector mouseDragged:loc modifierFlags:mask];
}

- (void)mouseDragged:(NSEvent *)event
{
  NSPoint loc = [self locationFromEvent:event];
  NSEventModifierFlags mask = [event deviceIndependentModifierFlags];
  [connector mouseDragged:loc modifierFlags:mask];
}

- (void)mouseMoved:(NSEvent *)event
{
  NSPoint loc = [self locationFromEvent:event];
  NSEventModifierFlags mask = [event deviceIndependentModifierFlags];
  [connector mouseMoved:loc modifierFlags:mask];
}

- (void)mouseEntered:(NSEvent *)event
{
  NSPoint loc = [self locationFromEvent:event];
  [connector mouseEntered:loc];
}

- (void)mouseExited:(NSEvent *)event
{
  [connector mouseExited];
}

- (void)scrollWheel:(NSEvent *)event
{
  // NSLog (@"scrollWheel: %@", event);
  CGFloat d = [event deltaY];
  NSPoint loc = [self locationFromEvent:event];
  NSEventModifierFlags mask = [event deviceIndependentModifierFlags];
  if (d > 0.0) {
    [connector mouseDown:BUTTON_WHEEL_UP atPoint:loc modifierFlags:mask];
    [connector mouseUp:BUTTON_WHEEL_UP atPoint:loc modifierFlags:mask];
  } else if (d < 0.0) {
    [connector mouseDown:BUTTON_WHEEL_DOWN atPoint:loc modifierFlags:mask];
    [connector mouseUp:BUTTON_WHEEL_DOWN atPoint:loc modifierFlags:mask];
  }
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
  return [[window contentView] frame].size.width;
}

- (int)geth
{
  return [[window contentView] frame].size.height;
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
                                       styleMask:(NSClosableWindowMask | NSTitledWindowMask | NSResizableWindowMask)
                                         backing:NSBackingStoreBuffered
                                           defer:NO];

  [window center];
  [window setAcceptsMouseMovedEvents:YES];
  [window setDelegate:self];


  [[NSNotificationCenter defaultCenter] addObserver:self
                                           selector:@selector(didEnterFullScreen)
                                               name:NSWindowDidEnterFullScreenNotification
                                             object:window];
  [[NSNotificationCenter defaultCenter] addObserver:self
                                           selector:@selector(didExitFullScreen)
                                               name:NSWindowDidExitFullScreenNotification
                                             object:window];

  MyView *myView = [[MyView alloc] initWithFrame:[[window contentView] bounds]
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

- (void)reshape:(NSValue *)val
{
  // NSLog (@"reshape: %@ isFullScreen: %d", val, [window isFullScreen]);
  if ([window isFullScreen]) {
    [window toggleFullScreen:self];
  }
  NSRect rect = [window frameRectForContentRect:[val rectValue]];
  [window setFrame:rect display:YES];
}

- (void)completeInit:(int)w height:(int)h
{
  NSLog (@"completeInit");
  [glContext makeCurrentContext];
  NSLog (@"OpenGL Version: %s", glGetString(GL_VERSION));
  [self performSelectorOnMainThread:@selector(reshape:)
                         withObject:[NSValue valueWithRect:NSMakeRect(0, 0, w, h)]
                      waitUntilDone:YES];
}

- (void)swapb
{
  [glContext flushBuffer];
}

- (void)didEnterFullScreen
{
  // NSLog (@"didEnterFullScreen: %d", [window isFullScreen]);
  [connector notifyWinstate:YES];
}

- (void)didExitFullScreen
{
  // NSLog (@"didExitFullScreen: %d", [window isFullScreen]);
  [connector notifyWinstate:NO];
}

- (void)fullscreen
{
  // NSLog (@"fullscreen: %d", [window isFullScreen]);
  if ([window isFullScreen] == NO) {
    [window toggleFullScreen:self];
  }
}

- (void)setCursor:(NSCursor *)aCursor
{
  [[window contentView] setCursor: aCursor];
  [window invalidateCursorRectsForView:[window contentView]];
}

- (void)windowDidResize:(NSNotification *)notification
{
  [glContext update];
  NSRect frame = [[window contentView] frame];
  [connector notifyReshapeWidth:frame.size.width height:frame.size.height];
}

- (void)windowDidMove:(NSNotification *)notification
{
  [glContext update];
}

- (void)applicationWillTerminate:(NSDictionary *)userInfo
{
  pthread_mutex_lock (&terminate_mutex);
  if (terminating == 0) {
    terminating = 1;
    [connector notifyQuit];
  }
  pthread_mutex_unlock (&terminate_mutex);
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
  [[NSApp delegate] performSelectorOnMainThread:@selector(reshape:)
                                     withObject:[NSValue valueWithRect:NSMakeRect (0, 0, Int_val (w), Int_val (h))]
                                  waitUntilDone:YES];
  CAMLreturn (Val_unit);
}

CAMLprim value ml_fullscreen (value unit)
{
  CAMLparam1 (unit);
  [[NSApp delegate] performSelectorOnMainThread:@selector(fullscreen)
                                     withObject:nil
                                  waitUntilDone:YES];
  CAMLreturn (Val_unit);
}

CAMLprim value ml_setcursor (value curs)
{
  CAMLparam1 (curs);
  NSLog (@"ml_setcursor: %d", Int_val (curs));
  NSCursor *cursor = GetCursor (Int_val (curs));
  [[NSApp delegate] performSelectorOnMainThread:@selector(setCursor:)
                                     withObject:cursor
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

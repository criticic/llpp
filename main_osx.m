#include <Cocoa/Cocoa.h>
#include <OpenGL/gl.h>

#define CAML_NAME_SPACE

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/callback.h>

@interface MyDelegate : NSObject <NSApplicationDelegate, NSWindowDelegate>

- (void) applicationWillFinishLaunching:(NSNotification *)not;
- (void) applicationDidFinishLaunching:(NSNotification *)not;
- (BOOL) applicationShouldTerminateAfterLastWindowClosed: (NSApplication *) theApplication;

@end

@interface MyWindow : NSWindow

- (void) keyDown: (NSEvent *) event;
- (void) reshapeWidth:(int)w height:(int)h;
- (void) mouseUp: (NSEvent *)event;
- (void) mouseDown: (NSEvent *)event;

@end

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

@interface MyOpenGLView : NSOpenGLView

- (void)drawRect:(NSRect)bounds;

@end

@implementation MyOpenGLView

- (void)drawRect:(NSRect)bounds
{
  NSLog(@"drawRect");
  value *cb = caml_named_value("llpp_display");
  if (cb != NULL) {
    caml_callback (*cb, Val_unit);
  }
}

@end

@implementation MyWindow

- (void)reshapeWidth:(int)w height:(int)h
{
  NSRect frame = self.frame;
  [self setFrame: NSMakeRect (frame.origin.x, frame.origin.y, w, h) display:YES];
}

- (void)keyDown:(NSEvent *)event
{
  int key = [event keyCode];
  int mask = [event modifierFlags];
  caml_callback2 (*caml_named_value ("llpp_key_down"), Val_int (key), Val_int (mask));
}

- (void)keyUp:(NSEvent *)event
{
  int key = [event keyCode];
  int mask = [event modifierFlags];
  caml_callback2 (*caml_named_value ("llpp_key_up"), Val_int (key), Val_int (mask));
}

- (void)mouseDown:(NSEvent *)event
{
  int buttons = 0; // [event pressedMouseButtons];
  NSPoint loc = [event locationInWindow];
  int mask = [event modifierFlags];
  value args[] = {Val_int (buttons), Val_int(loc.x), Val_int(loc.y), Val_int(mask)};
  caml_callbackN (*caml_named_value ("llpp_mouse_down"), 4, args);
}

- (void)mouseUp:(NSEvent *)event
{
  int buttons = 0; // [event pressedMouseButtons];
  NSPoint loc = [event locationInWindow];
  int mask = [event modifierFlags];
  value args[] = {Val_int (buttons), Val_int(loc.x), Val_int(loc.y), Val_int(mask)};
  caml_callbackN (*caml_named_value ("llpp_mouse_up"), 4, args);
}

- (void)mouseMoved:(NSEvent *)event
{
  NSPoint loc = [event locationInWindow];
  caml_callback2 (*caml_named_value ("llpp_mouse_moved"), Val_int (loc.x), Val_int (loc.y));
}

- (void)mouseEntered:(NSEvent *)theEvent
{
  NSPoint loc = [theEvent locationInWindow];
  caml_callback2 (*caml_named_value ("llpp_entered"), Val_int (loc.x), Val_int (loc.y));
}

- (void)mouseExited:(NSEvent *)theEvent
{
  caml_callback (*caml_named_value ("llpp_left"), Val_unit);
}

@end

@implementation MyDelegate
{
  char **argv;
  NSWindow *window;
}

- (instancetype) initWithArgv:(char **)theArgv
{
  argv = theArgv;
  return [super init];
}

- (void)setTitle:(NSString *)title
{
  [window setTitle:title];
}

- (void)reshapeWidth:(int)w height:(int)h
{
  NSLog(@"reshapeWidth:height");
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

  [window setAcceptsMouseMovedEvents:YES];
  [window makeKeyAndOrderFront:self];
  NSLog(@"%@", [window deviceDescription]);
  [window cascadeTopLeftFromPoint:NSMakePoint(20,20)];
  NSOpenGLView *openGLView =
    [[MyOpenGLView alloc] initWithFrame:[window frame]
                            pixelFormat:[NSOpenGLView defaultPixelFormat]];
  NSLog (@"%@", openGLView);
  [window setContentView:openGLView];
  [[openGLView openGLContext] makeCurrentContext];
  NSLog(@"OpenGL Version: %s", glGetString(GL_VERSION));
  [window setDelegate:self];
  NSTrackingArea *trackingArea =
    [[NSTrackingArea alloc] initWithRect:[[window contentView] bounds]
                                 options:(NSTrackingMouseEnteredAndExited | NSTrackingActiveAlways)
                                   owner:window
                                userInfo:nil];
  [[window contentView] addTrackingArea:trackingArea];
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

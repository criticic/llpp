#define CAML_NAME_SPACE

#include <pthread.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/callback.h>

@import Cocoa;

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

MyWindow *window = nil;
char **global_argv = NULL;

CAMLprim value stub_set_title (value title)
{
  if (window != NULL) {
    [window setTitle:[NSString stringWithUTF8String:String_val(title)]];
  }
  return Val_unit;
}

CAMLprim value stub_reshape (value w, value h)
{
  if (window != NULL) {
    [window reshapeWidth:Int_val (w) height:(Int_val (h))];
  }
  return Val_unit;
}

CAMLprim value stub_fullscreen (value unit)
{
  if (window != NULL) {
    if (([window styleMask] & NSFullScreenWindowMask) != NSFullScreenWindowMask) {
      [window toggleFullScreen:nil];
    }
  }
  return Val_unit;
}

@interface MyOpenGLView : NSOpenGLView

- (void)drawRect:(NSRect)bounds;

@end

@implementation MyOpenGLView

- (void)drawRect:(NSRect)bounds
{
  caml_callback (*caml_named_value ("llpp_display"), Val_unit);
}

@end

@implementation MyWindow
{
  NSOpenGLView *openGLView;
}

- (void) setUp
{
  self.acceptsMouseMovedEvents = YES;
  openGLView =
    [[MyOpenGLView alloc] initWithFrame:[self frame] pixelFormat:[NSOpenGLView defaultPixelFormat]];
  [[self contentView] addSubview:openGLView];
}

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

- (void) applicationWillFinishLaunching:(NSNotification *)not
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
                                         styleMask:(NSTitledWindowMask | NSResizableWindowMask)
                                           backing:NSBackingStoreBuffered
                                             defer:NO];
    [window cascadeTopLeftFromPoint:NSMakePoint (20,20)];
    [window makeKeyAndOrderFront:nil];
    [window setDelegate:self];
    [window setUp];

    NSTrackingArea *trackingArea =
      [[NSTrackingArea alloc] initWithRect:[[window contentView] bounds]
                                   options:(NSTrackingMouseEnteredAndExited | NSTrackingActiveAlways)
                                     owner:window
                                  userInfo:nil];
    [[window contentView] addTrackingArea:trackingArea];

    [[NSNotificationCenter defaultCenter] addObserver:self
                                             selector:@selector(appWillTerminate:)
                                                 name:NSApplicationWillTerminateNotification
                                               object:nil];
}

- (void)windowDidResize:(NSNotification *)notification
{
  NSWindow *w = [notification object];
  if (window != NULL) {
    caml_callback2 (*caml_named_value ("llpp_reshaped"),
                    Val_int (w.frame.size.width), Val_int (w.frame.size.height));
  }
}

- (void)appWillTerminate:(NSDictionary *)userInfo
{
  caml_callback (*caml_named_value ("llpp_quit"), Val_unit);
}

- (void)windowDidChangeOcclusionState:(NSNotification *)notification
{
}

- (void)applicationDidFinishLaunching:(NSNotification *)not
{
    NSLog(@"applicationDidFinishLaunching");
    caml_startup (global_argv);
}

- (BOOL)applicationShouldTerminateAfterLastWindowClosed:(NSApplication *)theApplication
{
  return YES;
}

@end

int main(int argc, char **argv)
{
    global_argv = argv;
    @autoreleasepool {
        NSLog(@"Main_OSX");
        [NSApplication sharedApplication];
        [NSApp setActivationPolicy:NSApplicationActivationPolicyRegular];
        id delegate = [MyDelegate new];
        [NSApp setDelegate:delegate];
        [NSApp activateIgnoringOtherApps:YES];
        [NSApp run];
    }
    return EXIT_SUCCESS;
}

#define CAML_NAME_SPACE

#include <pthread.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>

extern void * caml_startup(void *);

@import Cocoa;

@interface MyDelegate : NSObject <NSApplicationDelegate>
- (void) applicationWillFinishLaunching:(NSNotification *)not;
- (void) applicationDidFinishLaunching:(NSNotification *)not;
@end

NSWindow *window = nil;
char **global_argv = NULL;

CAMLprim value stub_set_title (value title, bool wait)
{
    if (window == NULL) return Val_unit;
    [window performSelectorOnMainThread:@selector(setTitle:) withObject:title waitUntilDone:wait];
    return Val_unit;
}

CAMLprim value stub_reshape (value w, value h)
{
    if (window == NULL) return Val_unit;
    id arg = [NSValue valueWithPoint:NSMakePoint(Int_val (w), Int_val (h))];
    [window performSelectorOnMainThread:@selector(reshape:) withObject:arg waitUntilDone:wait];
    return Val_unit;
}

@implementation MyDelegate
- (void) reshape: (NSValue *) pt
{
    [window setFrame: display:YES];
    [pt release];
}

- (void) keyDown: (NSEvent *) event
{
    int key = [event keyCode];
    int mask = [event modifierFlags];
    caml_callback2 (*caml_named_value ("post_key_down"), Val_int (key), Val_int (mask));
}

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

    window = [[NSWindow alloc] initWithContentRect:NSMakeRect(0, 0, 400, 400)
                                         styleMask:(NSTitledWindowMask | NSResizableWindowMask)
                                           backing:NSBackingStoreBuffered
                                             defer:NO];

    [window cascadeTopLeftFromPoint:NSMakePoint (20,20)];
    [window makeKeyAndOrderFront:nil];
}

- (void) applicationDidFinishLaunching:(NSNotification *)not
{
    NSLog(@"applicationDidFinishLaunching");
    pthread_t thread;
    pthread_create(&thread, NULL, caml_startup, global_argv);
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

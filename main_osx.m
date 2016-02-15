#define CAML_NAME_SPACE

#include <caml/mlvalues.h>
#include <caml/memory.h>

@import Cocoa;

@interface MyDelegate : NSObject <NSApplicationDelegate>
- (void) applicationWillFinishLaunching: (NSNotification *)not;
- (void)applicationDidFinishLaunching:(NSNotification *)not;
@end

NSWindow *window = nil;
char **global_argv = NULL;
extern void caml_startup(char **);

@implementation MyDelegate
- (void) applicationWillFinishLaunching: (NSNotification *)not
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
                                                 action:@selector(terminate:) keyEquivalent:@"q"];
    [appMenu addItem:quitMenuItem];
    [appMenuItem setSubmenu:appMenu];

    window =
        [[NSWindow alloc] initWithContentRect:NSMakeRect(0, 0, 400, 400)
                                    styleMask:(NSTitledWindowMask | NSResizableWindowMask)
                                      backing:NSBackingStoreBuffered
                                        defer:NO];

    [window cascadeTopLeftFromPoint:NSMakePoint(20,20)];
    [window makeKeyAndOrderFront:nil];
}

- (void)applicationDidFinishLaunching:(NSNotification *)not
{
    NSLog(@"applicationDidFinishLaunching");
    caml_startup(global_argv);
}
@end

int main (int argc, char **argv)
{
    global_argv = argv;
    @autoreleasepool {
        NSLog (@"Main_OSX");
        [NSApplication sharedApplication];
        [NSApp setActivationPolicy:NSApplicationActivationPolicyRegular];
        id delegate = [MyDelegate new];
        [NSApp setDelegate:delegate];
        [NSApp activateIgnoringOtherApps:YES];
        [NSApp run];
    }
    return EXIT_SUCCESS;
}

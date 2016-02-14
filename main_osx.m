@import Cocoa;

int main (int argc, char **argv)
{
    @autoreleasepool {
        NSLog (@"Main_OSX");

        [NSApplication sharedApplication];
        [NSApp setActivationPolicy:NSApplicationActivationPolicyRegular];

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

        NSWindow *window =
            [[NSWindow alloc] initWithContentRect:NSMakeRect(0, 0, 400, 400)
                                        styleMask:(NSTitledWindowMask | NSResizableWindowMask)
                                          backing:NSBackingStoreBuffered
                                            defer:NO];

        [window cascadeTopLeftFromPoint:NSMakePoint(20,20)];
        [window makeKeyAndOrderFront:nil];

        [NSApp activateIgnoringOtherApps:YES];
        [NSApp run];
    }
    return EXIT_SUCCESS;
}

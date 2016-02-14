@import Cocoa;

int main (int argc, char **argv)
{
    @autoreleasepool {
        NSLog (@"Main_OSX");

        [NSApplication sharedApplication];
        [NSApp setActivationPolicy:NSApplicationActivationPolicyRegular];

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

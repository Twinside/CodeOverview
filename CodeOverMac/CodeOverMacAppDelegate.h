//
//  CodeOverMacAppDelegate.h
//  CodeOverMac
//
//  Created by Vincent Berthoux on 05/04/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import "FileWatcher.h"

@interface CodeOverMacAppDelegate : NSObject <NSApplicationDelegate> {
    NSWindow *window;
    FileWatcher *watcher;
}

@property (assign) IBOutlet NSWindow *window;
@property (nonatomic, retain) IBOutlet FileWatcher *watcher;
@end

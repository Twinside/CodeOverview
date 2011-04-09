//
//  FileWatcher.h
//  CodeOverMac
//
//  Created by Vincent Berthoux on 05/04/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import <Cocoa/Cocoa.h>

typedef int KQueue;
typedef int EventFile;

@interface FileWatcher : NSImageView {
    FSEventStreamRef fileWatcher;
    NSWindow *window;
    
    NSInteger viewBegin;
    NSInteger viewEnd;
    
    NSInteger senderWindowXpos;
    NSInteger senderWindowYpos;
    
    NSColor  *backColor;
    NSColor  *overColor;
    NSString *wakeFilePath;
 
    KQueue      watchQueue;
    EventFile   file;
    bool        continuePolling;
}

@property (assign) IBOutlet NSWindow *window;

- (void)release;
- (void)fileChanged;
@end

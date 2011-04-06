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
    
    NSInteger viewBegin;
    NSInteger viewEnd;
    NSColor  *backColor;
    NSString *wakeFilePath;

    KQueue      watchQueue;
    EventFile   file;
    bool        continuePolling;
}

- (void)release;
- (void)fileChanged;
@end

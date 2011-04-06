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
    NSImageView *imageView;
    NSURL   *imageUrl;
    FSEventStreamRef fileWatcher;
    
    NSInteger viewBegin;
    NSInteger viewEnd;
    NSString *wakeFilePath;

    KQueue      watchQueue;
    EventFile   file;
}
@property (assign) IBOutlet NSImageView *imageView;
@property (copy)   NSURL *imageUrl;

- (void)release;
- (void)fileChanged;
@end

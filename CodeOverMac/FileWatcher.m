//
//  FileWatcher.m
//  CodeOverMac
//
//  Created by Vincent Berthoux on 05/04/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import "FileWatcher.h"
#include <sys/types.h>
#include <sys/event.h>
#include <sys/time.h>

@implementation FileWatcher
- (void)pollDispatch
{
    NSDictionary * arguments = 
        [[NSUserDefaults standardUserDefaults] 
            volatileDomainForName:NSArgumentDomain];

    NSString* pid = [arguments objectForKey:@"p"];
    
    wakeFilePath = [NSString stringWithFormat:@"/tmp/overviewFile%@.txt", pid];
    [wakeFilePath retain];
    
    NSLog(@"Watching %@", wakeFilePath);
    watchQueue = kqueue();
    if (watchQueue == -1)
    {
        NSLog(@"Error : can't create watchQueue\n");
        return;
    }

    file = open([wakeFilePath UTF8String], O_EVTONLY);
    if (file == -1)
    {
        NSLog(@"Error : can't open watched file\n");
        return;
    }

    struct kevent event;
    struct kevent change;

    EV_SET(&change, file, EVFILT_VNODE,
           EV_ADD | EV_ENABLE | EV_ONESHOT,
           NOTE_DELETE | NOTE_EXTEND | NOTE_WRITE | NOTE_ATTRIB,
           0, 0);
 
    while (continuePolling)
    {
        int nev = kevent(watchQueue, &change, 1, &event, 1, NULL);
        if (nev > 0 && (event.fflags & NOTE_EXTEND || event.fflags & NOTE_WRITE))
        {
            [self fileChanged];
        }
    }
}

- (void)awakeFromNib
{
    continuePolling = YES;
    dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT,0),
                   ^{[self pollDispatch];});
}

- (void)fileChanged
{
    NSString *fileContent =
    [NSString stringWithContentsOfFile:wakeFilePath
                              encoding:NSUTF8StringEncoding
                                 error:NULL];
    NSLog(@"Readed :%@ '%@'", wakeFilePath, fileContent);
    NSArray *parts = [fileContent componentsSeparatedByString:@"?"];
    // top
    viewBegin = [[parts objectAtIndex:0] integerValue];
    viewEnd = [[parts objectAtIndex:1] integerValue];
    
    NSString *cleanString =
        [[parts objectAtIndex:2]
            stringByTrimmingCharactersInSet:[NSCharacterSet whitespaceAndNewlineCharacterSet]];
    
    NSImage *img = [[NSImage alloc] initWithContentsOfFile:cleanString];

    dispatch_async(dispatch_get_main_queue(), ^{
        [self setImage:img];
        [img release];
    });
}

- (void)mouseUp:(NSEvent *)theEvent
{
}

- (void)release
{
}
@end


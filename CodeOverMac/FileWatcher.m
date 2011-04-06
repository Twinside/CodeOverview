//
//  FileWatcher.m
//  CodeOverMac
//
//  Created by Vincent Berthoux on 05/04/11.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import "FileWatcher.h"

void fileCallbackInfo( ConstFSEventStreamRef streamRef,
                      void *clientCallBackInfo,
                      size_t numEvents,
                      void *eventPaths,
                      const FSEventStreamEventFlags eventFlags[],
                      const FSEventStreamEventId eventIds[] )
{
    [(FileWatcher*)clientCallBackInfo fileChanged];
}

@implementation FileWatcher
@synthesize imageView;
@synthesize imageUrl;

- (void)awakeFromNib
{
    //self = [NSObject init];
    NSDictionary * arguments = [[NSUserDefaults standardUserDefaults] volatileDomainForName:NSArgumentDomain];
    NSString* pid = [arguments objectForKey:@"p"];
    
    wakeFilePath = [NSString stringWithFormat:@"/tmp/overviewFile%@.txt", pid];
    [wakeFilePath retain];
    
    NSString *pathToWatch = @"/tmp/";
    CFArrayRef pathsToWatch = CFArrayCreate(NULL, (const void **)&pathToWatch, 1, NULL);
    void *callbackInfo = self; // could put stream-specific data here.
    CFAbsoluteTime latency = 3.0; /* Latency in seconds */
    
    
    /* Create the stream, passing in a callback */
    fileWatcher = FSEventStreamCreate(NULL,
                                      &fileCallbackInfo,
                                      callbackInfo,
                                      pathsToWatch,
                                      kFSEventStreamEventIdSinceNow, /* Or a previous event ID */
                                      latency,
                                      kFSEventStreamCreateFlagNone /* Flags explained in reference */
                                      );
    FSEventStreamScheduleWithRunLoop(fileWatcher, CFRunLoopGetCurrent(), kCFRunLoopDefaultMode);
    FSEventStreamStart(fileWatcher);
    //return self;
}

- (void)fileChanged
{
    NSString *fileContent =
    [NSString stringWithContentsOfFile:wakeFilePath
                              encoding:NSUTF8StringEncoding
                                 error:NULL];
    NSArray *parts = [fileContent componentsSeparatedByString:@"?"];
    // top
    viewBegin = [[parts objectAtIndex:0] integerValue];
    viewEnd = [[parts objectAtIndex:1] integerValue];
    
    // filename
    [self setImageUrl:[NSURL URLWithString:[parts objectAtIndex:2]]];
}

- (void)mouseUp:(NSEvent *)theEvent
{
    NSLog(@"Meh\n");
}

- (void)release
{
   // FSEventStreamStop(fileWatcher);
   // FSEventStreamInvalidate(fileWatcher);
   // FSEventStreamRelease(fileWatcher);
}
@end

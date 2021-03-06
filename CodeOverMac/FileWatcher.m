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
@synthesize window;

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
        if (nev > 0 && (event.fflags & NOTE_EXTEND))// || event.fflags & NOTE_WRITE))
        {
            [self fileChanged];
        }
    }
}

- (void)drawRect:(NSRect)aRect
{
    // draw the background to the good color
    NSRect frame = [self bounds];

    [backColor setFill];
    NSRectFill( frame );
    [[NSColor blackColor] setFill];

    // normally draw the imageview
    [super drawRect:aRect];
    
    // draw the current rendering square
    NSRect viewRect = frame;
    NSSize imageSize = [[self image] size];
    
    
    viewRect.size.height = (viewEnd - viewBegin) * frame.size.height 
                         / imageSize.height;
    viewRect.origin.y = (frame.size.height - viewRect.size.height) // image view
                      - (viewBegin * frame.size.height / imageSize.height);
    
    
    [overColor setFill];
    NSRectFillUsingOperation(viewRect, NSCompositeSourceOver);
    [[NSColor blackColor] setFill];
}

- (void)awakeFromNib
{
    backColor = nil;
    overColor = nil;
    continuePolling = YES;
    NSDictionary * arguments = 
    [[NSUserDefaults standardUserDefaults] 
     volatileDomainForName:NSArgumentDomain];
    
    NSString* pid = [arguments objectForKey:@"p"];
    
    [window setTitle:pid];
    
    dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT,0),
                   ^{[self pollDispatch];});
}

+ (NSColor *) colorFromHexRGB:(NSString *)inColorString
                     andAlpha:(CGFloat)alpha
{
	NSColor *result = nil;
	unsigned int colorCode = 0;
	unsigned char redByte, greenByte, blueByte;
	
	if (nil != inColorString)
	{
		NSScanner *scanner =
            [NSScanner scannerWithString:[inColorString substringFromIndex:1]];
		(void) [scanner scanHexInt:&colorCode];	// ignore error
	}
	redByte		= (unsigned char) (colorCode >> 16);
	greenByte	= (unsigned char) (colorCode >> 8);
	blueByte	= (unsigned char) (colorCode);	// masks off high bits
	result = [NSColor
              colorWithCalibratedRed:		(float)redByte	/ 0xff
              green:	(float)greenByte/ 0xff
              blue:	(float)blueByte	/ 0xff
              alpha:alpha];
	return result;
}

- (void)fileChanged
{
    NSString *fileContent;
    NSArray *parts;
    
    int maxRetryCount = 10;
    do
    {
        fileContent =
            [NSString stringWithContentsOfFile:wakeFilePath
                                      encoding:NSUTF8StringEncoding
                                         error:NULL];
        parts = [fileContent componentsSeparatedByString:@"?"];
        
        
        if ([@"quit" isEqualTo:[fileContent stringByTrimmingCharactersInSet:
                                [NSCharacterSet whitespaceAndNewlineCharacterSet]]])
            exit(0);
        
        if (maxRetryCount-- <= 0) return;
        
        // there is some timing issue, if misread, retry.
    } while (!parts || [parts count] < 8);

    viewBegin = [[parts objectAtIndex:0] integerValue];
    viewEnd = [[parts objectAtIndex:1] integerValue];
    
    [backColor release];
    backColor = [[FileWatcher colorFromHexRGB:[parts objectAtIndex:2]
                                     andAlpha:1.0] retain];
    
    [overColor release];
    overColor = [[FileWatcher colorFromHexRGB:[parts objectAtIndex:3]
                                     andAlpha:0.6f] retain];
    
    // ignore 4th value
    senderWindowXpos = [[parts objectAtIndex:5] integerValue];
    senderWindowYpos = [[parts objectAtIndex:6] integerValue];
    
    NSString *cleanString =
        [[parts objectAtIndex:[parts count] - 1]
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


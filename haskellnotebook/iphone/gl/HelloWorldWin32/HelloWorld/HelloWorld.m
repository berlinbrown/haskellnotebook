//
// HelloWorldApp.h
// HelloWorld
//
// Created by PJ Cabrera on 08/18/2008.
// Copyright PJ Cabrera 2008. All rights reserved.
//
#import <Foundation/Foundation.h>
#import <UIKit/UIKit.h>

#import "HelloWorldApp.h"

int main(int argc, char *argv[]) {
	// This autorelease pool is managed by the UI's event dispatcher. It autoreleases
	// any allocated objects that fall out of scope. The iPhone's implementation of
	// Objective-C 2.0 does not have garbage collection yet, but autorelease pools and
	// proper release of allocated objects is still a good practice.
	NSAutoreleasePool *pool = [[NSAutoreleasePool alloc] init];
	// This is where UI execution gets started. This instantiates the UIApplication
	// subclass and UIApplicationDelegate subclass specified as string parameters.
	// The name of an UIApplication subclass can be passed as both UIApplication and
	// UIApplicationDelegate.
	UIApplicationMain(argc, argv, @"HelloWorldApp", @"HelloWorldApp");
	// Force release of the autorelease pool and all objects still allocated.
	[pool release];
	return 0;
} // End of the // 

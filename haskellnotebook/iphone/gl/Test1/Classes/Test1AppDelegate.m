//
//  Test1AppDelegate.m
//  Test1
//
//  Created by Berlin Brown on 9/1/09.
//  Copyright __MyCompanyName__ 2009. All rights reserved.
//

#import "Test1AppDelegate.h"
#import "EAGLView.h"

@implementation Test1AppDelegate

@synthesize window;
@synthesize glView;

- (void)applicationDidFinishLaunching:(UIApplication *)application {
    
	glView.animationInterval = 1.0 / 60.0;
	[glView startAnimation];
}


- (void)applicationWillResignActive:(UIApplication *)application {
	glView.animationInterval = 1.0 / 5.0;
}


- (void)applicationDidBecomeActive:(UIApplication *)application {
	glView.animationInterval = 1.0 / 60.0;
}


- (void)dealloc {
	[window release];
	[glView release];
	[super dealloc];
}

@end

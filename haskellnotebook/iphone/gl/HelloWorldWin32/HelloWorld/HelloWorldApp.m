//
// HelloWorldApp.m
// HelloWorld
//
// Created by PJ Cabrera on 08/18/2008.
// Copyright PJ Cabrera 2008. All rights reserved.
//
#import "HelloWorldApp.h"


@implementation HelloWorldApp
- (void)applicationDidFinishLaunching:(UIApplication *)application {

	// Create a "full-screen" window to hold the UI.
	window = [[UIWindow alloc] initWithContentRect:
		[UIHardware fullScreenApplicationContentRect] ];


	// Create a view to hold the window contents.
	contentView = [[UIView alloc] initWithFrame: CGRectMake(0.0f, 0.0f, 320.0f, 480.0f)];

	// Create a navigation bar for the top of the view, with two buttons.
	// The buttons do not do anything in this example.
	nav = [[UINavigationBar alloc] initWithFrame: CGRectMake(0.0f, 0.0f, 320.0f, 48.0f)];
	[nav pushNavigationItem:[[UINavigationItem alloc] initWithTitle:@"Hello World"]];
	[nav showButtonsWithLeftTitle: @"Left" rightTitle: @"Right"];
	[nav setBarStyle: 0];
	[contentView addSubview: nav];

	// Create a text view, this is a basic text editor, with incorporated keyboard.
	text = [[UITextView alloc] initWithFrame: CGRectMake(0.0f, 48.0f, 320.0f, 480.0f)];
	[text setText: [[NSString alloc]
		initWithString: @"Hello World\nCocoa Touch Application"]];
	[contentView addSubview: text];

	// UIWindow con only hold one view, the contentView. The contentView can hold many
	// navigation controllers, which control the different views in your application.
	window.contentView = contentView;

	// These three instructions effectively show the window.
	[window orderFront: self];
	[window makeKey: self];
	[window _setHidden: NO];
}


- (void)dealloc {
	// Release the UI elements as they were allocated
	[text release];
	[nav release];
	[contentView release];
	[window release];

	// And don't forget to call the parent class dealloc
	[super dealloc];
}
@end

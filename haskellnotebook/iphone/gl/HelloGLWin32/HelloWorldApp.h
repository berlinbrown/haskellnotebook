//
//  HelloWorldApp.h
//  HelloWorld
//
//  Created by PJ Cabrera on 08/18/2008.
//  Copyright PJ Cabrera 2008. All rights reserved.
//

#import <UIKit/UIKit.h>

#import <OpenGLES/EAGL.h>
#import <OpenGLES/ES1/gl.h>
#import <OpenGLES/ES1/glext.h>

@interface HelloWorldApp : UIApplication {
    UIWindow *window;
    UIView *contentView;
    UINavigationBar *nav;
    UITextView *text;
}

@end


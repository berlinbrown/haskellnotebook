CC=/usr/local/bin/arm-apple-darwin9-gcc
CXX=/usr/local/bin/arm-apple-darwin9-g++
LD=$(CC)

CFLAGS=-I/usr/local/lib/gcc/arm-apple-darwin9/4.2.1/include \
	-isysroot /usr/local/iphone-sysroot

LDFLAGS=-framework CoreFoundation -framework Foundation -framework UIKit \
	-framework OpenGLES \
	-lobjc -bind_at_load -isysroot /usr/local/iphone-sysroot

#LDFLAGS = -arch arm -lobjc
#LDFLAGS += -framework CoreFoundation
#LDFLAGS += -framework Foundation
#LDFLAGS += -framework UIKit
#LDFLAGS += -framework CoreGraphics
#LDFLAGS += -framework AddressBookUI
#LDFLAGS += -framework AddressBook
#LDFLAGS += -framework QuartzCore
#LDFLAGS += -framework GraphicsServices
#LDFLAGS += -framework CoreSurface
#LDFLAGS += -framework CoreAudio
#LDFLAGS += -framework Celestial
#LDFLAGS += -framework AudioToolbox
#LDFLAGS += -framework WebCore
#LDFLAGS += -framework WebKit
#LDFLAGS += -framework SystemConfiguration
#LDFLAGS += -framework CFNetwork
#LDFLAGS += -framework MediaPlayer
#LDFLAGS += -framework OpenGLES
#LDFLAGS += -framework OpenAL
#LDFLAGS += -bind_at_load
#LDFLAGS += -multiply_defined suppress
#LDFLAGS += -march=armv6
#LDFLAGS += -mcpu=arm1176jzf-s


all:	HelloWorld.app

HelloWorld.app:	HelloWorld Info.plist
	mkdir -p HelloWorld.app
	cp Info.plist HelloWorld Default.png icon.png HelloWorld.app/

HelloWorld:	HelloWorld.o HelloWorldApp.o
	$(LD) $(LDFLAGS) -o $@ $^

%.o:	%.m
	$(CC) -c $(CFLAGS) $(CPPFLAGS) $< -o $@

clean:
	rm -rf *.o HelloWorld HelloWorld.app


all: simple_http

simple_http: SimpleHTTPServer.class SimpleHTTPServerThread.class
	java SimpleHTTPServer

SimpleHTTPServer.class:
	javac SimpleHTTPServer.java

SimpleHTTPServerThread.class:
	javac SimpleHTTPServerThread.java

clean:
	rm -vf *.class

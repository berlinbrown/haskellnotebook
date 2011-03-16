###
### Berlin Brown
###
### Date: 12/10/2004
###

/// Scripts Revisited

- Here is an example model of how one would use the batch scripts

- From the 'src' directory building the application

 ** 1. To build the DLL **
 	|					|
 	| type build.bat 	|
 	|					|
 	
 		** 2. To Build the Test Cases **
 			|				|
 			| type run.bat 	|
 			|				|
 			
	** Optional - Clean **
 		|					|
 		| type clean.bat 	|
 		|					|
 		
	** Optional - Get lisp directory up to date (copy DLL to lisp workspace) **
		|				|
		| type lisp.bat |
		|				|
		
- From the lisp workspace directory building the library
	** Type in the clisp prompt **
		|					|
		| type load d.lisp 	|
		|					|
		
		d.lisp contains the test case for the library
		
		** Optional - Type ab.bat to copy the lisp library
				to the CVS Eclipse Directory **
				|				|
				| type ab.bat 	|
				|				|

/// Library DLL

- This library is included in a DLL, for example widget-toolkit.dll
- All that is need to run from lisp is placing the dll in the
	same location as you would run your lisp code.

/// Win32 Scripts

- There are a lot of batch scripts used to compile and run
	this system, normally 'build.bat' is a good place to start

/// Initial Commit

- Welcome to the WidgetToolkit for Win32 Clisp
- This is a Win32 based GUI library for lisp
- enjoy
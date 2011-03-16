///
/// Author: Berlin Brown
///
/// Date: 12/10/2004
///
/// Note: see:
///		http://clisp.cons.org/impnotes/dffi.html
///		for more info
/*
 *	typedef struct {
 * 		int quot;
 *		int rem;
 *	} div_t;
 * 
 * extern div_t div (int numer, int denom);
 * 
 * (default-foreign-language :stdc)
 * (def-call-out div (:arguments (numer int) (denom int))
 *     (:return-type div_t))
 *
 */

#include "dll.h"
#include <windows.h>
#include <stdio.h>
#include <stdlib.h>


extern HINSTANCE GetDLLHInstance();

static HINSTANCE gDLLHInstance = NULL;

///
/// Function Called by lisp library
///
/// to call: (define-func widget-helloworld "HelloWorld")
///
DLLIMPORT void TestMessageBox() {
	
    MessageBox (0, "MessageBox Test\n", "Hi", MB_ICONINFORMATION); 
    
} // end of the function //

///
/// C-STRING
///		This type corresponds to what C calls char*
////
DLLIMPORT void TestMessageBoxStr(char *str) {
	
	char buf[256];
	sprintf(buf, "MessageBox - String Test: %s\n", str);	
	MessageBox (0, buf, "Hi", MB_ICONINFORMATION);	
		
} // end of the function ///

HINSTANCE GetDLLHInstance() {
	
	return gDLLHInstance;
	
} // end of the function //


///
///  Description: DLL Main
///
BOOL APIENTRY DllMain (HINSTANCE hInst,
                       DWORD reason,
                       LPVOID reserved)
{
    switch (reason)
    {
      case DLL_PROCESS_ATTACH:
      	// The DLL is being mapped into the 
      	// process's address space.
      	gDLLHInstance = hInst;
      	
        break;

      case DLL_PROCESS_DETACH:      
      	// The DLL is being unmapped from the 
      	// process's address space. 
        break;

      case DLL_THREAD_ATTACH:
      	// A thread is being created. 
        break;
        
      case DLL_THREAD_DETACH:
      	// A thread is exiting cleanly. 
        break;
        
    } // end of switch //

    return TRUE;
    
} /// end of the function  ///

///
/// Author: Berlin Brown
///
/// Widget Windows.c
///
///
/// $Id: WidgetWindows.c,v 1.15 2004/12/21 08:01:52 bigbinc Exp $
///
/// Note:
///  A lot of the syntax shown here involves converting LISP
///  types defined by the FFI to Win32 types.
///
/// see widget-toolkit.lisp
///
/// Lisp FFI:
///
/// 	 http://clisp.cons.org/impnotes/dffi.html
///
/// also, see Win32 - COM msdn:
/// 	
/// 	http://msdn.microsoft.com/library/default.asp?url=/library/en-us/dnanchor/html/anch_win32com.asp
///
/// or
///
///		http://msdn.microsoft.com/library/default.asp?url=/library/en-us/winprog/winprog/functions_by_category.asp
///
/// see WINNT.h for defined types
///
///
/// We will use two ways of getting the HINSTANCE,
/// 	g_hInst = (HINSTANCE)GetModuleHandle(NULL or 0); or Get the DLL HInstance
///

#include <windows.h>
#include <winnt.h>

#include "WidgetWin.h"
#include "WidgetWindows.h"

extern HINSTANCE GetDLLHInstance();
static char g_szClassName[] = "MyWindowClass";

///
/// Class
///
#define IDC_MAIN_TEXT		1001

///
/// Description:
// 		WidgetMessageBox
///
DLLIMPORT void WidgetMessageBox(char *str, char *title) {
	
	char buf[1024];
	sprintf(buf, "%s\n", str);	
	MessageBox (0, buf, title, MB_ICONINFORMATION);
		
} // end of the function ///

DLLIMPORT void TestX(WNDCLASSEX *b) {						
} // end of the function

///
/// Simple Windows quit message - Wrapper
///
DLLIMPORT void WidgetPostQuitMessage(int nExitCode) {
		
	PostQuitMessage(nExitCode);
	
} // end of the function

DLLIMPORT int WidgetDestroyWindow(void *_hwnd) {
	
	return DestroyWindow((HWND)_hwnd);
	
} // end of the function //

DLLIMPORT long WidgetDefWindowProc(void *hwnd, 
					unsigned int Message, unsigned int wParam, long lParam) {

	return (long)DefWindowProc(hwnd, Message, wParam, lParam);
			
} // end of the function //

DLLIMPORT int WidgetGetLoWord(unsigned long l) {
	return LOWORD(l);
} /// end of the function ///

DLLIMPORT int WidgetGetHiWord(unsigned long l) {
	return HIWORD(l);
} /// end of the function ///


///
/// Description:
///
/// Windows Processing
///
LRESULT CALLBACK WndProc(HWND hwnd, UINT Message, WPARAM wParam, LPARAM lParam)
{
		
	HINSTANCE g_hInst = GetDLLHInstance();
	
   switch(Message)
   {
      case WM_CREATE:
		break;
      case WM_SIZE:
         if(wParam != SIZE_MINIMIZED)
            MoveWindow(GetDlgItem(hwnd, IDC_MAIN_TEXT), 0, 0, LOWORD(lParam),
               HIWORD(lParam), TRUE);
      break;
      case WM_SETFOCUS:
      break;
      case WM_COMMAND:
      break;
      case WM_CLOSE:
         DestroyWindow(hwnd);
      break;
      case WM_DESTROY:
         PostQuitMessage(0);
      break;
      default:
         return DefWindowProc(hwnd, Message, wParam, lParam);
         
   } // end of the switch //
   
   return 0;
} // end of the function //


DLLIMPORT unsigned int WidgetSizeofWND(void) {
	
	return sizeof(WNDCLASSEX);
	
} // end of the function

DLLIMPORT void* WidgetDefaultIcon(void) {
		
	return LoadIcon(NULL, IDI_APPLICATION);		
		
} // end of the function

DLLIMPORT void* WidgetDefaultCursor(void) {
	
	return LoadCursor(NULL, IDC_ARROW);
	
} // end of the function //

DLLIMPORT void* WidgetDefaultBrush(void) {
	
	return (HBRUSH)GetStockObject( WHITE_BRUSH );
	
} // end of the function //

///
/// Inputs: ptr = INT value
/// 
DLLIMPORT void TestLispPointer(void *ptr) {
	
	int *x = ptr;
	char buf[80];
	sprintf(buf, "Test get LISP Pointer: 0x%x - %d", (unsigned int)ptr, (int)*x);
	MessageBox(0, buf, "Test!",
		MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL);        	        	
	
} // end of the function //

DLLIMPORT void TestGetWNDCLASS(LISP_WNDCLASSEX *ptr) {
	
	MessageBox(0, "001: Test Get LISP Pointer", "Test!",
		MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL);        	        	
	
} // end of the function //

///
/// DLLIMPORT void TestWNDCLASS(TEST_WND_PTR *ptr) {
///
/// OUTPUT:
///
///	"Hello Function Pointer" 
/// from the LISP function
///
DLLIMPORT void TestWNDCLASS(void *ptr) {
	
	TEST_WND *local = ptr;	
	char buf[80];
	
	sprintf(buf, "002: Test Get LISP Pointer - 0x%x : 0x%d", 
			local, local->style);	
	local->lpfnWndProc(NULL, 22, 33, 40);
				
	MessageBox(0, buf, "Test!",
		MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL);		
	
} // end of the function //

///
/// Description:
///
/// Get the value of the HINSTANCE used by this particular
/// DLL, to be used by the Win32 functions
///
DLLIMPORT void* WINAPI WidgetGetHINSTANCE() {
	
	return (void *)GetDLLHInstance();
	
} // end of the function //

///
/// Convert from Pixel to DIALOG values
/// 	Note, you can also use the MapDialogRect
/// 	(There may be a better solution for this code)
///	
DLLIMPORT int WidgetPixelDialogCX(int _x) {
		
	unsigned short vx;
	
   	vx = WidgetGetLoWord(GetDialogBaseUnits());
   	
   	return (int)(_x / (vx / 4));
	
} // end of the function //

DLLIMPORT int WidgetPixelDialogCY(int _y) {
	
	unsigned short vy;
	
	vy = WidgetGetLoWord(GetDialogBaseUnits());
	
   	return (int)(_y / (vy / 4));
	
} // end of the function ///

DLLIMPORT int WidgetCopyDialogString(unsigned short *ptr, const char *str) {
		
	int nChar = 0;
	do {
		
       if (*str == '\t')
           *ptr++ = (WORD)' ';
       else
           *ptr++ = (WORD)*str;
       nChar++;
       
   } while (*str++);

   return nChar;
   
} /// end of the function ///

////
//// ;; (get-own-c-float (FFI:C-VAR-ADDRESS my-c-var))
///

///
/// Description:
/// 
/// Redfine this CreateWindow Function
///
/// WINUSERAPI HWND WINAPI CreateWindowExW(DWORD,LPCWSTR,LPCWSTR,DWORD,int
///   ,int,int,int,HWND,HMENU,HINSTANCE,LPVOID);
///
/// where DWORD = typedef ULONG_PTR DWORD_PTR, *PDWORD_PTR = unsinged long
///
/// and typedef const TCHAR *LPCTSTR :
///   LPCTSTR = Const Pointer to a Char Pointer*
///
/// Note: removed WINUSERAPI
///

DLLIMPORT void* WINAPI WidgetCreateWindowID(
	const char *lpClassName,
	const char *lpWindowName,
    unsigned long dwStyle,
    unsigned long x,
    unsigned long y,
    unsigned long nWidth,
    unsigned long nHeight,
    void *hWndParent,
	unsigned long _id,
	void *hInstance,
    void *lpParam) {    	
		    
    //
    // This object will be called LISP world
    // make sure to convert the type to (VOID *)
    //
   	HINSTANCE g_hInst = GetDLLHInstance();
	HWND h = CreateWindow(
      	lpClassName,
     	lpWindowName,
		dwStyle,
		x, y, 
      	nWidth, nHeight,		
		(HWND)hWndParent,
	    (HMENU)_id,
	    g_hInst,
	    NULL);	    
	
	return (void *)h;
											
} // end of function  ///

DLLIMPORT void* WINAPI WidgetCreateWindow(
	const char *lpClassName,
	const char *lpWindowName,
    unsigned long dwStyle,
    unsigned long x,
    unsigned long y,
    unsigned long nWidth,
    unsigned long nHeight,
    void *hWndParent,
	void *hMenu,
	void *hInstance,
    void *lpParam) {    	
		
	LISP_HWND *__hWndParent = hWndParent;
	LISP_HWND *__hMenu = hMenu;
	/// LISP_HWND *__hInstance = hInstance; // deprecated
		    
    //
    // This object will be called LISP world
    // make sure to convert the type to (VOID *)
    //
   	HINSTANCE g_hInst = GetDLLHInstance();
	HWND h = CreateWindow(
      	lpClassName,
     	lpWindowName,
		dwStyle,
		x, y, 
      	nWidth, nHeight,		
		((__hWndParent == NULL) ? NULL : __hWndParent->_hwnd),
	    ((__hMenu == NULL) ? NULL :__hMenu->_hwnd),
	    g_hInst,
	    NULL);
	
	return (void *)h;
											
} // end of function  ///


DLLIMPORT void* WINAPI WidgetCreateWindowEx(
	unsigned long dwExStyle,
	const char *lpClassName,
	const char *lpWindowName,
    unsigned long dwStyle,
    unsigned long x,
    unsigned long y,
    int nWidth,
    int nHeight,
    void *hWndParent,
	void *hMenu,
	void *hInstance,
    void *lpParam) {    	
		
	LISP_HWND *__hWndParent = hWndParent;
	LISP_HWND *__hMenu = hMenu;
	/// LISP_HWND *__hInstance = hInstance; // deprecated
		    
    //
    // This object will be called LISP world
    // make sure to convert the type to (VOID *)
    //
   	HINSTANCE g_hInst = GetDLLHInstance();
	HWND h = CreateWindowEx(
      	dwExStyle,
      	lpClassName,
     	lpWindowName,
		dwStyle,
		x, y, 
      	nWidth, nHeight,		
		((__hWndParent == NULL) ? NULL : __hWndParent->_hwnd),
	    ((__hMenu == NULL) ? NULL :__hMenu->_hwnd),
	    g_hInst,
	    NULL);
	
	return (void *)h;
											
} // end of function  ///

DLLIMPORT void WidgetGetLastError(const char *lfunc) {
		
	DWORD dw = GetLastError(); 
	TCHAR szBuf[80]; 
    LPVOID lpMsgBuf;
    
	FormatMessage(
        FORMAT_MESSAGE_ALLOCATE_BUFFER | 
        FORMAT_MESSAGE_FROM_SYSTEM,
        NULL,
        dw,
        MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
        (LPTSTR) &lpMsgBuf,
        0, NULL );

    wsprintf(szBuf, 
        "%s failed with error %d: %s", 
			lfunc, dw, lpMsgBuf); 
 
    MessageBox(NULL, szBuf, "Error", MB_OK); 
    LocalFree(lpMsgBuf);	
	
} // end of the function ///

///
/// Create A SimpleWindow
///
/// Description: Create a simple window where
/// a majority of the declarations are formed in LISP
///
DLLIMPORT int WidgetCreateSimpleWin(void *ptrWNDCLASS, void *ptrLISPHWND) {
	
	LISP_WNDCLASSEX *pwnd = ptrWNDCLASS;		
	HINSTANCE g_hInst = GetDLLHInstance();		
  	void *hwnd;
  	MSG Msg;
  	BOOL bSuccess = FALSE;
  	const char *_className = NULL;
  	char buf[255];
											  	
  	hwnd = (void *)ptrLISPHWND;
  	if (hwnd == NULL) {		
      MessageBox(0, "Window Creation Failed!", "Error!",
         MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL);         
      return 0;      
	} // end of the if ///
	
	_className = (pwnd == NULL) ? "InvalidWNDClassObject" : pwnd->lpszClassName;
	
	ShowWindow(hwnd, 1);
  	UpdateWindow(hwnd);
   
	while(GetMessage(&Msg, NULL, 0, 0)) {   	
		TranslateMessage(&Msg);
    	DispatchMessage(&Msg);      
	} // end of the while //
			
	if (!UnregisterClass(_className, g_hInst)) {
		
		sprintf(buf, "Unregister Failed : [ Class-Name Value: %s %x]",
				 _className, (unsigned int)g_hInst);
		
		MessageBox(0, buf, "Error!",
        	MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL);        	        	
		        	        	
      	return 0;
  	} // end of the if //  		
	
	return Msg.wParam;
				
} /// end of the function ///

///
/// Description:
///
/// Default Widget MessageBox
///
///
DLLIMPORT int WidgetNewMessageBox(void *v,
		const char *_message, const char *_title, unsigned int _style)
{		
	return MessageBox((HWND)v, _message, _title, _style);
	
} // end of the function //


///
/// Description:
///  Wrapper for GetDlgItem
///
DLLIMPORT void *WidgetGetDlgItem(void *_hwnd, int val) {
	
	return (void *)GetDlgItem((HWND)_hwnd, val);
	
} // end of the function //

///
/// Description:
///
DLLIMPORT int WidgetRegisterClassFull(void *ptrWNDCLASS, const char *gclass) {
	
	LISP_WNDCLASSEX *pwnd = ptrWNDCLASS;
	WNDCLASSEX WndClass;
	HINSTANCE g_hInst = GetDLLHInstance();		
		
	///
	/// Start: Perform Deep Copy of the Struct (see macro)
	///	
	HWND_DEEP_COPY(WndClass,pwnd, g_hInst)
		
	///
	/// End: End of Deep Copy
	///		
	if (!RegisterClassEx(&WndClass)) {
		MessageBox(0, "Window Registration Failed!", "Error!",
        	MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL);        	        	
      	return 0;
  	} // end of the if //  	
  	
} // end of the function //

///
/// Description:
/// 	Wrapper for SetWindowText
///
DLLIMPORT int WidgetSetWindowText(void *_hwnd, const char *str) {
	
	return (int)SetWindowText((HWND)_hwnd, (LPSTR)str);
	
} /// end of the function ///

DLLIMPORT int WidgetMoveWindow(void * _hwnd,
				int _x,int _y, int _w,int _h,int bRep) {
	
	return (BOOL)MoveWindow((HWND)_hwnd,_x,_y,_w,_h,(BOOL)bRep);
	
} /// end of the function //
			
///
/// Description:
/// 	WidgetCreateFrame
///
DLLIMPORT int WidgetCreateSimpleFrame(char *windowTitle, 
				int width, int height, char *initialText) {
	
	WNDCLASSEX WndClass;
  	HWND hwnd;
  	MSG Msg;
  	BOOL bSuccess = FALSE;
	HINSTANCE g_hInst = GetDLLHInstance();	
	
  	WndClass.cbSize        = sizeof(WNDCLASSEX);
  	WndClass.style         = 0;
   	WndClass.lpfnWndProc   = WndProc;
   	WndClass.cbClsExtra    = 0;
  	WndClass.cbWndExtra    = 0;
  	WndClass.hInstance     = g_hInst;
  	WndClass.hIcon         = LoadIcon(NULL, IDI_APPLICATION);
  	WndClass.hCursor       = LoadCursor(NULL, IDC_ARROW);
  	WndClass.hbrBackground = (HBRUSH)GetStockObject( WHITE_BRUSH );
  	WndClass.lpszMenuName  = "MAINMENU";
  	WndClass.lpszClassName = g_szClassName;
  	WndClass.hIconSm       = LoadIcon(NULL, IDI_APPLICATION);
  	
	if (!RegisterClassEx(&WndClass)) {
		MessageBox(0, "Window Registration Failed!", "Error!",
        	MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL);        	        	
      	return 0;
  	} // end of the if //

	hwnd = CreateWindowEx(
      WS_EX_CLIENTEDGE,
      g_szClassName,
      windowTitle,
      WS_OVERLAPPEDWINDOW,
      CW_USEDEFAULT, CW_USEDEFAULT, width, height,
         NULL, NULL, g_hInst, NULL);

	if(hwnd == NULL) {		
      MessageBox(0, "Window Creation Failed!", "Error!",
         MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL);
         
      return 0;      
	} // end of the if ///
	
	///
	/// Get the Intial Window Text
	///
	if (SetWindowText(GetDlgItem(hwnd, IDC_MAIN_TEXT), (LPSTR)initialText))
		bSuccess = TRUE;
			
  	ShowWindow(hwnd, 1);
  	UpdateWindow(hwnd);
   
	while(GetMessage(&Msg, NULL, 0, 0)) {   	
		TranslateMessage(&Msg);
    	DispatchMessage(&Msg);
	} // end of the while //
	
	if (!UnregisterClass(g_szClassName,g_hInst)) {		
		MessageBox(0, "Unregister Failed", "Error!",
        	MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL);        	        	
      	return 0;
  	} // end of the if //  	
	
	return Msg.wParam;
					
} // end of the function //

///
/// End of the C File //
///

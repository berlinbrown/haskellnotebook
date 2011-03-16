///
/// Author: Berlin Brown
///
/// Date: 12/10/2004
///
///
/// Note: include WidgetToolkit.dll
/// in the according directory and link to libWidgetToolkit.lib
///

#include <stdio.h>
#include <stdlib.h>

#include <windows.h>
///
/// include dll library from above
///
#include "../src/dll.h"
#include "../src/WidgetWindows.h"
#include "../src/WidgetWin.h"

#define IDC_MAIN_TEXT		1001

static char g_szClassName[] = "MyWindowClass";
static HINSTANCE g_hInst = NULL;
#define CIDC_MAIN_TEXT   1008

///
/// Test Enables
///
#define _TEST_01		0
#define _TEST_02		0
#define _TEST_03		0
#define _TEST_04		0
#define _TEST_05		1

LRESULT CALLBACK CommWndProc(HWND hwnd, UINT Message, WPARAM wParam, LPARAM lParam);


LRESULT CALLBACK MainDialogProc(HWND hwnd, UINT message,
	WPARAM wParam,LPARAM lParam) {
	
	
	switch(message) {
		
		case WM_INITDIALOG:
			return 0;
		break;
				
		case WM_COMMAND:					
		break;		
		case WM_DESTROY:
				PostQuitMessage(0);
			return TRUE;
						
		case WM_CLOSE:
				/// EndDialog(hwnd, TRUE);			
				PostQuitMessage(0);
			return 1;
		break;
		
		default:
				return 0;
				
	} // end of the switch ///
	   
   return 0;
   
} // end of the function ///


///
/// Description:
///
/// Test the creation of a dialog box
/// This is modal based dialog box, doesnt allow interaction
/// 
/// Meaning of DIALOG Creation
///
/// 
///		0x0080	Button
///		0x0081	Edit
///		0x0082	Static
///		0x0083	List box
///		0x0084	Scroll bar
///		0x0085	Combo box
///
///
/// Note: you can add the WS_EX_STATICEDGE tag to the extended style
///		also try WS_EX_TOOLWINDOW
///
int test_005(void) {
   	
	HINSTANCE hInst = WidgetGetHINSTANCE();
	unsigned short *dlgTemplate;
	unsigned short *ptr;	
		
	unsigned long dialogStyle;
	unsigned short *ptrNumItems;
	
	
	///
	/// For the DLG Template
	///  here is an example from a RC file
	///
	/// ABOUTDLG DIALOG 0, 0, 155, 102
	///		STYLE DS_MODALFRAME | DS_3DLOOK | WS_POPUP | WS_VISIBLE | WS_CAPTION | WS_SYSMENU
 	///		CAPTION "This is my About Box"
	/// FONT 8, "MS Sans Serif"
 	/// {
    ///		CONTROL "&OK", IDOK, "BUTTON", BS_DEFPUSHBUTTON | BS_CENTER | WS_CHILD | WS_VISIBLE | WS_TABSTOP, 6, 83, 50, 14
    ///		CONTROL "&Cancel", IDCANCEL, "BUTTON", BS_PUSHBUTTON | BS_CENTER | WS_CHILD | WS_VISIBLE | WS_TABSTOP, 95, 83, 50, 14
    ///		CONTROL "About This Program", -1, "button", BS_GROUPBOX | WS_CHILD | WS_VISIBLE | WS_GROUP, 5, 5, 144, 73
    ///		CONTROL "Example Program", -1, "static", SS_CENTER | WS_CHILD | WS_VISIBLE, 27, 27, 100, 50
 	/// }
 	///
	dlgTemplate = (unsigned short *)LocalAlloc(LPTR, (2 * 1024));
	ptr = dlgTemplate;
	
	///
	/// Dynamic DIALOG creation is interesting
	/// you will need to create a DLGTEMPLATE struct in memory and then
	/// perform WORD size memory edits
	///
	/// Note: cx/cy = width, height dialog units
	///
	
	dialogStyle = DS_MODALFRAME | DS_3DLOOK | WS_POPUP | WS_VISIBLE | WS_CAPTION | WS_SYSMENU;
	DIALOG_SET_LONG(dialogStyle);	/// Main Style
	DIALOG_SET_LONG(0);			/// Extended Style
   	ptrNumItems = ptr;			/// Change the number items added to box later
		
	unsigned short vx;
   	unsigned short vy;
	   	
   	vx = WidgetPixelDialogCX(640);
   	vy = WidgetPixelDialogCY(480);
   	
	DIALOG_SET_WORD(0);			/// Number of Items
	DIALOG_SET_WORD(10);		/// x
	DIALOG_SET_WORD(10);		/// y
   	DIALOG_SET_WORD(vx); 		/// cx - dialog units
	DIALOG_SET_WORD(vy); 		/// cy - dialog units
	
	DIALOG_SET_WORD(0);        // Menu
	DIALOG_SET_WORD(0);        // Class
   	
   	int nCtTitle = 0;
   	nCtTitle = WidgetCopyDialogString(ptr, "Current Title");   	
   	  	
	HWND hwnd = CreateDialogIndirect(
           hInst,
           (LPDLGTEMPLATE)dlgTemplate,
			NULL,
           (DLGPROC)MainDialogProc);
	
	MSG m;
	
	while(GetMessage(&m, NULL, 0, 0)) {   			   		
   		
		if(!IsDialogMessage(hwnd,&m)) {
		
            TranslateMessage(&m);
            DispatchMessage(&m);
		} /// end of the if ///
	
	} // end of the while //   	
	                      
	LocalFree(LocalHandle(dlgTemplate));
	
	return m.wParam;
	   			
} /// end of the function ///


int WINAPI test_004(void)
{
   WNDCLASSEX WndClass;
   HWND hwnd;
   MSG Msg;

   g_hInst = (HINSTANCE)WidgetGetHINSTANCE();

   WndClass.cbSize        = sizeof(WNDCLASSEX);
   WndClass.style         = 0;
   WndClass.lpfnWndProc   = CommWndProc;
   WndClass.cbClsExtra    = 0;
   WndClass.cbWndExtra    = 0;
   WndClass.hInstance     = g_hInst;
   WndClass.hIcon         = LoadIcon(NULL, IDI_APPLICATION);
   WndClass.hCursor       = LoadCursor(NULL, IDC_ARROW);
   WndClass.hbrBackground = (HBRUSH)(COLOR_WINDOW+1);
   WndClass.lpszMenuName  = NULL;
   WndClass.lpszClassName = "gDialogBox";
   WndClass.hIconSm       = LoadIcon(NULL, IDI_APPLICATION);

   if(!RegisterClassEx(&WndClass))
   {
      MessageBox(0, "Window Registration Failed!", "Error!",
         MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL);
      return 0;
   }

   hwnd = CreateWindowEx(
      WS_EX_CLIENTEDGE,
      g_szClassName,
      "File Editor Example Program",
      WS_OVERLAPPEDWINDOW,
      CW_USEDEFAULT, CW_USEDEFAULT, 320, 240,
      NULL, NULL, g_hInst, NULL);

   if(hwnd == NULL)
   {
      MessageBox(0, "Window Creation Failed!", "Error!",
         MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL);
      return 0;
   }

   ShowWindow(hwnd, 1);
   UpdateWindow(hwnd);

   while(GetMessage(&Msg, NULL, 0, 0))
   {   			   		
      TranslateMessage(&Msg);
      DispatchMessage(&Msg);
   } // end of the while //
   
   
   return Msg.wParam;
} /// end of the function ///


LRESULT CALLBACK CommWndProc(HWND hwnd, UINT Message, WPARAM wParam, LPARAM lParam)
{
   switch(Message)
   {
      case WM_CREATE:
                      
         CreateWindow("EDIT", "",
            WS_CHILD | WS_VISIBLE | WS_HSCROLL | WS_VSCROLL | ES_MULTILINE |
               ES_WANTRETURN,
            CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT,
            hwnd, (HMENU)CIDC_MAIN_TEXT, g_hInst, NULL);

         SendDlgItemMessage(hwnd, CIDC_MAIN_TEXT, WM_SETFONT,
            (WPARAM)GetStockObject(DEFAULT_GUI_FONT), MAKELPARAM(TRUE, 0));
      break;
      case WM_SIZE:
         if(wParam != SIZE_MINIMIZED)
            MoveWindow(GetDlgItem(hwnd, CIDC_MAIN_TEXT), 0, 0, LOWORD(lParam),
               HIWORD(lParam), TRUE);
      break;
      case WM_SETFOCUS:
         SetFocus(GetDlgItem(hwnd, CIDC_MAIN_TEXT));
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
   }
   return 0;
}

//
// long(CALLBACK *LISP_WNDPROC)(void*,unsigned int,unsigned int,long);
//
LRESULT CALLBACK WndProc(HWND hwnd, 
		UINT Message, WPARAM wParam, LPARAM lParam)
{
		
	switch(Message)
   {
      case WM_CREATE:
                      
         CreateWindow("EDIT", "",
            WS_CHILD | WS_VISIBLE | WS_HSCROLL | WS_VSCROLL | ES_MULTILINE |
               ES_WANTRETURN,
            CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT,
            hwnd, (HMENU)IDC_MAIN_TEXT, g_hInst, NULL);

         SendDlgItemMessage(hwnd, IDC_MAIN_TEXT, WM_SETFONT,
            (WPARAM)GetStockObject(DEFAULT_GUI_FONT), MAKELPARAM(TRUE, 0));
      break;
      case WM_SIZE:
         if(wParam != SIZE_MINIMIZED)
            MoveWindow(GetDlgItem(hwnd, IDC_MAIN_TEXT), 0, 0, LOWORD(lParam),
               HIWORD(lParam), TRUE);
      break;
      case WM_SETFOCUS:
         SetFocus(GetDlgItem(hwnd, IDC_MAIN_TEXT));
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
   }
   return 0;
} // end of the function //

void test_003() {
	
	MSG Msg;
	HINSTANCE g_hInst = NULL;
  	WNDCLASSEX WndClass;
	
	g_hInst = WidgetGetHINSTANCE();
	
	WndClass.cbSize        = sizeof(WNDCLASSEX);
    WndClass.style         = 0;
    WndClass.lpfnWndProc   = WndProc;
    WndClass.cbClsExtra    = 0;
    WndClass.cbWndExtra    = 0;
    WndClass.hInstance     = g_hInst;
    WndClass.hIcon         = LoadIcon(NULL, IDI_APPLICATION);
    WndClass.hCursor       = LoadCursor(NULL, IDC_ARROW);
    WndClass.hbrBackground = (HBRUSH)(COLOR_WINDOW+1);
    WndClass.lpszMenuName  = "MAINMENU";
    WndClass.lpszClassName = "gClassName";
    WndClass.hIconSm       = LoadIcon(NULL, IDI_APPLICATION);
		
    if(!RegisterClassEx(&WndClass))
    {
      MessageBox(0, "Window Registration Failed!", "Error!",
         MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL);
      return;
    } // end of the if //
   					
	
	HWND hwnd = CreateWindowEx(
      WS_EX_CLIENTEDGE,
      "gClassName",
      "File Editor Example Program",
      WS_OVERLAPPEDWINDOW,
      CW_USEDEFAULT, CW_USEDEFAULT, 
      320, 240,
      NULL, NULL, g_hInst, NULL);
	
	char buf[80];
	sprintf(buf, " [ -- ] %x\n", (void *)hwnd);
	WidgetMessageBox(buf ,"test");
	
  	if (hwnd == NULL) {
      MessageBox(0, "Window Creation Failed!", "Error!",
         MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL);
         
      return;      
	} // end of the if ///
	
	/// NEW ADDITIONS //	  
	if (SetWindowText(GetDlgItem(hwnd, IDC_MAIN_TEXT), (LPSTR)"TEST"));
	
	ShowWindow(hwnd, 1);
  	UpdateWindow(hwnd);
   
	while(GetMessage(&Msg, NULL, 0, 0)) {   	
		TranslateMessage(&Msg);
    	DispatchMessage(&Msg);      
	} // end of the while //
		
	if (!UnregisterClass("gClassName",g_hInst)) {		
		MessageBox(0, "Unregister Failed", "Error!",
        	MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL);        	        	
      	return;
  	} // end of the if //
	
	return;
			
} // end of the function //

void test_002() {
	
} /// end of the function //


void test_001() {
	
	MSG Msg;
	HINSTANCE g_hInst = NULL;			
	WidgetRegisterClassFull(NULL,"gClassName");
	
	g_hInst = WidgetGetHINSTANCE();
	HWND hwnd = CreateWindowEx(
      WS_EX_CLIENTEDGE,
      "gClassName",
      "File Editor Example Program",
      WS_OVERLAPPEDWINDOW,
      CW_USEDEFAULT, CW_USEDEFAULT, 320, 240,
      NULL, NULL, g_hInst, NULL);
	
	hwnd = WidgetCreateWindowEx(
		WS_EX_CLIENTEDGE,
		"gClassName",
		"WindowName",
	    WS_OVERLAPPEDWINDOW,
	    CW_USEDEFAULT,
  	    CW_USEDEFAULT,
    	640,	
	    480,
    	NULL,
		NULL,
		NULL,
    	NULL);
	
	printf(" [ --  ]%x\n", (void *)hwnd);
	
  	if (hwnd == NULL) {
      MessageBox(0, "Window Creation Failed!", "Error!",
         MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL);
         
      return;      
	} // end of the if ///
	
	ShowWindow(hwnd, 1);
  	UpdateWindow(hwnd);
   
	while(GetMessage(&Msg, NULL, 0, 0)) {   	
		TranslateMessage(&Msg);
    	DispatchMessage(&Msg);      
	} // end of the while //
	
	if (!UnregisterClass("gClassName",g_hInst)) {		
		MessageBox(0, "Unregister Failed", "Error!",
        	MB_ICONEXCLAMATION | MB_OK | MB_SYSTEMMODAL);        	        	
      	return;
  	} // end of the if //
	
	return;
	
} // end of the function 


///
/// Test
///
void RunFullSuite() {
	
	#if _TEST_01 == 1
		test_001();
	#endif
					
	#if _TEST_02 == 1
		test_002();
	#endif
				
	
	#if _TEST_03 == 1	
		printf("Test-003\n");
		test_003();
	#endif
			
	#if _TEST_04 == 1	
		printf("Test-004\n");
		test_004();
	#endif
	
	#if _TEST_05 == 1
		printf("Test-005\n");
		 test_005();
	#endif
				
} // end of the function //

///
///
/// main
///
int main(int argc, char *argv[]) {
	
	printf("Widget Test Application\n");
	RunFullSuite();
	      
	return 0;
	
} // end of main ///


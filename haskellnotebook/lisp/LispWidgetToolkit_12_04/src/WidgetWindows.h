///
/// Author: Berlin Brown
///
/// Date: 12/10/2004
///
/// $Id: WidgetWindows.h,v 1.13 2004/12/21 08:01:54 bigbinc Exp $
///
/// WidgetWindows.h
///

#ifndef _WIDGET_WINDOWS_H_
#define _WIDGET_WINDOWS_H_

#if BUILDING_DLL
# define DLLIMPORT __declspec (dllexport)
#else
# define DLLIMPORT __declspec (dllimport)
#endif


#include "WidgetWin.h"

///
/// DLL Functions
///
DLLIMPORT int WidgetCreateSimpleFrame(char *windowTitle, 
				int width, int height, char *initialText);
				
DLLIMPORT void WidgetMessageBox(char *str, char *title);
DLLIMPORT void TestX(WNDCLASSEX *b);

DLLIMPORT unsigned int WidgetSizeofWND(void);
DLLIMPORT void* WidgetDefaultIcon(void);
DLLIMPORT void* WidgetDefaultCursor(void);
DLLIMPORT void* WidgetDefaultBrush(void);


DLLIMPORT void* WINAPI WidgetCreateWindowID(
	const char *lpClassName,
	const char *lpWindowName,
    unsigned long dwStyle,
    unsigned long x,
    unsigned long y,
    unsigned long nWidth,
    unsigned long nHeight,
    void *hWndParent,
	unsigned long hMenu,
	void *hInstance,
    void *lpParam);

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
    void *lpParam);
    
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
    void *lpParam);
    
DLLIMPORT int WidgetCreateSimpleWin(void *ptrWNDCLASS, void *ptrLISPHWND);

DLLIMPORT int WidgetNewMessageBox(void *v,
		const char *_message, const char *_title, unsigned int _style);

DLLIMPORT void WidgetPostQuitMessage(int nExitCode);
DLLIMPORT int WidgetDestroyWindow(void *_hwnd);
DLLIMPORT long WidgetDefWindowProc(void *hwnd,
					unsigned int Message, unsigned int wParam, long lParam);
					
DLLIMPORT int WidgetRegisterClassFull(void *ptrWNDCLASS, const char *gclass);

DLLIMPORT void WidgetGetLastError(const char *lfunc);
DLLIMPORT int WidgetSetWindowText(void *_hwnd, const char *str);
DLLIMPORT int WidgetMoveWindow(void * _hwnd,
				int _x,int _y, int _w,int _h,int bRep);
DLLIMPORT void* WidgetGetDlgItem(void *_hwnd, int val);

DLLIMPORT int WidgetGetLoWord(unsigned long l);
DLLIMPORT int WidgetGetHiWord(unsigned long l);

DLLIMPORT void* WINAPI WidgetGetHINSTANCE();

DLLIMPORT int WidgetPixelDialogCX();
DLLIMPORT int WidgetPixelDialogCY();

DLLIMPORT int WidgetCopyDialogString(unsigned short *ptr, const char *str);

DLLIMPORT void TestPointer(LISP_WNDCLASSEX *ptr);
DLLIMPORT void TestGetWNDCLASS(LISP_WNDCLASSEX *ptr);
DLLIMPORT void TestWNDCLASS(void *ptr);

#endif /// _WIDGET_WINDOWS_H_


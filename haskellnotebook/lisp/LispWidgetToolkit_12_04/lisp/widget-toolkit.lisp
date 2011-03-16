;;;
;;; Author: Berlin Brown
;;;
;;; Date: 12/10/2004
;;;
;;; widget-toolkit.lisp
;;;
;;; also see...
;;; see... http://clisp.cons.org/impnotes/dffi.html
;;;
;;; $Id: widget-toolkit.lisp,v 1.16 2004/12/18 14:35:46 bigbinc Exp $
;;;
;;; clisp - win32 - see WidgetWindows.c
;;;
;;; Note: output-c-functions - outs the function calls to extern
;;;
;;; Needed Win32 Data Structure
;;; HANDLE = void* : Handle represents a v
;;;
;;; WNDCLASSEX
;;; HWND
;;;
;;; NOTE - Mapping Between WIN32 Com objects:
;;;
;;; HANDLE = void* : Handle represents a void *
;;; 
;;; possibly use 'THE' to define types
;;; 
;;; Note: you may try 
;;; (require 'LIB) ....(in-package :LIB) ...(provide "LIB")
;;;-------------------------------------------------------------------

;;; Define our package and include [ ffi ]
(defpackage #:widget-toolkit
  (:use #:common-lisp #:ffi)
  (:export
   #:run-full-testsuite
   ))

;;; define this package
(in-package #:widget-toolkit)

;;; see the ffi clisp notes
(eval-when (compile) (setq FFI:*OUTPUT-C-FUNCTIONS* t))
(default-foreign-language :stdc)

;;;
;;; Include the DLL containing all GUI calls
;;; Make sure this file is located in the current
;;; directory or other PATH
;;;
(defconstant *dll-library-file* "WidgetToolkit.dll")

;;;-------------------------------------------------------------------
;;;
;;; Windows Defines : 
;;; WINUSER.h  / WINDef.h
;;;
;;;-------------------------------------------------------------------
(defconstant *WS_EX_CLIENTEDGE*		512)
(defconstant *WS_OVERLAPPEDWINDOW*	#xcf0000)
(defconstant *CW_USEDEFAULT* 		#x80000000)

(defconstant *COLOR_WINDOW*                 5)
(defconstant *CS_HREDRAW*                   1)
(defconstant *CS_VREDRAW*                   2)
(defconstant *IDC_ARROW*                32512)
(defconstant *SW_SHOW*                    5)

(defconstant *WM_CREATE*           1)
(defconstant *WM_SIZE*             5)
(defconstant *WM_COMMAND*          27)
(defconstant *WM_CLOSE*            #x00000010)
(defconstant *WM_DESTROY*          #x00000002)
(defconstant *WM_PAINT*            #x0000000f)

(defconstant *WS_BORDER*           #x00080000)
(defconstant *WS_CAPTION*          #x00C00000)
(defconstant *WS_CHILD*            #x40000000)
(defconstant *WS_DISABLED*         #x08000000)
(defconstant *WS_EX_CONTROLPARENT* #x00010000)
(defconstant *WS_EX_APPWINDOW*     #x00040000)

(defconstant *WS_MAXIMIZEBOX*      #x00010000)
(defconstant *WS_MINIMIZEBOX*      #x00020000)
(defconstant *WS_OVERLAPPED*       #x00000000)
(defconstant *WS_POPUP*            #x80000000)
(defconstant *WS_SYSMENU*          #x00080000)
(defconstant *WS_THICKFRAME*       #x00040000)
(defconstant *WS_VISIBLE*          #x10000000)

(defconstant *ES_AUTOHSCROLL*        128)
(defconstant *ES_AUTOVSCROLL*        64)
(defconstant *ES_CENTER*     1)
(defconstant *ES_LEFT*       0)
(defconstant *ES_LOWERCASE* 16)
(defconstant *ES_MULTILINE* 4)
(defconstant *ES_NOHIDESEL* 256)
(defconstant *ES_NUMBER* #x2000)
(defconstant *ES_OEMCONVERT* #x400)
(defconstant *ES_PASSWORD* 32)
(defconstant *ES_READONLY* #x800)
(defconstant *ES_RIGHT* 2)
(defconstant *ES_UPPERCASE* 8)
(defconstant *ES_WANTRETURN* 4096)
(defconstant *MOUSE_ATTRIBUTES_CHANGED* #x00000004)

(defconstant *HS_BDIAGONAL*   3)
(defconstant *HS_CROSS*       4)
(defconstant *HS_DIAGCROSS*   5)
(defconstant *HS_FDIAGONAL*   2)
(defconstant *HS_HORIZONTAL*  0)
(defconstant *HS_VERTICAL*    1)

(defconstant *WS_VSCROLL*    #x200000)
(defconstant *WS_HSCROLL*    #x100000)

;;;
;;; Macro for defining the function used from the DLL
;;;
(defmacro def-external-func (lisp-name def-c-name &key arguments return-type)
  `(def-call-out ,lisp-name
     (:name ,def-c-name)
     (:library ,"WidgetToolkit.dll")
     (:arguments ,@arguments)
     (:return-type ,return-type)))

(defmacro define-createwnd (aa ba ca)
  `(make-lispwndclassex
    :cbSize (widget-sizeof-wnd)
    :style 0
    :lpfnWndProc ,aa
    :cbClsExtra 0
    :cbWndExtra 0
    :hInstance nil
    :hIcon (widget-defaulticon)
    :hCursor (widget-defaultcursor)
    :hbrBackground (widget-defaultbrush)
    :lpszMenuName ,ba
    :lpszClassName ,ca
    :hIconSm (widget-defaulticon)
    ))

;;;
;;; Lisp Simple WNDPROC
;;;
;;; For to define WNDPROC
;;; we need LRESULT,HWND,WPARAM,LPARAM
;;;
;;; LONG_PTR = LRESULT = LONG = LPARAM
;;; UINT_PTR = WPARAM = unsigned int
;;;
;;; We are ignoring the Win32 UNICODE
;;;
(defun lisp-wndproc (hwnd message wparam lparam)
  (declare (type ffi:c-pointer hwnd))
  (declare (type ffi:uint message))
  (declare (type ffi:uint wparam))
  (declare (type ffi:long lparam))
  ;;; Check the message value and respond
  (cond ((= message *WM_CREATE*)
	 (print "WM_CREATE")
	 (let ((anx (widget-createwindowid
		"EDIT"
		""
		(logior 
		 *WS_CHILD*
		 *WS_VISIBLE*
		 *WS_HSCROLL*
		 *WS_VSCROLL*
		 *ES_MULTILINE*
		 *ES_WANTRETURN*)
		*CW_USEDEFAULT* *CW_USEDEFAULT*	 
		*CW_USEDEFAULT* *CW_USEDEFAULT*	 
		hwnd
		1001
		NIL
		NIL
		)))
	   ))
	((= message *WM_SIZE*) (print "WM_SIZE")
	 (not (= (widget-movewindow
		  (widget-getdialogitem hwnd 1001) 
		  0 0
		  (widget-getloword lparam)
		  (widget-gethiword lparam)
		  1
		  ) 1)))
	((= message *WM_COMMAND*) (print "WM_COMMAND"))
	((= message *WM_CLOSE*)
	 (print "Closing application")
	(widget-destroywindow hwnd))
	((= message *WM_DESTROY*)
	 (widget-postquitmessage 0))
	(t
	 (let ((res-val
		(widget-defwindowproc hwnd message wparam lparam)))
	   (return-from lisp-wndproc res-val))
	 ))
  ;;; return ;;;
  (let ((lresult 0))
    (declare (type long lresult))
    lresult))

;;;
;;; WNDCLASSEX
;;; note: typedef CONST CHAR *LPCCH,*PCSTR,*LPCSTR;
;;;
;;; typedef WORD *PWORD,*LPWORD, whenever you
;;; see PWORD = WORD*
;;;
;;; invalid: (b (c-pointer (c-function 
;;; invalid:   (:arguments (v int))) d))
;;;
(def-c-struct lispwndclassex
  (cbSize uint)
  (style uint)
  (lpfnWndProc 
   (c-function (:arguments 
		(hwnd ffi:c-pointer)
		(message ffi:uint)
		(wparam ffi:uint)
		(lparam ffi:long))
	       (:language :stdc)
	       (:return-type ffi:long)))
  (cbClsExtra int)
  (cbWndExtra int)
  (hInstance c-pointer)
  (hIcon c-pointer)
  (hCursor c-pointer)
  (hbrBackground c-pointer)
  (lpszMenuName c-string)
  (lpszClassName c-string)
  (hIconSm c-pointer))

;;;
;;; Description:
;;;
;;; Define a struct for the HWND pointer
;;;
;;;
;;; typedef struct __LISP_HWND {
;;;	void *_hwnd;		
;;; } LISP_HWND;
;;;
(def-c-struct lisp-hwnd
  (hwnd ffi:c-pointer))

;;;
;;; __LISP_PTRTCHAR
;;;
(def-c-struct lisptchar
  (ptr-tchar ffi:c-string))

;;;
;;; widget-endl
;;;
(defun widget-endl ()
  (format nil "~C~C" #\Return #\Newline))

;;;
;;; Register Class
;;;
(defun lisp-registerclass (w name-str)
  (with-c-var (a-arg 'lispwndclassex w)
	      (widget-registerclass (c-var-address a-arg) name-str)
	      ))

;;;-------------------------------------------------------------------
;;;
;;; Simple Testing Framework
;;;
;;;-------------------------------------------------------------------
(defun test-set001 ()
  (let* ((cName "gClassName")
	 (mName "Window Name")
	 (title "WinCreate")
	 (simpl (define-createwnd 
		  #'lisp-wndproc mName cName))
	 (register-res (lisp-registerclass simpl cName))
	 (createwn (widget-createwindow-ex
		    *WS_EX_CLIENTEDGE*
		    cName
		    mName
		    *WS_OVERLAPPEDWINDOW*
		    *CW_USEDEFAULT*
		    *CW_USEDEFAULT*
		    640
		    480
		    nil
		    nil
		    nil
		    nil
		    )))    
    (print (widget-setwindowtext (widget-getdialogitem createwn 1001) 
				 (format nil "~A~A~A~A" 
					 "AAA" (widget-endl) 
					 "BBB" (widget-endl))))
    (with-c-var (a-arg 'lispwndclassex simpl)
		(widget-createsimple-win (c-var-address a-arg)
					 createwn))
    ))

(defun run-full-testsuite ()
  (test-set001))

;;;-------------------------------------------------------------------
;;;
;;; DLLIMPORT unsigned int WidgetSizeofWND();
;;; DLLIMPORT void* WidgetDefaultIcon();
;;; DLLIMPORT void* WidgetDefaultCursor();
;;; DLLIMPORT void* WidgetDefaultBrush();
;;;
;;; To get to any member:
;;; (print (slot-value aa 'cbSize)) 
;;;
;;; Default Widget Create Handles: 
;;;
;;; [ DLLIMPORT void TestPointer(LISP_WNDCLASSEX *ptr) ]
;;;
;;;-------------------------------------------------------------------

(def-call-out widget-getloword
  (:name "WidgetGetLoWord")
  (:arguments
   (lx ffi:ulong))
  (:return-type ffi:int)
  (:library "WidgetToolkit.dll"))
(def-call-out widget-gethiword
  (:name "WidgetGetHiWord")
  (:arguments
   (lx ffi:ulong))
  (:return-type ffi:int)
  (:library "WidgetToolkit.dll"))

(def-call-out widget-movewindow
  (:name "WidgetMoveWindow")
  (:arguments
   (hwnd ffi:c-pointer)
   (win-x ffi:int)
   (win-y ffi:int)
   (win-w ffi:int)
   (win-h ffi:int)
   (brepaint ffi:int))
  (:return-type ffi:int)
  (:library "WidgetToolkit.dll"))
(def-call-out widget-gethinstance
  (:name "WidgetGetHINSTANCE")
  (:arguments)
  (:return-type ffi:c-pointer)
  (:library "WidgetToolkit.dll"))

(def-call-out widget-createwindowid
  (:name "WidgetCreateWindowID")
  (:arguments
   (lpClassName ffi:c-string)
   (lpWindowName ffi:c-string)
   (dwStyle ulong)
   (x ffi:ulong)
   (y ffi:ulong)
   (nWidth ffi:ulong)
   (nHeight ffi:ulong)
   (hWndParent ffi:c-pointer)
   (hMenu ffi:ulong)
   (hInstance ffi:c-pointer)
   (lpParam ffi:c-pointer))   
  (:return-type ffi:c-pointer)
  (:library "WidgetToolkit.dll"))

(def-call-out widget-createwindow
  (:name "WidgetCreateWindow")
  (:arguments
   (lpClassName ffi:c-string)
   (lpWindowName ffi:c-string)
   (dwStyle ulong)
   (x ffi:ulong)
   (y ffi:ulong)
   (nWidth ffi:ulong)
   (nHeight ffi:ulong)
   (hWndParent ffi:c-pointer)
   (hMenu ffi:c-pointer)
   (hInstance ffi:c-pointer)
   (lpParam ffi:c-pointer))   
  (:return-type ffi:c-pointer)
  (:library "WidgetToolkit.dll"))

(def-call-out widget-getdialogitem
  (:name "WidgetGetDlgItem")
  (:arguments
   (nhwnd ffi:c-pointer)
   (nval ffi:int))
  (:return-type ffi:c-pointer)
  (:library "WidgetToolkit.dll"))

(def-call-out widget-setwindowtext
  (:name "WidgetSetWindowText")
  (:arguments
   (hwnd ffi:c-pointer)
   (str ffi:c-string))
  (:return-type int)
  (:library "WidgetToolkit.dll"))

(def-call-out widget-getlasterror
  (:name "WidgetGetLastError")
  (:arguments
   (lfunc ffi:c-string))
  (:return-type)
  (:library "WidgetToolkit.dll"))

(def-call-out widget-registerclass
  (:name "WidgetRegisterClassFull")
  (:arguments
   (wndclass ffi:c-pointer)
   (cname ffi:c-string))
  (:return-type int)
  (:library "WidgetToolkit.dll"))

(def-call-out widget-defmsgbox
  (:name "WidgetMessageBox")
  (:arguments
   (wmessage ffi:c-string)
   (wtitle ffi:c-string))
  (:return-type)
  (:library "WidgetToolkit.dll"))

(def-call-out widget-messagebox
  (:name "WidgetNewMessageBox")
  (:arguments
   (whwnd ffi:c-pointer)
   (wmessage ffi:c-string)
   (wtitle ffi:c-string)
   (wstyle ffi:uint))
  (:return-type int)
  (:library "WidgetToolkit.dll"))

(def-call-out widget-postquitmessage
  (:name "WidgetPostQuitMessage")
  (:arguments
   (dwExStyle int))
  (:return-type)
  (:library "WidgetToolkit.dll"))

(def-call-out widget-destroywindow
  (:name "WidgetDestroyWindow")
  (:arguments
   (hwnd ffi:c-pointer))
  (:return-type int)
  (:library "WidgetToolkit.dll"))

(def-call-out widget-defwindowproc
  (:name "WidgetDefWindowProc")
  (:arguments
   (hwnd ffi:c-pointer)
   (msg ffi:int)
   (wparam ffi:uint)
   (lparam ffi:long))
  (:return-type long)
  (:library "WidgetToolkit.dll"))

(def-call-out widget-createwindow-ex
  (:name "WidgetCreateWindowEx")
  (:arguments
   (dwExStyle ulong)
   (lpClassName ffi:c-string)
   (lpWindowName ffi:c-string)
   (dwStyle ulong)
   (x ffi:ulong)
   (y ffi:ulong)
   (nWidth int)
   (nHeight int)
   (hWndParent ffi:c-pointer)
   (hMenu ffi:c-pointer)
   (hInstance ffi:c-pointer)
   (lpParam ffi:c-pointer)
   )
  (:return-type ffi:c-pointer)
  (:library "WidgetToolkit.dll"))

(def-call-out widget-createsimple-win
  (:name "WidgetCreateSimpleWin")
  (:arguments 
   (ptrwnd c-pointer) 
   (ptrhwnd c-pointer))
  (:return-type int)
  (:library "WidgetToolkit.dll"))

(def-call-out widget-testpointer-ptr
  (:name "TestWNDCLASS")
  (:arguments (lisp-wndclass-ptr c-pointer))
  (:return-type)
  (:library "WidgetToolkit.dll"))

(def-call-out widget-testpointer-int
  (:name "TestLispPointer")
  (:arguments (lisp-wndclass-ptr c-pointer))
  (:return-type)
  (:library "WidgetToolkit.dll"))

;;; Note: this test code fails
(def-call-out widget-testpointer-wnd
  (:name "TestGetWNDCLASS")
  (:arguments (lisp-wndclass-ptr c-pointer))
  (:return-type)
  (:library "WidgetToolkit.dll"))

(def-call-out widget-sizeof-wnd
  (:name "WidgetSizeofWND")
  (:arguments)
  (:return-type ffi:uint)
  (:library "WidgetToolkit.dll"))

(def-call-out widget-defaulticon
  (:name "WidgetDefaultIcon")
  (:arguments)
  (:return-type ffi:c-pointer)
  (:library "WidgetToolkit.dll"))

(def-call-out widget-defaultcursor
  (:name "WidgetDefaultCursor")
  (:arguments)
  (:return-type ffi:c-pointer)
  (:library "WidgetToolkit.dll"))

(def-call-out widget-defaultbrush
  (:name "WidgetDefaultBrush")
  (:arguments)
  (:return-type ffi:c-pointer)
  (:library "WidgetToolkit.dll"))

;;;-------------------------------------------------------------------
;;;
;;; End of Function C Defines
;;;
;;;-------------------------------------------------------------------

;;;
;;; End
;;;

;;;-------------------------------------------------------------------
;;;
;;; End of File
;;;
;;;-------------------------------------------------------------------
;;;
;;; Author: Berlin Brown
;;;
;;; Date: 12/14/2004
;;;
;;; $Id$
;;;
;;;
;;; Note: FFI:C-PTR is a keyword denoting a pointer of type x
;;;
(defpackage #:widget-toolkit-test
  (:use #:common-lisp #:ffi)
  (:export #:run-test-suite))

;;; define this package
(in-package #:widget-toolkit-test)

(eval-when (compile) (setq FFI:*OUTPUT-C-FUNCTIONS* t))
(default-foreign-language :stdc)

;;;
;;; Member Functions ///
;;;

;;;
;;; Test WNDPROC
;;;
(defun test-wndproc (hwnd message wparam lparam)
  (declare (type ffi:c-pointer hwnd))
  (declare (type ffi:uint message))
  (declare (type ffi:long lparam))
  ;;; return ;;;
  (let ((lresult -1))
    (declare (type long lresult))
    (print "Hello Function Pointer")
    lresult))

;;;
;;; WNDCLASSEX
;;; note: typedef CONST CHAR *LPCCH,*PCSTR,*LPCSTR;
;;;
;;; typedef WORD *PWORD,*LPWORD, whenever you
;;; see PWORD = WORD*
;;;
;;;
;;; invalid: (b (c-pointer (c-function 
;;; invalid:   (:arguments (v int))) d))
;;;
(def-c-struct test-wndclassex
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
;;; End of WNDClass
;;;

(def-c-struct test-simpl
  (cbSize uint)
  (lpfnWndProc 
   (c-function (:arguments 
		(hwnd ffi:c-pointer)
		(message ffi:uint)
		(wparam ffi:uint)
		(lparam ffi:long))
	       (:language :stdc)
	       (:return-type ffi:long))))

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

(def-call-out widget-testpointer-wnd
  (:name "TestGetWNDCLASS")
  (:arguments (lisp-wndclass-ptr (c-ptr test-wndclassex)))  
  (:return-type)
  (:library "WidgetToolkit.dll"))

(def-call-out widget-sizeof-wnd
  (:name "WidgetSizeofWND")
  (:arguments)
  (:return-type ffi:c-pointer)
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
;;; 100 Type of Pointer: FOREIGN-ADDRESS
;;; Pointer Value: #<FOREIGN-ADDRESS #x00229BCC>
;;; Cast: 100
;;;
(defun tst ()
  (with-c-var (int-var 'int 100)
              (print (type-of int-var))
              (print int-var)
              (with-c-var (pointer 'c-pointer (c-var-address int-var))
                          (print (type-of pointer))
                          (print pointer)
                          (print (cast pointer '(c-ptr int)))))
  (with-c-var (int-place '(c-ptr int) 200)
              (print (type-of int-place))
              (print int-place)
              (with-c-var (pointer 
                           'c-pointer (c-var-address (deref int-place)))
                          (print (type-of pointer))
                          (print pointer)
                          (print (cast pointer '(c-ptr int))))))
 
;;;
;;; Run the set of tests
;;;
(defun run-test-suite ()
  (print "/// test suite begin .....")
  (let ((wnd
	 (make-test-wndclassex
	  :cbSize (widget-sizeof-wnd)
	  :style 0
	  :lpfnWndProc #'test-wndproc
	  :cbClsExtra 0
	  :cbWndExtra 0
	  :hInstance nil
	  :hIcon (widget-defaulticon)
	  :hCursor (widget-defaultcursor)
	  :hbrBackground (widget-defaultbrush)
	  :lpszMenuName "NONE"
	  :lpszClassName "gClassName"
	  :hIconSm (widget-defaulticon)
	  )))
    ;;(with-c-var (varc-wnd 'test-wndclassex wnd)
    ;;	(widget-testpointer-wnd (c-var-address varc-wnd)))
    (let ((simpl (make-test-simpl :cbSize 32
                                  :lpfnWndProc #'test-wndproc)))
      (with-c-var (bb 'test-simpl simpl)
                  (widget-testpointer-ptr (c-var-address bb))))
    (tst)
    )
  ;;; Exit
  (print "..... test suite end ///"))

;;;
;;; End
;;;
(provide "widget-toolkit-test")

;;;
;;; End of File
;;;
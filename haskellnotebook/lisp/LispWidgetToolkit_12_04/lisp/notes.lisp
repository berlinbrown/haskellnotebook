;;;
;;; Berlin Brown
;;;
;;; Date: 12/13/2004
;;;
;;; $Id$
;;;
;;; Here is an example mapping between a winuser.h win32 datastructure
;;; to lisp-win32-clisp
;;;

;;;
;;; typedef struct _WNDCLASSEXA {
;;; 	UINT cbSize;
;;; 	UINT style;
;;; 	WNDPROC lpfnWndProc;
;;; 	int cbClsExtra;
;;; 	int cbWndExtra;
;;; 	HINSTANCE hInstance;
;;; 	HICON hIcon;
;;; 	HCURSOR hCursor;
;;; 	HBRUSH hbrBackground;
;;; 	LPCSTR lpszMenuName;
;;; 	LPCSTR lpszClassName;
;;; 	HICON hIconSm;
;;; } WNDCLASSEXA,*LPWNDCLASSEXA,*PWNDCLASSEXA;
;;;
;;;

#|

  
(def-c-struct lisp-wndclassex
  (cbSize uint32)
  (style uint32)
  (lpfnWndProc c-pointer)
  (cbClsExtra int)
  (cbWndExtra int)
  (hInstance c-pointer)
  (hIcon c-pointer)
  (hCursor c-pointer)
  (hbrBackground c-pointer)
  (lpszMenuName c-string)
  (lpszClassName c-string)
  (hIconSm c-pointer))
	
	(defun test-s2 ()
		  (let ((px (make-lisp-wcndclassex)))
		    (print px)
		    (print (slot-value px 'a))
	    ))
  
|#


#|
 -----------------------------------------
 
 Taken from the CLISP tests 
 
 -----------------------------------------
  
(progn
  (defparameter *x* 0)
  (defun callback (x)
    (the (unsigned-byte 16) x)
    (setf *x* x)
    (the (unsigned-byte 16) (1+ (* 2 x))))
  *x*)
0

(def-c-type idfun
 (c-function (:arguments (x uint)) (:return-type uint)
   (:language :stdc)))
IDFUN

;; convert forth and back
(type-of (setq callbackf (with-c-var (x 'idfun #'callback) x)))
FOREIGN-FUNCTION

(list (funcall callbackf 32767) *x*)
(65535 32767)

|#
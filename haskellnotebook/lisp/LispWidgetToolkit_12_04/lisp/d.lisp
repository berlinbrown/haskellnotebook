;;;
;;; Author: Berlin Brown
;;; Contact: retroevolution.com
;;;
;;; Date: 12/10/2004
;;;
;;; see... http://clisp.cons.org/impnotes/dffi.html
;;; 
;;; Test DLL loading
;;;
;;; Note: Dev-CPP used as development environment
;;;
;;; to load type [ load "d.lisp" ] or run from the clisp command line
;;;

(load "widget-toolkit.lisp")
(use-package 'widget-toolkit)
;;;
;;; Entry - Point
;;;
(defun lisp-main () 
  (run-full-testsuite))

(lisp-main)
      
;;;
;;; End of file
;;;

;;;
;;; packages.lisp
;;;

(in-package :cl-user)

(defpackage :ghost-trinity
  (:use :cl 
		:html-template 
		:hunchentoot
		:clsql 
		:clsql-mysql
		:sb-bsd-sockets)
  (:export :start-app
		   :riki-main
		   :stop-app))

;;; End of File
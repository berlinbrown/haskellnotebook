;;; ********************************************************
;;; FILE IDENTIFICATION
;;;
;;; Name:          ghost-trinity.asd
;;; Purpose:       ASDF definition file for botlist trinity project
;;; Author:        Berlin Brown
;;; Date Started:  Feb 2008
;;;
;;; ********************************************************

(defpackage #:ghost-trinity-system (:use #:asdf #:cl))

(in-package :ghost-trinity-system)

(asdf:defsystem :ghost-trinity
                :name "ghost-trinity"
                :author "Berlin Brown"
                :version "0.1"
                :maintainer "Berlin Brown <berlin.brown@gmail.com>"
                :licence "BSD"
                :description "Ghost Trinity Web Front End"
                :long-description "Ghost Trinity Web Front End"
                :depends-on (:hunchentoot
                             :cl-who
                             :html-template
                             :clsql
                             :clsql-mysql
							 :sb-bsd-sockets)
                :components ((:file "packages")
							 (:file "riki_client" :depends-on ("packages"))
                             (:file "trinity" :depends-on ("packages" "riki_client"))
							 ))
;;; End of File
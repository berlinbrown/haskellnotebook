;;;
;;; package.lisp
;;;

(defpackage :simple-codegen
  (:use :cl)
  (:export
   :tokenize-string
   :write-content
   :parse-access-file
   :test-parser
   :parse-line
   :inc-lines
   :etime
   :stime
   :print-stats
   :*database-ip*
   :processIPContent
   ))

(in-package :simple-codegen)
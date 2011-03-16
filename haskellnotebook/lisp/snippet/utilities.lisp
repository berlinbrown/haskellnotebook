;;
;; Berlin Brown
;; utilities.lisp
(defclass snippet ()
 ;----------------------------
 ; Example to access slots:
 ;  (setf (http-status headers) 200)"
 ;  (http-status headers)
 ;----------------------------
  ((title :accessor code-title :initarg title :initform nil)
   (descr :accessor code-descr :initarg descr :initform nil)
   (keywords :accessor code-keywords :initarg keywords :initform nil)))

(defparameter *lisp-code*)
(defparameter *jlisp-code*)
(defparameter *scheme-code*)
(defparameter *haskell-code*)
(defparameter *erlang-code*)
(defparameter *java-code*)
(defparameter *scala-code*)
(defparameter *clojure-code*)
(defparameter *factor-code*)
(defparameter *ruby-code*)
(defparameter *jruby-code*)
(defparameter *python-code*)
(defparameter *jython-code*)

(defparameter *xml-code*)
(defparameter *jsp-code*)
(defparameter *sql-code*)
(defparameter *bash-code*)
(defparameter *config-code*)

(defparameter *django-code*)

;;

(defparameter *src-url-prefix* "/src/codegen/")

(defclass codegen ()
 ;----------------------------
 ; To access slots:
 ;  (make-codegen :code-title "test" :code-keywords "test2")
 ;  (setf (http-status headers) 200)"
 ;  (http-status headers)
 ;----------------------------
  ((title :accessor code-title :initarg title :initform nil)
   (keywords :accessor code-keywords :initarg keywords :initform nil)
   (url :accessor code-url :initarg url :initform nil)))

(defun snippet (titl tags u)
  (make-instance 'codegen 
                 'title titl
                 'keywords tags
                 'url (format nil "~a~a" *src-url-prefix* u)))
(defmacro snip (code-sect &rest args)
  `(pushnew (snippet ,@args) ,code-sect))

(defparameter *all-code* '())

(defparameter *all-lisp* '())
(defparameter *all-fortran* '())
(defparameter *all-functional* '())
(defparameter *all-jvm* '())
(defparameter *all-misc* '())

;; Lisp languages (lisp == common lisp) (1, 2)
(defparameter *lisp-code* '())
(defparameter *scheme-code* '())

;; Procedural langauges, Fortran family (3, 4, 5, 6)
(defparameter *java-code* '())
(defparameter *ruby-code* '())
(defparameter *python-code* '())
(defparameter *c-code* '())

;; Functional languages (7, 8, 9)
(defparameter *erlang-code* '())
(defparameter *haskell-code* '())
(defparameter *factor-code* '())

;; JVM Languages (10, 11, 12, 13, 14, 15)
(defparameter *scala-code* '())
(defparameter *clojure-code* '())
(defparameter *jruby-code* '())
(defparameter *jython-code* '())
(defparameter *abcl-code* '())
(defparameter *swing-code* '())

;; Misc configurations and code (16, 17, 18, 19)
(defparameter *sql-code* '())
(defparameter *config-code* '())
(defparameter *xhtml-code* '())
(defparameter *xml-code* '())

;; Push all the code sections onto the main code stack
(defun set-properties ()
  (when *lisp-code* (pushnew *lisp-code* *all-lisp*))
  (when *scheme-code* (pushnew *scheme-code* *all-lisp*))
  
  (when *java-code* (pushnew *java-code* *all-fortran*))
  (when *ruby-code* (pushnew *ruby-code* *all-fortran*))
  (when *python-code* (pushnew *python-code* *all-fortran*))
  (when *c-code* (pushnew *c-code* *all-fortran*))
   
  (when *erlang-code* (pushnew *erlang-code* *all-functional*))
  (when *haskell-code* (pushnew *haskell-code* *all-functional*))
  (when *factor-code* (pushnew *factor-code* *all-functional*))
  
  (when *scala-code* (pushnew *scala-code* *all-jvm*))
  (when *clojure-code* (pushnew *clojure-code* *all-jvm*))
  (when *jruby-code* (pushnew *jruby-code* *all-jvm*))
  (when *jython-code* (pushnew *jython-code* *all-jvm*))
  (when *abcl-code* (pushnew *abcl-code* *all-jvm*))
  (when *swing-code* (pushnew *swing-code* *all-jvm*))
  
  (when *sql-code* (pushnew *sql-code* *all-misc*))
  (when *config-code* (pushnew *config-code* *all-misc*))
  (when *xhtml-code* (pushnew *xhtml-code* *all-misc*))
  (when *xml-code* (pushnew *xml-code* *all-misc*))
  
  ;; Push all the code sections onto the main code stack
  (when *all-lisp* (pushnew *all-lisp* *all-code*))
  (when *all-fortran* (pushnew *all-fortran* *all-code*))
  (when *all-functional* (pushnew *all-functional* *all-code*))
  (when *all-jvm* (pushnew *all-jvm* *all-code*))
  (when *all-misc* (pushnew *all-misc* *all-code*))
  (reverse *all-code*))
;; End of File
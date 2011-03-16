;; codegen.lisp
;;
;; References:
;; [1] http://www.weitz.de/html-template/

;;(in-package #:cl-user)
;;(defpackage #:codegen (:use #:cl #:html-template))
;;(in-package #:codegen)

(require :html-template)

(load "codegen.properties")

(defparameter *codegen-template* #p"index_templ.html")

(defun code-rows (stack-vals)
  (loop for lang-set in stack-vals
        collect
        (list :lang
              (loop for lang in lang-set
                    collect
                    (list :title (code-title lang)
                          :keywords (code-keywords lang)
                          :url (code-url lang))))))

(defun codegen-main ()
  ;; ALL-CODE --> SECTIONS(all-lisp) --> URLS(lisp-code)
  (let* ((lisp-rows (code-rows *all-lisp*))
         (fortran-rows (code-rows *all-fortran*))
         (functional-rows (code-rows *all-functional*))
         (jvm-rows (code-rows *all-jvm*))
         (misc-rows (code-rows *all-misc*))
		 (values (list :lisp-rows lisp-rows
                       :fortran-rows fortran-rows
                       :functional-rows functional-rows
                       :jvm-rows jvm-rows
                       :misc-rows misc-rows)))
    (with-open-file (stream "code_snippets.html"
                            :direction :output
                            :if-exists :supersede)
                    (html-template:fill-and-print-template 
					 *codegen-template* 
					 values :stream stream))))
(defun main ()
  (format t "INFO: Running~%")
  (codegen-main)
  (format t "INFO: Done~%"))
(main)

;; End of Script

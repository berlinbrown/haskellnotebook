;;
;; Load all modules through the ASDF system
;;

(load "asdf")
(setf asdf:*central-registry* '(*default-pathname-defaults*))

;;;
;;; Add the examples,test, and wtk-src directories
;;;
(progn
  ;;;
  ;;; Set the root path
  ;;;
  (defparameter *widget-toolkit-root*
    (make-pathname :directory '(:relative)))
  (push (merge-pathnames
         (make-pathname :directory '(:relative "lib"))
	 *widget-toolkit-root*)
        asdf:*central-registry*))

;;;
;;; Load the *.asd files
;;;
;;; This includes: (trivial-sockets)
;;;
(asdf:operate 'asdf:load-op 'simple-codegen)

(load "codegen-main")

;;; End of File ;;
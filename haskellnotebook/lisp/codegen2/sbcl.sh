#|
exec sbcl --noinform --load $0 --end-toplevel-options "$@"
|#
(handler-case
	(progn
		(load "codegen.lisp")
		(quit))
	(error (e) 
		(progn 
			(format t "~%Exiting [~a]~%" e)
			(quit))))

;; End of Script 
						

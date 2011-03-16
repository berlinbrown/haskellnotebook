#|
exec sbcl --noinform --load $0 --end-toplevel-options "$@"
|#
(handler-case
	(progn
		(load "test_riki_sbcl.lisp")
		(quit))
	(error (e)
		(progn 
			(format t "~%Exiting [~a]~%" e)
			(quit))))

						

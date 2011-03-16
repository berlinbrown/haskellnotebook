;;
;; Euler problem, number 1
;; Environment: mzscheme
;; 
;; If we list all the natural numbers below 10 that 
;; are multiples of 3 or 5, we get 3, 5, 6 and 9. 
;; The sum of these multiples is 23.
;;
;; Find the sum of all the multiples of 3 or 5 below 1000.
;;
;; References:
;; [1] http://schemecookbook.org/Cookbook/TOC
;;

(define (euler1 n)
  (let ((x 0))
	(do ([i 0 (+ i 1)]) [(= i n)]
	  (if (or (= 0 (modulo i 5))
			  (= 0 (modulo i 3)))
		  (begin
			(set! x (+ x i)))))
	x))

(define (main)
  (display "Running\n")
  (display (euler1 999))
  (display "\nDone\n")
  (exit))

(main)

;;
;; euler9.scm
;;
;;
;; A Pythagorean triplet is a set of three natural numbers, 
;; a < b < c, for which,
;;
;; a2 + b2 = c2
;;
;; For example, 32 + 42 = 9 + 16 = 25 = 52.
;;
;; There exists exactly one Pythagorean triplet for which a + b + c = 1000.
;; Find the product abc.


(define (sqr n)
  (* n n))

(define (get-c a b)
  (let* ((a2 (sqr a))
		 (b2 (sqr b))
		 (c2 (+ a2 b2))
		 (c (sqrt c2)))
	c))
  
(define (triplet? a b)
  (let* ((a2 (sqr a))
		 (b2 (sqr b))
		 (c2 (+ a2 b2))
		 (c (sqrt c2))
		 (c-int? (integer? c))
		 (lt? (< a b c)))
	(if (and lt? c-int?)
		#t #f)))

(define (euler9? a b n)
  (let* ((t? (triplet? a b))
		 (c (get-c a b)))
	(if (and t? (= (+ a b c) n))
		#t #f)))
	
(define (euler9 n z)
  (let ((x 0))
	(do ([i 0 (+ i 1)]) [(= i n)]
	  (do ([j 0 (+ j 1)]) [(= j n)]
		(if (euler9? i j z)
			(begin 
			  (display "Euler9 (a, b, c) ==>")
			  (display i) (display " ") (display j) (display " ")
			  (display (get-c i j))
			  (newline)
			  (display (* i j (get-c i j)))
			  (newline)
			))))
	x))

(define (main)
  (display "Running\n")
  (euler9 500 1000)
  (display "Done\n")
  (exit))
(main)

;;
;; String tokenizer
;;

(in-package :simple-codegen)

(defun str-exclude (x)
  " Chars to ignore "
  (or (eq #\Space x) (eq #\Tab x) (eq #\Newline x)))

;;
;; From Paul Graham's (ansi common lisp)
(defun tokens (str test start)
  " pass the arg 'test', the function used to check for invalid chars "
  (let ((p1 (position-if test str :start start)))
    (if p1 (let ((p2 
		  (position-if #'(lambda (c) 
				(not (funcall test c)))
			    str :start p1)))
	  (cons (subseq str p1 p2)
		(if p2 (tokens str test p2) 
		    nil)))
	nil)))

(defun tokenize-string (*in-string*)
  (tokens *in-string*
	  (lambda (x) (not (str-exclude x))) 0))
    
(defun test-tokenize ()
  " Test the tokenizer functionality "
  (print (tokens "GET /ejb/session_HelloWorldSessionBean.html HTTP/1.0"
		 (lambda (x) (not (str-exclude x))) 0)
	 ))

;;==========================================================

;;
;; property-parser - parse the properties file and look for 
;;  access.log.file
(defun property-parse-logfile ()
    (with-open-file (props "logparser.properties"
			     :direction :input 
			     :if-does-not-exist nil)
      (let ((log-hash (make-hash-table)))
	(setf (gethash 'log-home log-hash) nil)
	(loop for line = (read-line props nil)
	      while line	      
	      do (when (when (> (length line) 2)
			 (not (equal (char line 0) #\#)))
		   ;; Really lazy load the property file
		   ;; TODO: add complete property file loading
		   (setf (gethash 'log-home log-hash)
			 (subseq line 16))))
	(gethash 'log-home log-hash))))	

(defun test-property-load ()
  (print (property-parse-logfile)))
		    
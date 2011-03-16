;;
;; Berlin Brown
;; 9/30/2005
;;
;; File-stats.lisp
;;
;; Use of CLOS for keeping track of file parsing
;; You may want to use the accessor over the (slot-value) tags
;;
;; Various Usage Notes and Examples:
:;
;;
;; Several stages of parsing:
;;------------------------------------------------
;; Example Line of the Access_Log
;; 1. [IP] (|65.214.44.78| 
;; 2. [] - 
;  3. [] - 
;; 4. [DATE and TIME] "{05/Oct/2005:18:48:31 +0000}"
;; 5. [Request] "GET /ejb/session_HelloWorldSessionBean.html HTTP/1.0" 
;; 6. [Status Code] 304 
;; 7. - 
;; 8. "-"
;; 9. [USER_AGENT] "Mozilla/2.0 (compatible; Ask Jeeves/Teoma; +http://sp.ask.com/docs/about/tech_crawling.html)")
;;------------------------------------------------

(in-package :simple-codegen)

;;; ==============================================

;; parse-replace-all:
;; Note: that the args are in the same position as in substitute
(defun parse-replace-all (replacement part string &key (test #'char=))
  "Returns a new string in which all the occurences of the part 
is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
	  for old-pos = 0 then (+ pos part-length)
	  for pos = (search part string
			    :start2 old-pos
			    :test test)
	  do (write-string string out
			   :start old-pos
			   :end (or pos (length string)))
	  when pos do (write-string replacement out)
	  while pos)))

;;
;; (setf my-stats (make-instance 'file-stats))
;; @see file-walk-parse
(defclass file-stats ()
  ((file-count :accessor number-files
	       :initform 0
	       :initarg :file-count)
   (time-proc :accessor process-time
	      :initform 0
	      :initarg :time-proc)
   (lines-proc :accessor lines-proc
	       :initform 0
	       :initarg :lines-proc)
   (start-time :accessor start-time-p
	       :initform 0)
   (end-time :accessor end-time-p
	     :initform 0)
   ))

(defmethod stime (file-stats)
  (setf (start-time-p file-stats) 
	(get-internal-real-time)))

(defmethod etime (file-stats)
  (setf (end-time-p file-stats) 
	(get-internal-real-time)))

(defmethod print-stats (file-stats)
  (format t "~%Numbers of Files: ~a~%" (slot-value file-stats 'file-count))
  (format t "Numbers of Lines: ~a~%" (slot-value file-stats 'lines-proc))
  (format t "Diff-Time: ~f~%" (/ (- (end-time-p file-stats)
				 (start-time-p file-stats)) 
				 internal-time-units-per-second)))

(defmethod inc-file (file-stats)
  (incf (slot-value file-stats 'file-count)))

(defmethod inc-lines (file-stats)
  (incf (slot-value file-stats 'lines-proc)))

;;; ==============================================
(defun clos-test ()
  " Test file stat functionality "
  (let ((my-stats nil))
    (setf my-stats (make-instance 'file-stats 
				  :file-count 0
				  :time-proc 0))
    (print (number-files my-stats))
    (inc-file my-stats)
    (inc-file my-stats)
    (print-stats my-stats)))

;;; ==============================================

;; java-line-parse can also be used as a generic line parser,
;; for example parsing 'access-log' documents.
;;
;; (setf my-parser (make-instance 'java-line-parser :id-one 3))
(defclass java-line-parser ()
  ((cur-line :accessor cur-line
	     :initform NIL
	     :initarg :cur-line)
   (n :accessor n-size
      :initform 0)
   (lines-proc :accessor lines-proc-no
	       :initform 0)
   ))

(defmethod set-lines-log (lineno java-line-parser)
  " Set the count on the number of lines in the file "
  (setf (slot-value java-line-parser 'lines-proc) lineno))

;;
;; Line substitute - parse for any invalid chars
;;
(defun check-line-invalid (line)
  " Reads a line from the access_log and does substitutions 
of invalid chars with those that will work with the parser"
  (let ((newline line))
    ;; Replace any strings - first pass
    (dolist (val-swap '(( "\"[" "["   )
			( "]\"" "]"   )))
      (setf newline (parse-replace-all (first val-swap) (second val-swap) newline))
      newline)
    (dolist (val-swap '(( #\{ #\[ )			
			( #\} #\] )))
      (setf newline (substitute (first val-swap) (second val-swap) newline))
      newline)
    newline))

;;
;; Parse a line of the log
;;
;; @param *db-streams*  Class containg database content
;; @param line  Input line to parse as a string
;; @param java-line-parser  Class, used for this method
(defmethod parse-line (*db-streams* line in-line-no java-line-parser)
  " Parse a line of the file "
  (setf (cur-line java-line-parser) line)
  (setf (n-size java-line-parser) 
	(length line))
  (set-lines-log in-line-no java-line-parser)
  (let ((in-line
	 (format nil "(~a)"
		 (cur-line java-line-parser)))
	(line-no (lines-proc-no java-line-parser)))
    (unwind-protect
	 (let ((n-parse
		(check-line-invalid in-line)))
	   (parse-access-list line-no *db-streams*
	    (read-from-string n-parse))
	   ))
    ))

(defun parse-access-list (line-no *db-streams* inlist)
  " Parse this particular line from the access_log as a list "
  (processIPContent inlist *db-streams*)
  (processAccessContent inlist *db-streams*)
  (when (= 0 (mod line-no 100)) (format t "~%"))
  (when (= 0 (mod line-no 20))
    (format t "..[~A]" line-no)))
  	
;; End of file
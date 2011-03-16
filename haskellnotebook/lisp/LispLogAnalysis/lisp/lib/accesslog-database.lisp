;;;
;;; Berlin Brown
;;;
;;; build the seperate tables of access_log content
;;; 10/8/2005
;;;
;;; The word 'access' in this context represents a
;;; web access or download, for example a GIF file or HTML document
;;;
;;; @see file-stats
#|
  ------------------------------------------------
   A. IP_PAGE_REQUEST: (ip and what page was requested)
    10/6/2005
  ------------------------------------------------
  FIELDS:
   1. ID 
   2. IP unique
		one-to-many -> 3. DATE_TIME
		one-to-many -> 4. PAGE requested
   
  ------------------------------------------------
   B. PAGE and DATE (for each page, give the date of
    the request) 10/6/2005
  ------------------------------------------------
  FIELDS:
   1. ID
   2. PAGE (request)
   		one-to-many -> 3. DATE
   		one-to-many -> 4. STATUS_CODE
   		one-to-many -> 5. Length of the QUERY string
  ------------------------------------------------
   Total Number of browsers
  ------------------------------------------------
  FIELDS:
   1. ID
   2. BROWSER
   3. FULL BROWSER NAME
   4. DATE
|#

(in-package :simple-codegen)

(defparameter *database-ip* 
  "../logs/tables/iptable.txt")

(defparameter *database-access* 
  "../logs/tables/webaccesstable.txt")

;; A quick access entry into the database streams
(defclass *database-streams* ()
    ((strm-ip :accessor db-stream-ip
	      :initform nil)
     (strm-access :accessor db-stream-access
		  :initform nil)
     (ip-table :accessor db-table-ip
	       :initform (make-hash-table))
     (access-table :accessor db-table-access
		   :initform (make-hash-table 
			      :test #'equalp))
     ))

;;
;; @param line-data  List of log content
;; @param *database-streams*  Class defined for this method
;; @see file-stats  For more on the format of the list
(defmethod processIPContent (line-data *database-streams*)
  ;; Parse the input data, a simple list format
  (let ((ip-str (string (first line-data)))
	(*ip-hash* (db-table-ip *database-streams*)))
    ;; First check if the hash exists and/or add to hash key
    (if (gethash ip-str *ip-hash*)
	(incf (gethash ip-str *ip-hash*))
	(setf (gethash ip-str *ip-hash*) 1))
    ))

;;
;; Parse the GET/POST request field in the log
;;   5. [Request] "GET /ejb/session_HelloWorldSessionBean.html HTTP/1.0" 
;; Use the 'nth' function get the 5th element (zero-index)
(defmethod processAccessContent (line-data *database-streams*)
  ;; Parse the input data, a simple list format
  ;; Use the tokenizer to extract 'GET/POST' download information
  (let ((my-str 
	 (second (tokenize-string (nth 4 line-data))))
	(*a-hash* 
	 (db-table-access *database-streams*)))
    ;; First check if the hash exists and/or add to hash key
    (if (gethash my-str *a-hash*)
	(incf (gethash my-str *a-hash*))
	(setf (gethash my-str *a-hash*) 1))
    ))

;;
;; Generate the database tables, this method
;; is invoked after all the content has been parsed
(defmethod generate-tables (*database-streams*)
  (format t "# generating tables~%")
  (generate-ip-tables *database-streams*)
  (generate-access-tables *database-streams*)
  (format t "# generating done~%")
  )

(defmethod generate-access-tables (*database-streams*)
  "Generate the table associated with accesses and downloads of content"
  (format t "+ Generating Access Download Tables~%")
  (let ((*my-hash* (db-table-access *database-streams*))
	(id-ctr 0))
    (with-hash-table-iterator (my-iterator *my-hash*)
      (loop
       (multiple-value-bind (entry-p key value)
	   (my-iterator)
	 (incf id-ctr)
	 (if entry-p
	   (format (db-stream-access *database-streams*) "~A ~A ~A~%" 
		   id-ctr key value)
	   (return)))
       ))))

(defmethod generate-ip-tables (*database-streams*)
  "Generate the table associated with IP content"
  (format t "+ Generating IP Tables~%")
  (let ((*ip-hash* (db-table-ip *database-streams*))
	(id-ctr 0))
    (with-hash-table-iterator (my-iterator *ip-hash*)
      (loop
       (multiple-value-bind (entry-p key value)
	   (my-iterator)
	 (incf id-ctr)
	 (if entry-p
	   (format (db-stream-ip *database-streams*) "~A ~A ~A~%" 
		   id-ctr key value)
	   (return)))
       ))))
		
(defun print-hash-entry (id key value)
  " Misc utility print function "
  (format t "(~S) The value associated with the key ~S is ~S~%" id key value))
			      
;;; End of File
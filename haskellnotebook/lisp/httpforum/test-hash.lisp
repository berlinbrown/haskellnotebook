;;
;; Simple Test Case for Hash Table Routines

(defparameter *postdata-hash* nil)
(defparameter *postdata* 
  '(("username" "myuser")
    ("password" "mypwd")))

(defun join (lst)
  "Using reduce to join, note O(n^2)
 Or:
  (with-output-to-string (stream) (dolist (string strings) 
      (write-string string stream)))"
  (reduce #'(lambda (x y) (concatenate 'string x y)) lst))

(defun list->hash (lst hashdata)
  "Convert key value list data into a hash table"
  (dolist (key-val lst)
    (setf (gethash (first key-val) hashdata)
          (second key-val)))
  hashdata)

(defun init-postdata ()
  (load "postdata.properties")
  (setf *postdata-hash* (make-hash-table :test #'equal))
  (list->hash *postdata* *postdata-hash*))

(defun print-hash-entry (key value)
  (format t "key=~S | value=~S~%" key value))
(defun print-postdata->str (hash)
  "Pretty print a hashmap of URL form POST data into 
 a list string data structure"
  (with-hash-table-iterator (it hash)
   (loop
    (multiple-value-bind (entry-p key value)
        (it)
      (if entry-p (print-hash-entry key value) (return))))))

(defun postdata->list (hash)
  "Convert a hashmap of URL form POST data into a list string  data structure"
  (let ((lst nil))
    (with-hash-table-iterator (it hash)
     (loop
      do (multiple-value-bind (entry-p key value) (it)
           (if entry-p (setf 
                        lst (cons (format nil "&~a=~a" key value) lst))
           (return lst)))))))

(defun postdata->str (hash)
  "Wrapper function, Convert a hashmap of URL form POST data into a string
for posting to the server"
  (subseq (join (postdata->list *postdata-hash*)) 1))

(defun main ()
  "Define the main entry point for this application"
  (format t "Running~%")
  (init-postdata)
  (dolist (key-val *postdata*)
    (print key-val))
  (when *postdata-hash*
    (print (list->hash *postdata* *postdata-hash*)))
  (print-postdata->str *postdata-hash*)
  (print (postdata->str *postdata-hash*))
  (format t "~%Done~%"))
(main)
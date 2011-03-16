##
## 

----------------------------------------------------------- 
 Heavy Use of Lisp Hash-Tables:
----------------------------------------------------------- 
 ;;; same hash-table as above
 * (with-hash-table-iterator (my-iterator *my-hash*)
    (loop
      (multiple-value-bind (entry-p key value)
          (my-iterator)
        (if entry-p
            (print-hash-entry key value)
            (return)))))

----------------------------------------------------------- 
  Also Loop and Collect:
-----------------------------------------------------------   
   loop for x across "abcd" collect x) ==> (#\a #\b #\c #\d)
 
----------------------------------------------------------- 
 Object Oriented Programming with CLOS:
----------------------------------------------------------- 
 (defclass file-stats ()
  ((file-count :accessor number-files
	       :initform 0
	       ...
	       ...
	       
  (defmethod inc-lines (file-stats)
  	(incf (slot-value file-stats 'lines-proc)))	       	   
  	

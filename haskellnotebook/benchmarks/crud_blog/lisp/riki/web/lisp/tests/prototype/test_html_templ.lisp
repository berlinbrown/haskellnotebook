;;
;; Berlin Brown
;; Simple prototype example; using commmon lisp and html-template,
;; Read an input html document template file and use the
;; template parser to generate a new html document with the
;; populated variables.
;;
;; Filename: test_html_templ.lisp
;; Environment: (SBCL 1.0.14) common lisp with libraries.
;;
;; References:
;; [1] http://www.weitz.de/html-template/

(require :html-template)

(defun parse-template ()
  (let* ((rows 
		  (loop for i below 49 by 7
			 collect (list :cols
						   (loop for j from i below (+ i 7)
							  for string = (format nil "~R" j)
							  collect (list :content string
											:colorful-style 
											(oddp j))))))
		 (values (list :rows rows)))
	(with-open-file (stream "output.html" 
							:direction :output 
							:if-exists :supersede)
	  (html-template:fill-and-print-template 
	   #p"./index_templ.html" values
	   :stream stream))))

(defun main ()
  (format t "Running~%")
  (parse-template)
  (format t "Done~%"))
(main)

;; End of the file.
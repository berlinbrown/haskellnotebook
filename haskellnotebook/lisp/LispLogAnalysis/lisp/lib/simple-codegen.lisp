;;;
;;; Berlin Brown
;;; Simple-codegen
;;;

(in-package :simple-codegen)

(defparameter *html-header* (string "<meta http-equiv='Content-Type' content='text/html; charset=ISO-8859-1'>
<html>
 <head>
  <title>SpiritCompany | Projects</title>
  <META NAME='Author' CONTENT='Berlin Brown'>
  <META NAME='DESCRIPTION' CONTENT='Projects Page'>
  <META NAME='keywords' CONTENT='projects, spero, lisp'>	
  <style type='text/css'>   
		BODY
		{      
		margin: 10px 10px 10px 10px;
		FONT-FAMILY: Arial, Verdana, Sans;
		BACKGROUND-COLOR: #ffffff
		}
		IMG
		{
		BORDER-RIGHT: 0px;
		BORDER-TOP: 0px;
		BORDER-LEFT: 0px;
		BORDER-BOTTOM: 0px
		}        

		A:link
		{   
			COLOR: #1A3391;
			FONT-SIZE: 12pt;			
		}	
		A:hover
		{
			COLOR: #1A3391;
			CURSOR: wait;     
			FONT-SIZE: 12pt;
			background-color: #E9E9E9;			
		}

		.divbox {
		 margin-top: 80px;
			width: 760px;			
			border-top: 2px solid #e0e0e0;
		}
   </style>
 </head> 
  <body>  
   <div class='divbox'>
"))

(defparameter *html-footer* (string "
   </div>
  </body>
</html>"))


;; Writing a File
(defun open-write-file (file-name)
  (format t "# Writing file ~%")
  (let ((out-file
         (open file-name :direction :output)))
    (when out-file     
      (format out-file "dogs and cats")
      (close out-file)))
  (format t "# end ~%"))

(defun href-content (link title)
  (format nil "~%<p><a href='~a'>~a</a><br>~%" link title))

(defun write-content (file-name *content*)
  (format t "~%# Writing file ~%")
  (let ((out-file
	 (open file-name :direction :output)))
    (when out-file
      (format out-file *html-header*)
      (dolist (node *content*) 
	(progn
	  (format out-file (href-content 
			    (car node) (first (cdr node))))
	  ;; Print the description
	  (format out-file "~a~%" (first (last node)))
	  ))
      (format out-file *html-footer*)
  (close out-file)))
  (format t "# end ~%"))

;;; End of File
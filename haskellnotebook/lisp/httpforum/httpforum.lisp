;; Author: Berlin Brown <berlin dot brown at gmail.com>
;; Date: 6/6/2008
;; File: httpforum.lisp
;;
;; ---------------------------
;; Short Description:
;; ---------------------------
;; Simple HTTP client for communicating with web forums.
;;
;; Environment: Tested with GNU CLISP 2.44 (2008-02-02) Win32
;;
;; Full Description:
;;
;; ---------------------------
;; Approaching for posting to the forum:
;; ---------------------------
;; Posting to a forum is going to happen in three HTTP requests.
;; 1. Request the main page - open this page to get the cookie/session
;; 2. Request the POST page
;; 3. Using the POST HTTP request to transmit the data to the server7.
;; 
;; ---------------------------
;; Example 200 response from server:
;; (first request)
;; --------------------------
;; HTTP/1.1 200 OK
;; Date: Sun, 08 Jun 2008 17:51:09 GMT
;; Server: Apache 3
;; X-Powered-By: PHP/4.4.3
;; Set-Cookie: SESSION=e7c1aefdb2c7137963a5ff94592f5f66;  \
;;    expires=Mon, 08 Jun 2009 17:51:09 GMT
;; Expires: Sat, 07 Jun 2008 17:51:09 +0000GMT
;; Last-Modified: Sun, 08 Jun 2008 17:51:09 +0000
;; Cache-Control: private
;; Connection: close
;; Transfer-Encoding: chunked
;; Content-Type: text/html
;;
;; References:
;; [1] http://cl-cookbook.sourceforge.net/sockets.html
;; [2] http://clocc.sourceforge.net/dist/port.html
;; [3] http://clisp.cons.org/impnotes/socket.html
;; [4] http://www.unixuser.org/~euske/doc/cl/loop.html

(defparameter *DEFAULT_USER_AGENT_IE* 
  "Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.0; .NET CLR 1.1.4322; .NET CLR 2.0.50727; InfoPath.1)")
(defparameter *DEFAULT_USER_AGENT_FF* 
  "Mozilla/5.0 (Windows; U; Windows NT 5.1; en-US; rv:1.8.1.7) Gecko/20070914 Firefox/2.0.0.7")

(defparameter *postdata-hash* nil)
(defparameter *postdata* 
  '(("username" "myuser")
    ("password" "mypwd")))

(defclass http-headers ()
 ;----------------------------
 ; To access slots:
 ;  (setf (http-status headers) 200)"
 ;  (http-status headers)
 ;----------------------------
  ((status :accessor http-status :initarg status :initform nil)
   (status-code :accessor http-status-code :initarg status-code :initform -1)
   (date :accessor http-date :initarg date :initform nil)
   (server :accessor http-server :initarg server)
   (cookie :accessor http-cookie :initarg cookie :initform nil)
   (cookie-data :accessor http-cookie-data :initform nil)
   (session :accessor http-session :initform nil)
   (expires :accessor http-expires :initarg expires :initform nil)
   (modified :accessor http-modified :initarg modified :initform nil)
   (connection :accessor http-connection :initarg connection :initform nil)
   (content :accessor http-content :initarg content)))

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
    (multiple-value-bind (entry-p key value) (it)
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

(defmethod set-cookies ((headers http-headers))
  " Create a hashtable data structure from the collection of cookies
 E.g.
 SESSION=d988fc7b47b25d93e2f3fb6509a08f14; 
 expires=Mon, 08 Jun 2009 23:59:52 GMT"
  (let* ((data-str (http-cookie headers))
		 (data-lst (split-by-one data-str #\;))
		 (tabl (make-hash-table :test #'equal)))
	(dolist (key-val data-lst)
	  (let* ((kv (split-by-one key-val #\=))
			 (key (string-trim " " (first kv)))
			 (val (second kv)))
		(setf (gethash key tabl) val)))
	(setf (http-cookie-data headers) tabl)
	(setf (http-session headers) 
		  (gethash "SESSION" tabl))
	headers))

(defun print-headers (headers)
  "Pretty print the HTTP header information"
  (format t "~%-----------------~%")
  (format t " Status - ~a~%" (http-status headers))
  (format t " Status Code - [~a]~%" (http-status-code headers))
  (format t " Cookies - ~a~%" (http-cookie-data headers))
  (format t " Session - ~a~%" (http-session headers))
  (format t " Date - ~a~%" (http-date headers))
  (format t " Server - [~a]~%" (http-server headers))
  (format t " Expires - ~a~%" (http-expires headers))
  (format t " Modified - ~a~%" (http-modified headers))
  (format t " Connection - ~a~%" (http-connection headers))
  (format t " Content - [~a]~%" (http-content headers))
  (format t "-----------------~%") headers)

(defun join (lst)
  "Using reduce to join, note O(n^2)
 Or:
  (with-output-to-string (stream) (dolist (string strings) 
      (write-string string stream)))"
  (reduce #'(lambda (x y) (concatenate 'string x y)) lst))

(defun split-by-one (string delim)
    "Returns a list of substrings of string
divided by ONE space each.
Note: Two consecutive spaces will be seen as
if there were an empty string between them.
 http://cl-cookbook.sourceforge.net/strings.html"
    (loop for i = 0 then (1+ j)
          as j = (position delim string :start i)
          collect (subseq string i j)
          while j))

(defun get-headers (data)
  "Loop till we get all the header data.  We are using a
bad approach to detect when to quit.  Look for a zero length string"
	(loop
	   for line in (split-by-one data #\Newline)
	   when (= (length line) 0)
	   return lines
	   collect line into lines
	   finally lines))

(defun header-data (line)
  "Convert a header string into a simple data structure.
 (list 'Header:' start-of-text-int)"
  (let* ((lst (split-by-one line #\Space))
		 (header (first lst))
		 (pos (length header)))
	(list header (1+ pos))))

(defun http-parse-headers (data)
  "Parse the HTTP headers and a return a HTTP header data object"
  (let ((headers (make-instance 'http-headers)))
	(dolist (header-str (get-headers data))
	  (let* ((h (first (header-data header-str)))
			 (pos (second (header-data header-str)))
			 (htxt (subseq header-str pos)))
		(cond ((string-equal "HTTP/1.1" h)
			   (progn
				 (setf (http-status headers) htxt)
				 (setf (http-status-code headers)
					   (funcall (lambda (str)
								  (first (split-by-one str #\Space)))
								htxt))))
			  ((string-equal "Date:" h)
			   (setf (http-date headers) htxt))
			  ((string-equal "Server:" h)
			   (setf (http-server headers) htxt))
			  ((string-equal "Set-Cookie:" h)
			   (setf (http-cookie headers) htxt))
			  ((string-equal "Expires:" h) 
			   (setf (http-expires headers) htxt))
			  ((string-equal "Last-Modified:" h) 
			   (setf (http-modified headers) htxt))
			  ((string-equal "Connection:" h)
			   (setf (http-connection headers) htxt))
			  ((string-equal "Content-Type:" h)
			   (setf (http-content headers) htxt)))))
	headers))
			
(defun http-client-request (socket host page 
                                   &key (method "GET") (postmap nil))
  "Send a valid http 1.1 request to the server"
  (let* ((p (string-equal "POST" method))
         (post-str (postdata->str postmap))
         (post-len (if (and post-str p)
                       (length post-str) 0))
         (content-type (when p 
                         "Content-Type: application/x-www-form-urlencoded~%")))
  (format socket "~a ~a HTTP/1.1~%" method page)
  (format socket "Host: ~a~%" host)
  (format socket "User-Agent: ~a~%" *DEFAULT_USER_AGENT_FF*)
  (format socket "Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.81~%")
  (format socket "Accept-Language: en-us,en;q=0.51~%")
  (format socket "Accept-Encoding: gzip,deflate~%")
  (format socket "Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.71~%")
  (format socket "Keep-Alive: 0~%")
  (format socket "Connection: close~%")
  (when p
    (format socket "Content-Length: ~a~%" post-len)
    (format socket content-type))
  (format socket "Keep-Alive: 0~2%")
  (when (and post-str p)
    (format socket post-str))))
  	
(defun http-connect (host page &optional (port 80) 
                          &key (method "GET") (postmap nil))
  "Use of common lisp keyword arguments [(defun i (&key x &key y) (list x y))]"
  ;; HTTP requires the :DOS line terminator
  (with-open-stream (socket 
					 (SOCKET:SOCKET-CONNECT
					  port host :EXTERNAL-FORMAT :DOS))
	(format t "~%###############~%")
	(format t "INFO: Sending Request~%")
	(format t "###############~%~%")
    ;; Print REQUEST data to file and to STDOUT
	(http-client-request t host page 
                         :method method :postmap postmap)
    (with-open-file 
     (ostream "request_data.log"
              :direction :output
			   :EXTERNAL-FORMAT :DOS)
     (http-client-request ostream host page 
                          :method method :postmap postmap))
	(format t "~%###############~%")
	(http-client-request socket host page)
	(loop 
	   :for line = (read-line socket nil nil)
	   :while line
	   :collect (concatenate 'string line (string #\Newline)))))

(defun http-connect-data (host page &optional (port 80) 
                               &key (method "GET") (postmap nil))
  (join (http-connect host page port 
                      :method method :postmap postmap)))

(defun http-main (host page &optional (port 80) 
                       &key (method "GET") (postmap nil))  
  "Entry point for a HTTP get request, return the http header object"
  (let* ((http-data (http-connect-data host page port 
                                       :method method :postmap postmap))
         (headers (http-parse-headers http-data)))
    (print http-data)
	(print-headers (set-cookies headers))))
	
(defun main ()
  "Main entry point for the application"
  (format t "INFO: httpforum - connecting~%")
  (init-postdata)
  ;;(print (http-main "localhost" "/test/index.jsp" 9080))
  ;; Post data from the postdata.properties file to localhost
  ;; post.php.
  (print (http-main "localhost" "/fruit/post.php" 80
                    :method "POST" :postmap *postdata-hash*))
  (format t "~%INFO: done~%"))

(main)
;; End of File
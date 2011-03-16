;; Author: Berlin Brown <berlin dot brown at gmail.com>
;; Date: 6/6/2008
;; File: test_riki_server.lisp
;;
;; ---------------------------
;; Short Description:
;; ---------------------------
;; Simple tcp client for communicating with riki server
;;
;; Environment: Tested with GNU CLISP 2.44 (2008-02-02) Win32
;; GNU CLISP 2.42 (2007-10-16) (built 3403360776) (memory 3419736148)
;; Software: GNU C 4.1.3 20071019 (prerelease) (Ubuntu 4.1.2-17ubuntu1) 
;;
;; Full Description:
;;
;; References:
;; [1] http://cl-cookbook.sourceforge.net/sockets.html
;; [2] http://clocc.sourceforge.net/dist/port.html
;; [3] http://clisp.cons.org/impnotes/socket.html
;; [4] http://www.unixuser.org/~euske/doc/cl/loop.html

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

(defun riki-client-hello (socket)
  " Send a hello request to the server"
  (format socket "@RIKI:CLNT:HELLO"))

(defun riki-client-lisp (socket)
  " Send a hello request to the server"
  (format socket "(defun hello-world () (+ 1 1))"))

(defun read-data (socket)
  " Read lines of data from the server until we encounter the ENDMSG"
  (format t "Waiting for incoming data~%")
  (loop
	 :for line = (read-line socket nil nil)	 
	 :for linet = (string-trim '(#\Space #\e #\t #\m) line)
	 :while line 	 
	 :do (format t "trace: read-line [~a] ~%" linet)
	 :collect line into res
	 :when (string-equal "@RIKI:SERV:ENDMSG" linet)
	 :return res))

(defun riki-connect (host &optional (port 9083))
  "Use of common lisp keyword arguments [(defun i (&key x &key y) (list x y))]"
  ;; HTTP requires the :DOS line terminator
  (with-open-stream (socket 
					 (socket:socket-connect
					  port host :EXTERNAL-FORMAT :DOS))
	(format t "~%###############~%")
	(format t "INFO: Sending Request~%")
	(format t "###############~%~%")
    ;; Print REQUEST data to file and to STDOUT
    (with-open-file 
		(ostream "request_data.log"
				 :direction :output
							:EXTERNAL-FORMAT :DOS)
	  (riki-client-hello ostream))
	(format t "~%###############~%")
	(riki-client-hello socket)
	(read-data socket)
	(riki-client-lisp socket)))

(defun riki-main (host &optional (port 9083))
  "Entry point for a HTTP get request, return the http header object"
  (let* ((http-data (riki-connect host port)))))

(defun main ()
  "Main entry point for the application"
  (format t "INFO: riki server - connecting~%")
  (print (riki-main "localhost" 9083))
  (format t "~%INFO: done~%"))

(main)
;; End of File
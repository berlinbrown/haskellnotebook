;; Author: Berlin Brown <berlin dot brown at gmail.com>
;; Date: 6/6/2008
;; File: test_riki_sbcl.lisp
;;
;; ---------------------------
;; Short Description:
;; ---------------------------
;; Simple tcp client for communicating with riki server
;;
;; Environment: Tested with SBCL 1.0.14 / Ubuntu Linux Heron
;;
;; Full Description:
;;
;; References:
;; [1] http://cl-cookbook.sourceforge.net/sockets.html
;; [2] http://clocc.sourceforge.net/dist/port.html
;; [3] http://clisp.cons.org/impnotes/socket.html
;; [4] http://www.unixuser.org/~euske/doc/cl/loop.html
;;
;; SBCL Socket code from Peter Hildebrandt

(require :sb-bsd-sockets)

(use-package :sb-bsd-sockets)

;;(define-condition host-not-found-error (socket-error))

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
  (socket-send socket "@RIKI:CLNT:HELLO" nil))

(defun riki-client-lisp (socket)
  " Send a hello request to the server"
  (socket-send socket "(defun hello-world () (+ 1 1))" nil))

(defun nslookup (hostname)
   "Performs a DNS look up for HOSTNAME and returns the address as a
   four element array, suitable for socket-connect.  If HOSTNAME is
   not found, a host-not-found-error condition is thrown."
   (if hostname (sb-bsd-sockets:host-ent-address 
				 (sb-bsd-sockets:get-host-by-name hostname)) nil))

(defun tcp-read-raw (socket &key (maxsize 65536) (timeout 5))
   "Reads one line from SOCKET, removes CRLF, and returns it.  The buffer  
size  is 65536 bytes, unless MAXSIZE is specified.  If no result is received
  within TIMEOUT seconds (defaults to 5), nil is returned."
   (when socket
	 (values (socket-receive socket nil maxsize))))

(defun tcp-read (socket &key (timeout 10) (maxsize 1048576))
   (when socket
     (let ((s (tcp-read-raw socket 
							:maxsize maxsize :timeout timeout))) s)))

(defun tcp-connect (server port &optional (timeout 5))
   "Returns a socket connected to SERVER:PORT.  If an error occurs, or the  
attempt times out after TIMOUT (default 5) seconds, nil is returned."
   (when (and server port)
	 (handler-case
		 (let ((socket (make-instance 
						'sb-bsd-sockets:inet-socket :type  
													:stream 
													:protocol :tcp)))
		   (socket-connect socket (nslookup server) port)
		   (format t "INFO: socket => ~a~%" socket)
		   socket)
	   (sb-bsd-sockets:host-not-found-error ()
		 (format t "ERR: host ~A not found." server)
		 (force-output)
		 nil))))

(defun read-data (socket)
  " Read lines of data from the server until we encounter the ENDMSG"
  (format t "INFO: Waiting for incoming data=>~%")
  (let* ((data (tcp-read socket))
		 (lines (split-by-one data #\Newline)))
	(loop
	   :for line in lines
	   :for linet = (string-trim '(#\Space #\e #\t #\m #\r #\Newline #\Return) line)
	   :while line
	   :do (format t " ===> trace: read-line: (~a) ~%" linet)
	   :collect line into res
	   :when (string-equal "@RIKI:SERV:ENDMSG" linet)
	   :return res)))

(defun riki-connect (host &optional (port 9083))
  "Use of common lisp keyword arguments [(defun i (&key x &key y) (list x y))]"
  ;; HTTP requires the :DOS line terminator
  (let ((socket 
		 (tcp-connect host port)))
	(format t "~%###############~%")
	(format t "INFO: Sending Request <~a>~%" socket)
	(format t "###############~%~%")
	(riki-client-hello socket)
	(read-data socket)
	(riki-client-lisp socket)
	(socket-close socket)))

(defun riki-main (host &optional (port 9083))
  "Entry point for a HTTP get request, return the http header object"
  (let* ((http-data (riki-connect host port)))))

(defun main ()
  "Main entry point for the application"
  (format t "INFO: riki server - connecting~%")
  (print (riki-main "127.0.0.1" 9083))
  (format t "~%INFO: done~%"))

(main)
;; End of File
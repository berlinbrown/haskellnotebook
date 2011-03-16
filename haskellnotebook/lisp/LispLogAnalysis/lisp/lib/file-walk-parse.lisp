;;
;; Berlin Brown
;; 9/30/2005
;;
;; Script to Walk a directory and parse a set of documents
;;
;; There are all aspect-oriented programming ways to handle
;; the files before processing, after processing and during the
;; file processing cycle(see below).
;;
;; (As part of the http-test package)
;;

(in-package :simple-codegen)

;; Global variable - initial scan directory "
(defparameter 
  *BOOTSTRAP-DIRECTORY* 
  (string "C:\\Berlin\\projects\\PDP_UI\\Common\\src"))

;; Global variable - file extension to check for"
(defparameter 
  *SCAN-EXTENSION* 
  (string ".java"))
;;------------------------------------------------

;; save file-count stats
(defparameter
  *my-file-stats* nil)
(progn
  (setf *my-file-stats* (make-instance 'file-stats 
				     :file-count 0 
				     :time-proc 0)))

;;------------------------------------------------

(defun walk-parse-test ()
  (print ".done")
  )

(print *package*)

(defun component-present-p (value)
  (and value (not (eql value :unspecific))))

(defun directory-pathname-p (pathspec)
  (and 
    (not (component-present-p (pathname-name pathspec)))
    (not (component-present-p (pathname-type pathspec)))
    pathspec))

(defun pathname-as-directory (pathspec)
  (let ((pathname (pathname pathspec)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (cond ((not (directory-pathname-p pathspec))
           (make-pathname :directory (append (or (pathname-directory pathname)
                                                 (list :relative))
                                             (list (file-namestring pathname)))
                          :name nil
                          :type nil
                          :defaults pathname))
          (t pathname))))

(defun directory-wildcard (dirname)
  (make-pathname :name #-:cormanlisp :wild #+:cormanlisp "*"
                 :type #-(or :clisp :cormanlisp) :wild
                       #+:clisp nil
                       #+:cormanlisp "*"
                       :defaults (pathname-as-directory dirname)))

#+:clisp
(defun clisp-subdirectories-wildcard (wildcard)
  (make-pathname :directory (append (pathname-directory wildcard)
                                    (list :wild))
                 :name nil
                 :type nil
                 :defaults wildcard))

(defun list-directory (dirname)
  (when (wild-pathname-p dirname)
    (error "Can only list concrete directory names."))
  #+:ecl (directory (pathname-as-directory dirname))
  #-:ecl 
  (let ((wildcard (directory-wildcard dirname)))
    #+:abcl (system::list-directory dirname)
    #+(or :sbcl :cmu :lispworks) (directory wildcard)
    #+:openmcl (directory wildcard :directories t)
    #+:allegro (directory wildcard :directories-are-files nil)
    #+:clisp (nconc (directory wildcard)
                    (directory (clisp-subdirectories-wildcard wildcard)))
    #+:cormanlisp (nconc (directory wildcard)
                         (cl::directory-subdirs dirname)))
  #-(or :sbcl :cmu :lispworks :openmcl :allegro :clisp :cormanlisp :ecl :abcl)
  (error "LIST-DIRECTORY not implemented"))

(defun pathname-as-file (pathspec)
  (let ((pathname (pathname pathspec)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (cond ((directory-pathname-p pathspec)
           (let* ((directory (pathname-directory pathname))
                  (name-and-type (pathname (first (last directory)))))
             (make-pathname :directory (butlast directory)
                            :name (pathname-name name-and-type)
                            :type (pathname-type name-and-type)
                            :defaults pathname)))
          (t pathname))))

(defun file-exists-p (pathspec)
  #+(or :sbcl :lispworks :openmcl :ecl) (probe-file pathspec)
  #+:allegro (or (excl:probe-directory (pathname-as-directory pathspec))
                 (probe-file pathspec))
  #+(or :cmu :abcl) (or (probe-file (pathname-as-directory pathspec))
                        (probe-file pathspec))
  #+:cormanlisp (or (and (ccl:directory-p pathspec)
                         (pathname-as-directory pathspec))
                    (probe-file pathspec))
  #+:clisp (or (ignore-errors
                 (let ((directory-form (pathname-as-directory pathspec)))
                   (when (ext:probe-directory directory-form)
                     directory-form)))
               (ignore-errors
                 (probe-file (pathname-as-file pathspec))))
  #-(or :sbcl :cmu :lispworks :openmcl :allegro :clisp :cormanlisp :ecl :abcl)
  (error "FILE-EXISTS-P not implemented"))

(defun directory-exists-p (pathspec)
  #+:allegro
  (and (excl:probe-directory pathspec)
       (pathname-as-directory (truename pathspec)))
  #+:lispworks
  (and (lw:file-directory-p pathspec)
       (pathname-as-directory (truename pathspec)))
  #-(or :allegro :lispworks)
  (let ((result (file-exists-p pathspec)))
    (and result
         (directory-pathname-p result)
         result)))

;;
;; Lighter version of the walking system
;;
(defun walk-light (dirname fn &key 
                           directories (test (constantly t)))
  " Recursively walk a set of sub-directories "
  (labels
      ((walk (name)
             (cond
              ((directory-pathname-p name)
               (when (and directories (funcall test name))
                 (funcall fn name))
               (dolist (x (list-directory name))
                 (walk x)))
              ((funcall test name)
               (funcall fn name)))))
    (walk (pathname-as-directory dirname))))

(defun walk-directory (dirname fn &key directories
                                       (if-does-not-exist :error)
                                       (test (constantly t)))
  " Recursively walk a set of sub-directories "
  (labels ((walk (name)
             (cond
               ((directory-pathname-p name)
                (dolist (file (list-directory name))
                  (walk file))
                (when (and directories
                           (funcall test name))
                  (funcall fn name)))
               ((funcall test name)
                (funcall fn name)))))
    (let ((pathname-as-directory (pathname-as-directory dirname)))
      (case if-does-not-exist
        ((:error)
         (cond ((not (file-exists-p pathname-as-directory))
                (error "File ~S does not exist."
                       pathname-as-directory))
               (t (walk pathname-as-directory))))
        ((:ignore)
         (when (file-exists-p pathname-as-directory)
           (walk pathname-as-directory)))
        (otherwise
         (error "IF-DOES-NOT-EXIST must be one of :ERROR or :IGNORE."))))
    (values)))

;;
;; Standard approach for parsing a file, read
;; line-by-line
(defun read-print-all (infile)
  " Read the entire file and print. "
  (loop for line = (read-line infile nil)
            while line 
	    do 
	    (progn
	      (inc-lines *my-file-stats*)
	      (parse-line nil line 0
	       (make-instance 
		'java-line-parser :cur-line line))
	      )))

(defun read-print-header (infile)
  " Only read the first lines of the file. 
An Example Java Approach:
 Object [] vct = new Object[3];
 line = buf.readline();
 vct[0] = buf.readline();
 vct[1] = buf.readline();
 vct[2] = buf.readline();
 System.out.println(line);
 return vct; "
  (let ((*vct*
         (make-array 3 :initial-element nil)))
    (dotimes (node 3)
      (let ((val (read-line infile nil)))
            (when val        
              (setf (elt *vct* node) val)
            )))
    *vct*))
  
;; 
;; Open the file and process with the file concern
;; 
;; Misc Notes (Macros) - syntactic extension:
;; "Macros extend the rules for evaluating an expression, 
;;  while functions obey the rules" -- Peter Norvig
(defun walk-open-file (file-name fn)
  ;; Increment the global file-counter
  (inc-file *my-file-stats*)
  (with-open-file (in-file file-name
			   :if-does-not-exist nil)
    (let ((all-file nil))
      (format t ".open ~%")
      (when in-file
	(setf all-file (funcall fn in-file))
	(close in-file))
      (format t ".end ~%")
      all-file
      )))

(defun check-header-line (line)
  " Check the line for the proper header and return the raw text. "
  (when line
    (when (> (length line) 3)
      (when (string= 
             (subseq line 0 2) "#!")
        (subseq line 2))
      )))

;;
;; Concern for checking the header information
;;  read-print-header --> process results
;;
;; @see #file-concern
;; @see #walk-open-file
;; @see #read-print-header
(defun parse-file-concern (infile)
  " Process the result vector after they have been read from the file. 
Check for '#!' and then read the rest of the line. "
  (let ((res 
	 (read-print-all infile)))
    (when res
      ;; map to a set of routines to store the header-values
      ;; override the current vector value
      (mapcar #'(lambda (*v* indx str)
                  (if (and str (> (length str) 5))
                      (setf (elt *v* indx) str)
                    (setf (elt *v* indx) nil)))
              ; map the following lists to the lambda args
              (list res res res)
              '(0 1 2)
              (list (check-header-line (elt res 0))
                    (check-header-line (elt res 1))
                    (check-header-line (elt res 2))))
      ) res))


(defun check-file-extension (strm *ext* filename)
  " We are only concerned with filetypes with the EXTENSION
value defined above
NOTE: When filename is > 5, check for EXTENSION, subsequence "
  ;; Use .java for example (not just java)
  (let ((str-name 
	 (file-namestring filename)))   
    (when (> (length str-name) (+ 1 (length *ext*)))
       (string-equal *ext*
	(subseq str-name
		(- (length str-name) 
		   (length *ext*)))))
    ))
	
;;
;; This concern is for dealing with what particular file
;; to open and check to process or not, for example, a
;; file-extension check or filename check.
;;
;; "Suppose we want to do logging at both the beginning and the end of
;; each function body. This will result in crosscutting all 
;; classes that have at least one function." -- AOP
;;
(defun file-concern (fname out-strm &key (verbose nil))
  " Perform work on a single file, open the file, and read first few 
lines (header info)."
  (let* ((*check-ext* (check-file-extension out-strm *SCAN-EXTENSION* fname))
	 (*vect-res*
	  (when *check-ext* (walk-open-file 
			     fname #'(lambda (val)
				       (parse-file-concern val))))))
    (when *vect-res*
      (progn
	;; write the file name, possibly with other modifiers
	(format out-strm "##### ~a~c" 
		(file-namestring fname)
		#\Newline)
	;; 0: Description 1: Misc? 2: Image/Diagram relative URL
	(when (elt *vect-res* 0)
	  (format out-strm "<br><i>description:</i> ~a<p>~c~c"
		  (elt *vect-res* 0) #\Newline #\Newline))))
    (when verbose (format t "File: ~a~%"
                          (file-namestring fname)))
  ))

;;
;; Walk-main - entry point, test-case of our walking dir library
;;
(defun walk-main ()  
  (format t  "~%## File Walker System~%")
  (stime *my-file-stats*)
  ;; write the directory listing to file - to out-strm
  ;; With <code>*standard-output*</code>, dont close the stream
  ;; Example - to output to a file:
  ;; #((out-strm (open "mainallsrc.txt" :direction :output)))
  (let ((out-strm *standard-output*))
    (when out-strm
      (walk-light *BOOTSTRAP-DIRECTORY* 
		  #'(lambda (val)
		      (file-concern val out-strm :verbose nil)))
      ))
  (etime *my-file-stats*)
  (print-stats *my-file-stats*)
  (format t "~%## Done~%"))
  
;;; End of file
##
## Berlin Brown
##
## 10/4/2005
##
## License: This application is under the latest BSD-License

--------------------------------------------------
 1.1 About: SperoLogger
 
 Current version: 0.1
--------------------------------------------------

  SperoLogger is a lisp based application to parse Apache web server log files 
  and extract useful information. From this data, sperologger will
  create data logging reports.

  Note: currently, sperologger operates through off-line processing.
  This means, it is recommended to run this application 
  seperately from your host webserver.  For example, you
  can run this from a desktop application.  You will need
  a way to access your access_log.  This can be accomplished
  through the java utility. 
  
  It works with FTP and SFTP.
 
  *QUICKSTART*
  
  Everything beings in the 'lisp' directory, launch the
  run command.
  
  You can change the default position of the log
  file through the 'logparser.properties' file
 
  Note: the property file loader is not sophisticated, try to keep
  the format as so:
   
    # Place the home of the access_log here - LINE0
	access.log.file=../logs/access_log
  
--------------------------------------------------
 2.1 Dependencies and needed third-party applications
--------------------------------------------------
 Only two(one) applications are need, Java and CLisp(or other lisp runtime)
 
 *Lisp*
 A lisp environment is needed, CLisp/SBCL or other.  This application
 was tested with (GNU CLISP 2.33) (2004-03-17).
 
 *Java*
 A java runtime is needed if you want to use the FTP/SFTP system
 to access your access_log files.  Any runtime from java.sun.com
 should work.
 
 Note: you may also want to instll the Ant build tool, but it
 is not needed.
 
--------------------------------------------------
 3.1 USAGE: Enter the 'lisp' directory
--------------------------------------------------

 1. Launch the 'run.bat' script or if on Linux, use:
   clisp run-all-asdf.lisp
   or
   sbcl run-all-asdf.lisp
 
 The database tables (not SQL tables but tab-delimited) and info
 are generated to the 'logs/tables' sub-directory.
 
--------------------------------------------------
 4.1 Data and Database collected
--------------------------------------------------
 The information collected from the access_log file
 will mimic a database, here are the core tables or
 key pieces of information.
 
 These database files will be generated in the 'logs/tables'
 sub-directory.
 
  ** KEY DATA TABLES **
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


--------------------------------------------------
 5.1 BUGS and other TODO
--------------------------------------------------
 Currently, making changes to the lisp documents 
 may throw off the ASDF system with dependency errors.
 Try doing a complete 'clean' of the system and then
 re-running the 'run' script  

 The download of access_log files may be too complex, 
 a better approach may be needed.

--------------------------------------------------
 6.1 Resources:
-------------------------------------------------- 
 
 http://scratchy.sourceforge.net/

 http://www.lisp.org/table/style.htm
 http://cl-cookbook.sourceforge.net/
 
 http://clisp.cons.org/
 http://sbcl.sourceforge.net/
 
 http://www.franz.com/resources/educational_resources/
--------------------------------------------------

--------------------------------------------------
 7.1 Development:
--------------------------------------------------
 
 The entry point for this application begins with
 accesslog-parse.lisp, see the bottom of the file.
 This file is located in the 'lib' directory.
 
 Also, file-stats.lisp, accesslog-database.lisp are
 key to the application.
 
 The other files are used for utilities and setting
 up the lisp environment.
 
 DEV_README.txt contains some various lisp constructs
 used in this application.

--------------------------------------------------
 Copyright - Berlin Brown 2005
 License: Under a BSD license
 
 berlin.brown@gmail.com
 for questions and comments
--------------------------------------------------
! ------------------------------------------------
! * Author:			Berlin Brown
! * File:			botchart.factor
! * Description:	Simple use of google chart to
! *					generate charts with factor
! * Libraries:		Use of threading library, http library
! * Vocabulary:		botchart
! ------------------------------------------------

USING: kernel
	   random
	   io
	   io.files
	   namespaces
	   math
	   hashtables 
	   sequences
	   vars
	   prettyprint
	   http
	   http.client
	   tools.time
	   threads ;

IN: botchart

VAR: loop

: max-worker-threads    100   ; inline
: max-worker-delay      10000 ; inline
: max-daemon-delay      30000 ; inline

: main-request-url "http://localhost:9080/testwebapp/index.jsp" ; inline

: worker ( -- )
	"Launching worker thread " print
	! **********************************		
	! * Connect to the HTTP server and then delay
	! * for at most 10 seconds.
	! **********************************
	max-worker-delay random dup sleep
	"Worker sleep done at t=" write pprint " ms" print
	
	! **********************************
	! * Perform the http request and calculate the execution time
	! **********************************
	[ main-request-url http-get pprint nl ] time 
	"Done " print ;

: run-manager-daemon ( -- )
    "Waiting up to 30 seconds for aux worker threads to complete" print  				
	max-daemon-delay random dup
	sleep "Manager thread done at t=" write pprint " ms" print ;

! ### : toggle-loop ( -- ) loop> [ loop off ] [ loop on [ run ] in-thread ] if ;

: init-botchart ( -- )
	loop on
  	"INFO: launching loop" print
	max-worker-threads [ [ worker ] in-thread ] times
	run-manager-daemon ;
! ------------------------------------------------
! Main vocabulary entry point definition
! ------------------------------------------------
	
MAIN: init-botchart

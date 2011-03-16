! ======================================
! Simple load test of a http web server 
! ======================================

USING: assocs http http.client kernel math math.parser namespaces sequences
  io io.sockets io.streams.string io.files strings splitting prettyprint ; 
IN: httptest 

: load-all-urls ( filename -- seq )
  <file-reader> [
    stdio get contents
  ] with-stream "\n" split ;

: load-test-url ( url -- )
  dup length 0 > [
   [ "loading url... | " write write " |" print ] keep
   http-get pprint pprint pprint  
  ] [ drop ] if ;

: http-connect ( -- )
  "Connecting to server" print
  "urls.txt" load-all-urls 
  [ load-test-url ] each
  "Done." print

MAIN: http-connect ;



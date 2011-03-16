! ======================================
! Simple csvparser usage
! ======================================

USING: kernel sequences io io.files namespaces combinators prettyprint csvparser ;
IN: client.parse

: parse-document ( -- )
   "Running simple csv parser example" print
   "Opening file, testcsv.txt" print
   "Number of lines in file: " print
   "testcsv.txt" <file-reader> csv length pprint
   "\nDone." print

MAIN: parse-document ;

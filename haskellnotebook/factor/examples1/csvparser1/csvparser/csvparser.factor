! ======================================
! Simple csvparser
! Source from author,  Phil Dawes
! http://www.phildawes.net/blog/2007/10/02/baby-steps-with-factor-a-csvparser/
! ======================================

USING: kernel sequences io io.files namespaces combinators prettyprint ;
IN: csvparser
DEFER: quoted-field

: not-quoted-field ( -- endchar )
  ",\"\n\s\t" read-until    ! Handle quote
  dup
  { { CHAR: \s  [ drop % not-quoted-field ] } ! Note: skip whitespace
    { CHAR: \t  [ drop % not-quoted-field ] } 
    { CHAR: ,   [ swap % ] } 
    { CHAR: "   [ drop drop quoted-field ] }  ! Handle quote
    { CHAR: \n  [ swap % ] }    
    { f         [ swap % ] }                  ! End of File
  } case ;

: maybe-escaped-quote ( -- endchar )
  read1 
  dup
  { { CHAR: "   [ , quoted-field ] }       ! Handle quote quote as an escaped quote
    { CHAR: \s  [ drop not-quoted-field ] } 
    { CHAR: \t  [ drop not-quoted-field ] } 
    [ drop ]
  } case ;

: quoted-field ( -- endchar )
  "\"" read-until                           ! Handle Quote
  drop % maybe-escaped-quote ;

: field ( -- string sep )
  [ not-quoted-field ] "" make swap ;

: (row) ( -- sep )
  field swap , 
  dup CHAR: , = [ drop (row) ] when ;

: row ( -- array[string] eof? )
  [ (row) ] { } make swap ;

: (csv) ( -- )
  row swap , [ (csv) ] when ;

: csv-row ( stream -- row )
  [ row drop ] with-stream ;

: csv ( stream -- rows )
  [ [ (csv) ] { } make ] with-stream ;
	

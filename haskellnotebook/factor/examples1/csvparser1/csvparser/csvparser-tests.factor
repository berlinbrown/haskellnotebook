! tests nicked from the wikipedia csv article
! http://en.wikipedia.org/wiki/Comma-separated_values

[ { { "1997" "Ford" "E350" } } ] 
[ "1997,Ford,E350" <string-reader> csv ] unit-test

[ { { "1997" "Ford" "E350" } } ]
[ "1997,   Ford   , E350" <string-reader> csv ] unit-test

[ { { "1997" "Ford" "E350" "Super, luxurious truck" } } ]
[ "1997,Ford,E350,\"Super, luxurious truck\"" <string-reader> csv ] unit-test

[ { { "1997" "Ford" "E350" "Super \"luxurious\" truck" } } ]
[ "1997,Ford,E350,\"Super \"\"luxurious\"\" truck\"" 
  <string-reader> csv ] unit-test

[ { { "1997" "Ford" "E350" "Go get one now\nthey are going fast" } } ]
[ "1997,Ford,E350,\"Go get one now\nthey are going fast\""
  <string-reader> csv ] unit-test

[ { { "1997" "Ford" "E350" "  Super luxurious truck    " } } ]
[ "1997,Ford,E350,\"  Super luxurious truck    \""
  <string-reader> csv ] unit-test

[ { { "1997" "Ford" "E350" } } ]
[ "\"1997\",\"Ford\",\"E350\"" <string-reader> csv ] unit-test

[ { { "Year" "Make" "Model" } 
    { "1997" "Ford" "E350" }
    { "2000" "Mercury" "Cougar" } } ]
[ "Year,Make,Model\n1997,Ford,E350\n2000,Mercury,Cougar" 
   <string-reader> csv ] unit-test

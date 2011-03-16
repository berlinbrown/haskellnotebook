-module( home_controller ).
-export( [ private/0, index/1 ] ).

private() -> false.
 
index( _A ) ->
    { data, {} }.

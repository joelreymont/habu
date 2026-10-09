\ 2>r 2r> 2r@ on a pair of a three-cell and a one-cell value, either way up,
\ beside >r and r>: each moves its cells as one block in their stack order
\ (habu2.f:10275-10290 LP2RS), so a value pushed by one spelling comes back
\ whole by the other.
STRUCTURE trip 0
   FIELD a n
   FIELD b n
   FIELD c n
;STRUCTURE
: SHOW ( trip -- ) TRIP:UNMAKE rot . swap . . ;
: F ( trip n -- trip n ) 2>r 2r> ;
: G ( trip n -- trip n trip n ) 2>r 2r@ 2r> ;
: H ( trip n -- n trip ) 2>r r> r> ;
: K ( trip n -- n trip ) >r >r 2r> ;
: F2 ( n trip -- n trip ) 2>r 2r> ;
: H2 ( n trip -- trip n ) 2>r r> r> ;
: MAIN ( -- )
   1 2 3 TRIP:MAKE 4 F . SHOW
   5 6 7 TRIP:MAKE 8 G . SHOW . SHOW
   9 10 11 TRIP:MAKE 12 H SHOW .
   13 14 15 TRIP:MAKE 16 K SHOW .
   17 18 19 20 TRIP:MAKE F2 SHOW .
   21 22 23 24 TRIP:MAKE H2 . SHOW ;
MAIN

\ Every transport, and >r r@ r>, on operands of one and three cells; each
\ word's results are printed cell by cell, so a value moved whole is a value
\ printed in order.
STRUCTURE trip 0
   FIELD a n
   FIELD b n
   FIELD c n
;STRUCTURE
: SHOW ( trip -- ) TRIP:UNMAKE rot . swap . . ;
: T ( n -- trip ) dup 1+ over 2 + TRIP:MAKE ;
: X-DUP ( trip -- trip trip ) dup ;
: X-DROP ( n trip -- n ) drop ;
: X-SWAP ( trip n -- n trip ) swap ;
: X-SWAPN ( n trip -- trip n ) swap ;
: X-OVER ( trip n -- trip n trip ) over ;
: X-OVERN ( n trip -- n trip n ) over ;
: X-NIP ( trip n -- n ) nip ;
: X-NIPN ( n trip -- trip ) nip ;
: X-TUCK ( trip n -- n trip n ) tuck ;
: X-TUCKN ( n trip -- trip n trip ) tuck ;
: X-ROT ( trip n trip -- n trip trip ) rot ;
: X--ROT ( trip n trip -- trip trip n ) -rot ;
: X-2DUP ( trip n -- trip n trip n ) 2dup ;
: X-2DROP ( n trip n -- n ) 2drop ;
: X-2SWAP ( trip n n trip -- n trip trip n ) 2swap ;
: X-2OVER ( trip n n trip -- trip n n trip trip n ) 2over ;
: X-RS ( trip n -- trip trip n n ) >r >r r@ r> r@ r> ;
: MAIN ( -- )
   1 T X-DUP SHOW SHOW
   4 7 T X-DROP .
   11 T 14 X-SWAP SHOW .
   15 16 T X-SWAPN . SHOW
   20 T 23 X-OVER SHOW . SHOW
   24 25 T X-OVERN . SHOW .
   30 T 33 X-NIP .
   34 35 T X-NIPN SHOW
   40 T 43 X-TUCK . SHOW .
   44 45 T X-TUCKN SHOW . SHOW
   50 T 53 54 T X-ROT SHOW SHOW .
   60 T 63 64 T X--ROT . SHOW SHOW
   70 T 73 X-2DUP . SHOW . SHOW
   80 81 T 84 X-2DROP .
   90 T 93 94 95 T X-2SWAP . SHOW SHOW .
   100 T 103 104 105 T X-2OVER . SHOW SHOW . . SHOW
   110 T 113 X-RS . . SHOW SHOW ;
MAIN

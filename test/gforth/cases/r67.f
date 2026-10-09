\ Transports over values wider than a cell's bits: an 81-cell structure moved
\ whole past 1-cell values, printed cell by cell, so a value moved whole is a
\ value printed in order.
STRUCTURE trip 0
   FIELD a n
   FIELD b n
   FIELD c n
;STRUCTURE
STRUCTURE n9 0
   FIELD a trip
   FIELD b trip
   FIELD c trip
;STRUCTURE
STRUCTURE n27 0
   FIELD a n9
   FIELD b n9
   FIELD c n9
;STRUCTURE
STRUCTURE n81 0
   FIELD a n27
   FIELD b n27
   FIELD c n27
;STRUCTURE
: T3 ( n -- trip ) dup 1+ over 2 + TRIP:MAKE ;
: T9 ( n -- n9 ) dup T3 over 3 + T3 rot 6 + T3 N9:MAKE ;
: T27 ( n -- n27 ) dup T9 over 9 + T9 rot 18 + T9 N27:MAKE ;
: T81 ( n -- n81 ) dup T27 over 27 + T27 rot 54 + T27 N81:MAKE ;
: S3 ( trip -- ) TRIP:UNMAKE rot . swap . . ;
: S9 ( n9 -- ) N9:UNMAKE rot S3 swap S3 S3 ;
: S27 ( n27 -- ) N27:UNMAKE rot S9 swap S9 S9 ;
: S81 ( n81 -- ) N81:UNMAKE rot S27 swap S27 S27 cr ;
: F ( n81 n -- n n81 ) swap ;
: G ( n n81 -- n81 n ) swap ;
: R ( n81 n n81 -- n n81 n81 ) rot ;
: RR ( n81 n n81 -- n81 n81 n ) -rot ;
: MAIN ( -- )
   1 T81 99 F S81 . cr
   7 101 T81 G . cr S81
   201 T81 5 301 T81 R S81 S81 . cr
   401 T81 6 501 T81 RR . cr S81 S81 ;
MAIN

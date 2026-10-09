\ Locals declared inside begin/until, a 3-cell local among them, and inside
\ if/then, each read inside its block.
STRUCTURE trip 0
   FIELD a n
   FIELD b n
   FIELD c n
;STRUCTURE
: SHOW ( trip -- ) TRIP:UNMAKE rot . swap . . ;
: DOWN ( n -- n ) begin {: x :} x . x 1 - dup 0 = until ;
: PICK2 ( n n -- n ) 2dup < if {: a b :} b a - else drop then ;
: ROLL3 ( trip n -- trip ) begin {: t:trip k :} t TRIP:UNMAKE rot TRIP:MAKE dup SHOW k 1 - dup 0 = until drop ;
: MAIN ( -- )
   3 DOWN .
   2 7 PICK2 .  7 2 PICK2 .
   1 2 3 TRIP:MAKE 3 ROLL3 SHOW ;
MAIN

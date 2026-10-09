\ A generic family at a three-cell instantiation: grow<trip> reserves six
\ payload slots where the declaration reserves two, so every variant carries
\ its declared pads and the instantiation's extra ones under the tag, whether
\ `construct` or the generated constructor builds it (habu2.f:12566-12577),
\ and its MATCH arm drops both (habu2.f:12700-12712).
STRUCTURE trip 0
   FIELD a n
   FIELD b n
   FIELD c n
;STRUCTURE
ENUM grow 1
   VARIANT g0 ;VARIANT
   VARIANT g1 FIELD x a ;VARIANT
   VARIANT g2 FIELD x a FIELD y a ;VARIANT
;ENUM
: SHOW ( trip -- ) TRIP:UNMAKE rot . swap . . ;
: T ( n -- trip ) dup 1+ over 2 + TRIP:MAKE ;
: TAKE ( grow<trip> -- )
   MATCH grow
      g0 OF 0 . ENDOF
      g1 OF SHOW ENDOF
      g2 OF SHOW SHOW ENDOF
   ;MATCH ;
\ One cell carried across the whole bundle: it comes back only when the bundle
\ is as wide as its instantiation.
: VIA ( grow<trip> -- grow<trip> ) 99 swap >r . r> ;
: MK0 ( -- grow<trip> ) construct grow g0 ;
: MK1 ( trip -- grow<trip> ) construct grow g1 ;
: MK2 ( trip trip -- grow<trip> ) construct grow g2 ;
: CL0 ( -- grow<trip> ) GROW:G0 ;
: CL1 ( trip -- grow<trip> ) GROW:G1 ;
: CL2 ( trip trip -- grow<trip> ) GROW:G2 ;
: MAIN ( -- )
   MK0 VIA TAKE
   1 T MK1 VIA TAKE
   4 T 7 T MK2 VIA TAKE
   CL0 VIA TAKE
   10 T CL1 VIA TAKE
   13 T 16 T CL2 VIA TAKE ;
MAIN

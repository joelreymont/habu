ENUM shape 0
   VARIANT dot ;VARIANT
   VARIANT circle FIELD r n ;VARIANT
   VARIANT rect FIELD w n FIELD h n ;VARIANT
;ENUM

: AREA ( shape -- n )
   MATCH shape
      dot OF 0 ENDOF
      circle OF dup * 3 * ENDOF
      rect OF * ENDOF
   ;MATCH ;
\ dup and swap of a three-cell value
: DOUBLE-AREA ( shape -- n ) dup AREA swap AREA + ;
: C07 ( -- )
   2 5 construct shape rect DOUBLE-AREA .
   3 construct shape circle DOUBLE-AREA . ;
C07

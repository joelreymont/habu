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
\ construct and a MATCH whose arms take 0, 1 and 2 payload cells
: C01 ( -- )
   construct shape dot AREA .
   5 construct shape circle AREA .
   4 6 construct shape rect AREA . ;
C01

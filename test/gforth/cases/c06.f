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
\ case arms that each build a value; the default arm swaps a value under the selector
: MAKE ( n -- shape )
   case
      0 of construct shape dot endof
      1 of 2 construct shape circle endof
      3 4 construct shape rect swap
   endcase ;
: C06 ( -- )
   0 MAKE AREA .
   1 MAKE AREA .
   9 MAKE AREA . ;
C06

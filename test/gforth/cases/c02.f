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
\ a three-cell value bound to one local and pushed twice
: TWICE-AREA ( shape -- n )
   {: s :}
   s AREA s AREA + ;
: C02 ( -- )
   3 4 construct shape rect TWICE-AREA .
   7 construct shape circle TWICE-AREA . ;
C02

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
\ locals of mixed widths: one cell, three cells, one cell
: MIX ( n shape n -- n )
   {: k:n s v:n :}
   s AREA k * v + ;
: C03 ( -- )
   2 3 4 construct shape rect 1 MIX .
   10 construct shape dot 7 MIX . ;
C03

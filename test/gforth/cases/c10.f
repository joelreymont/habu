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
\ a quotation that builds a three-cell value
: MAKE-WITH ( n [ n -- shape ] -- shape ) execute ;
: C10 ( -- )
   6 [: construct shape circle ;] MAKE-WITH AREA .
   6 [: dup construct shape rect ;] MAKE-WITH AREA . ;
C10

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
\ MATCH arms that drop their payload and leave a string
: NAME ( shape -- ptr u8 n )
   MATCH shape
      dot OF s" dot" ENDOF
      circle OF drop s" circle" ENDOF
      rect OF 2drop s" rect" ENDOF
   ;MATCH ;
: C08 ( -- )
   construct shape dot NAME type cr
   1 construct shape circle NAME type cr
   1 2 construct shape rect NAME type cr ;
C08

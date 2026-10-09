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
\ the generated constructors, a loop over them, and a multi-cell local in a loop
: NTH ( n -- shape )
   dup 0 = if drop SHAPE:DOT exit then
   dup 1 = if SHAPE:CIRCLE exit then
   dup 1 + SHAPE:RECT ;
: C12 ( -- )
   0 4 0 do i NTH {: s :} s AREA + loop . ;
C12

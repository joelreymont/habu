\ dispatch.fs — Phase 0.3 DISPATCH-bound microbench (the fair gate shape).
\ Per byte: load + add + 3x xorshift = ~10 cheap dispatched ops. gforth pays NEXT
\ per op; native folds each `dup lshift xor` into one shifted-EOR. Same hash both
\ ways (seed 1, buf[i]=i&0xff). Run: gforth dispatch.fs -e "BENCH bye".
65536 constant BUFLEN
15000 constant PASSES
create BUF BUFLEN allot
variable RES

: FILL   BUFLEN 0 ?do  i 255 and  BUF i + c!  loop ;

: HASH ( -- h )
  1
  PASSES 0 ?do
    BUFLEN 0 ?do
      BUF i + c@  +
      dup 13 lshift xor
      dup  7 rshift xor
      dup 17 lshift xor
    loop
  loop ;

: BENCH ( -- )
  FILL
  utime  HASH RES !  utime
  2swap d- d>s
  ." us=" .  ." result=" RES @ 255 and .  cr ;

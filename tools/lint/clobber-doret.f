\ clobber-doret.f - fixture read as text by clobber-lint-test.f (never loaded).

\ `do` takes its first turn, so RET is always emitted before the code after
\ `loop`, which never runs. Real: 0.
variable LDO-CALLEE
: EMIT-DO-CALLEE ( -- )
   LDO-CALLEE LABEL@ LBL,
   9 0 MOVZ,  RET, ;
: EMIT-DO ( n -- ) {: n:n :}
   n 0 do RET, loop
   9 0 MOVZ,
   LDO-CALLEE LABEL@ BL,
   4 9 0 LDRB, ;

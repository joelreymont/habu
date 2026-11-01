\ clobber-preif.f - fixture read as text by clobber-lint-test.f (never loaded).

\ x9 is written before the `if`; when early is false it is clobbered and
\ read. Real: 1.
variable LPI-CALLEE
: EMIT-PI-CALLEE ( -- )
   LPI-CALLEE LABEL@ LBL,
   9 0 MOVZ,  RET, ;
: EMIT-PI ( bool -- ) {: early:bool :}
   9 0 MOVZ,
   early if RET, then
   LPI-CALLEE LABEL@ BL,
   4 9 0 LDRB, ;

\ clobber-endedpre.f - fixture read as text by clobber-lint-test.f (never loaded).

\ RET before the block: the code after `then` never runs. Real: 0.
variable LEP-CALLEE
: EMIT-EP-CALLEE ( -- )
   LEP-CALLEE LABEL@ LBL,
   9 0 MOVZ,  RET, ;
: EMIT-EP ( bool -- ) {: early:bool :}
   RET,
   early if RET, then
   9 0 MOVZ,
   LEP-CALLEE LABEL@ BL,
   4 9 0 LDRB, ;

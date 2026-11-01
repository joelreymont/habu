\ clobber-arm-if.f - fixture for a RET in a host `if`, read as text by
\ clobber-lint-test.f (never loaded).

variable LAI-CALLEE

: EMIT-AI-CALLEE ( -- )
   LAI-CALLEE LABEL@ LBL,
   9 0 MOVZ,  RET, ;

\ When early is false the code after `then` runs: x9 is written, clobbered by
\ the callee and read. One finding.
: EMIT-AI ( bool -- ) {: early:bool :}
   early if RET, then
   9 0 MOVZ,
   LAI-CALLEE LABEL@ BL,
   4 9 0 LDRB, ;

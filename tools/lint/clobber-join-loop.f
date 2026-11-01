\ clobber-join-loop.f - fixture for a back edge carrying poison, read as text by
\ clobber-lint-test.f (never loaded).

variable LJOIN-CALLEE

: EMIT-JOIN-CALLEE ( -- )
   LJOIN-CALLEE LABEL@ LBL,
   12 0 MOVZ,  RET, ;

\ The second time round, head reads the x12 the call clobbered: one finding.
: EMIT-JOIN-LOOP ( -- )
   LBL {: head :}
   12 9 0 ADDI,
   head LBL,
   4 12 0 LDRB,
   LJOIN-CALLEE LABEL@ BL,        \ clobbers x12
   head B, ;

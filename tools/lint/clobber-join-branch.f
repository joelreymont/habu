\ clobber-join-branch.f - fixture for a branch carrying poison, read as text by
\ clobber-lint-test.f (never loaded).

variable LJOIN-CALLEE

: EMIT-JOIN-CALLEE ( -- )
   LJOIN-CALLEE LABEL@ LBL,
   12 0 MOVZ,  RET, ;

\ The fall-through rewrites x12, but the branch brings the clobbered x12 to
\ join: one finding.
: EMIT-JOIN-BRANCH ( -- )
   LBL {: join :}
   12 9 0 ADDI,
   LJOIN-CALLEE LABEL@ BL,        \ clobbers x12
   9 join CBNZ,
   12 9 0 ADDI,
   join LBL,
   4 12 0 LDRB, ;

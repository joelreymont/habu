\ clobber-local-literal.f - fixture for a register literal below the cell a
\ mid-body group binds, read as text by clobber-lint-test.f (never loaded).

variable LRL-CALLEE

: EMIT-RL-CALLEE ( -- )
   LRL-CALLEE LABEL@ LBL,
   12 0 MOVZ,  RET, ;               \ clobbers x12

\ MOVZ x12, #7 rewrites x12 after the BL: no finding.
: EMIT-RL ( -- )
   12 0 MOVZ,
   LRL-CALLEE LABEL@ BL,
   12 7 {: k :} k MOVZ,
   4 12 0 LDRB, ;

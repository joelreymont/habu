\ clobber-dead-leak.f - fixture for dead code between a RET and a label, read
\ as text by clobber-lint-test.f (never loaded).

variable LDL-CALLEE

: EMIT-DL-CALLEE ( -- )
   LDL-CALLEE LABEL@ LBL,
   9 0 MOVZ,  RET, ;

\ The write and call after RET never run; only the branch reaches l, with x9
\ intact. No finding.
: EMIT-DL ( -- )
   LBL {: l:label :}
   9 0 MOVZ,
   10 l CBZ,
   RET,
   9 0 MOVZ,
   LDL-CALLEE LABEL@ BL,
   l LBL,
   4 9 0 LDRB, ;

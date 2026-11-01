\ clobber-join-dead.f - fixture for a dead path, read as text by
\ clobber-lint-test.f (never loaded). Nothing runs after an exit system call.

variable LJOIN-CALLEE

: EMIT-JOIN-CALLEE ( -- )
   LJOIN-CALLEE LABEL@ LBL,
   12 0 MOVZ,  RET, ;

\ Only the branch reaches skip, and x12 is intact on it: no finding.
: EMIT-JOIN-DEAD ( -- )
   LBL {: skip :}
   12 9 0 ADDI,
   9 skip CBNZ,
   LJOIN-CALLEE LABEL@ BL,        \ clobbers x12
   0 0 MOVZ,  NR-EXIT-GROUP SYS,
   skip LBL,
   4 12 0 LDRB, ;

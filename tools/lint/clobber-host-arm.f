\ clobber-host-arm.f - fixture for a local that dies at its arm's closer, read
\ as text by clobber-lint-test.f (never loaded).

: REWRITE-ARG ( n -- ) {: r:n :}
   r 0 MOVZ, ;

variable LARM2-CALLEE

: EMIT-ARM2-CALLEE ( -- )
   LARM2-CALLEE LABEL@ LBL,
   9 0 MOVZ,  RET, ;                \ answers in x9

\ rewrite-arg after then is REWRITE-ARG: the LDR writes x12, no finding.
: EMIT-ARM2 ( n bool -- )
   if {: rewrite-arg :} rewrite-arg 0 MOVZ, else drop then
   9 rewrite-arg
   12 10 0 LDR,
   LARM2-CALLEE LABEL@ BL,
   4 9 0 LDRB, ;

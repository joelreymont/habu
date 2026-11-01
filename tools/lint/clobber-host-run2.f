\ clobber-host-run2.f - fixture for a host call ending pass 2's operand run,
\ read as text by clobber-lint-test.f (never loaded).

: REWRITE-ARG ( n -- ) {: r:n :}
   r 0 MOVZ, ;

variable LRUN2-CALLEE

: EMIT-RUN2-CALLEE ( -- )
   LRUN2-CALLEE LABEL@ LBL,
   9 0 MOVZ,  RET, ;                \ answers in x9

\ The LDR's operands are 12 10 0, so it writes x12, not x9, and x9 holds the
\ answer: no finding.
: EMIT-RUN2 ( -- )
   9 REWRITE-ARG
   12 10 0 LDR,
   LRUN2-CALLEE LABEL@ BL,
   4 9 0 LDRB, ;

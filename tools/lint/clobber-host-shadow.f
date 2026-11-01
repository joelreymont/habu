\ clobber-host-shadow.f - fixture for a body local spelled like an earlier
\ host word, read as text by clobber-lint-test.f (never loaded).

: SCRATCH ( n -- ) {: r:n :}
   r 0 MOVZ, ;

variable LSHADOW-CALLEE

: EMIT-SHADOW-CALLEE ( -- )
   LSHADOW-CALLEE LABEL@ LBL,
   0 0 MOVZ,  RET, ;                \ answers in x0

\ The local hides SCRATCH: the LDR writes x9 from the address in x12, so x0 is
\ unwritten when the callee answers in it: no finding.
: EMIT-SHADOW ( -- )
   12 {: scratch :}
   9 scratch 0 LDR,
   LSHADOW-CALLEE LABEL@ BL,
   4 0 0 LDRB, ;

\ clobber-arm-loop.f - fixture for a RET in a host loop, read as text by
\ clobber-lint-test.f (never loaded).

variable LAL-CALLEE

: EMIT-AL-CALLEE ( -- )
   LAL-CALLEE LABEL@ LBL,
   9 0 MOVZ,  RET, ;

\ The loop may run no times, so the code after it runs. One finding.
: EMIT-AL ( n -- ) {: n:n :}
   n 0 ?do RET, loop
   9 0 MOVZ,
   LAL-CALLEE LABEL@ BL,
   4 9 0 LDRB, ;

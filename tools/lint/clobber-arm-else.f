\ clobber-arm-else.f - fixture for a RET in a host `if` arm with an `else`,
\ read as text by clobber-lint-test.f (never loaded).

variable LAE-CALLEE

: EMIT-AE-CALLEE ( -- )
   LAE-CALLEE LABEL@ LBL,
   9 0 MOVZ,  RET, ;

\ The `else` arm writes x9, calls the callee and reads x9. One finding.
: EMIT-AE ( bool -- ) {: early:bool :}
   early if
      RET,
   else
      9 0 MOVZ,
      LAE-CALLEE LABEL@ BL,
      4 9 0 LDRB,
   then ;

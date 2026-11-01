\ clobber-ifelse.f - fixture read as text by clobber-lint-test.f (never loaded).

\ The arms are alternatives: the read in the else arm never follows the if
\ arm's write and call. Real: 0.
variable LIE-CALLEE
: EMIT-IE-CALLEE ( -- )
   LIE-CALLEE LABEL@ LBL,
   9 0 MOVZ,  RET, ;
: EMIT-IE ( bool -- ) {: f:bool :}
   f if
      9 0 MOVZ,
      LIE-CALLEE LABEL@ BL,
   else
      4 9 0 LDRB,
   then ;

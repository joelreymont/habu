\ clobber-nested.f - fixture read as text by clobber-lint-test.f (never loaded).

\ The inner RET is skipped when either flag is false. Real: 1.
variable LNE-CALLEE
: EMIT-NE-CALLEE ( -- )
   LNE-CALLEE LABEL@ LBL,
   9 0 MOVZ,  RET, ;
: EMIT-NE ( bool bool -- ) {: a:bool b:bool :}
   a if b if RET, then then
   9 0 MOVZ,
   LNE-CALLEE LABEL@ BL,
   4 9 0 LDRB, ;

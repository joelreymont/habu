\ clobber-both.f - fixture read as text by clobber-lint-test.f (never loaded).

\ Both arms return, so the code after `then` never runs. Real: 0.
variable LBO-CALLEE
: EMIT-BO-CALLEE ( -- )
   LBO-CALLEE LABEL@ LBL,
   9 0 MOVZ,  RET, ;
: EMIT-BO ( bool -- ) {: early:bool :}
   early if RET, else RET, then
   9 0 MOVZ,
   LBO-CALLEE LABEL@ BL,
   4 9 0 LDRB, ;

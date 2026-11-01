\ clobber-whileret.f - fixture read as text by clobber-lint-test.f (never loaded).

\ RET after `while` is skipped when f is false at once. Real: 1.
variable LWR-CALLEE
: EMIT-WR-CALLEE ( -- )
   LWR-CALLEE LABEL@ LBL,
   9 0 MOVZ,  RET, ;
: EMIT-WR ( bool -- ) {: f:bool :}
   begin f while RET, repeat
   9 0 MOVZ,
   LWR-CALLEE LABEL@ BL,
   4 9 0 LDRB, ;

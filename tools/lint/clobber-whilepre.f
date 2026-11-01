\ clobber-whilepre.f - fixture read as text by clobber-lint-test.f (never loaded).

\ RET before `while` is emitted on every turn, so the code after `repeat`
\ never runs. Real: 0.
variable LWP-CALLEE
: EMIT-WP-CALLEE ( -- )
   LWP-CALLEE LABEL@ LBL,
   9 0 MOVZ,  RET, ;
: EMIT-WP ( bool -- ) {: f:bool :}
   begin RET, f while 10 0 MOVZ, repeat
   9 0 MOVZ,
   LWP-CALLEE LABEL@ BL,
   4 9 0 LDRB, ;

\ clobber-untilret.f - fixture read as text by clobber-lint-test.f (never loaded).

\ A `begin … until` body runs at least once, so RET precedes the code after
\ `until`, which never runs. Real: 0.
variable LUN-CALLEE
: EMIT-UN-CALLEE ( -- )
   LUN-CALLEE LABEL@ LBL,
   9 0 MOVZ,  RET, ;
: EMIT-UN ( bool -- ) {: f:bool :}
   begin RET, f until
   9 0 MOVZ,
   LUN-CALLEE LABEL@ BL,
   4 9 0 LDRB, ;

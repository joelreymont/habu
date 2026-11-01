\ clobber-leave.f - fixture read as text by clobber-lint-test.f (never loaded).

\ `leave` before RET exits the first turn, so the code after `loop` runs.
\ Real: 1.
variable LLV-CALLEE
: EMIT-LV-CALLEE ( -- )
   LLV-CALLEE LABEL@ LBL,
   9 0 MOVZ,  RET, ;
: EMIT-LV ( n bool -- ) {: n:n f:bool :}
   n 0 do f if leave then RET, loop
   9 0 MOVZ,
   LLV-CALLEE LABEL@ BL,
   4 9 0 LDRB, ;

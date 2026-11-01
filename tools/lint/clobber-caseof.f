\ clobber-caseof.f - fixture read as text by clobber-lint-test.f (never loaded).

\ RET in one `of` arm; other selectors reach the code after `endcase`. Real: 1.
variable LCO-CALLEE
: EMIT-CO-CALLEE ( -- )
   LCO-CALLEE LABEL@ LBL,
   9 0 MOVZ,  RET, ;
: EMIT-CO ( n -- ) {: s:n :}
   s case 1 of RET, endof endcase
   9 0 MOVZ,
   LCO-CALLEE LABEL@ BL,
   4 9 0 LDRB, ;

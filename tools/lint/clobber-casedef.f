\ clobber-casedef.f - fixture read as text by clobber-lint-test.f (never loaded).

\ RET in the default arm; selector 1 reaches the code after `endcase`. Real: 1.
variable LCD-CALLEE
: EMIT-CD-CALLEE ( -- )
   LCD-CALLEE LABEL@ LBL,
   9 0 MOVZ,  RET, ;
: EMIT-CD ( n -- ) {: s:n :}
   s case 1 of 11 0 MOVZ, endof RET, endcase
   9 0 MOVZ,
   LCD-CALLEE LABEL@ BL,
   4 9 0 LDRB, ;

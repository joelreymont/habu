\ B opens in the if arm and leaves past `then`; C opens in the else arm, which
\ B never enters. Real: 0.
variable LX2-B
variable LX2-C
: EMIT-X2-ROUTINES ( bool -- ) {: f:bool :}
   f if
      LX2-B LABEL@ LBL,
      11 0 MOVZ,
   else
      LX2-C LABEL@ LBL,
      9 0 MOVZ,  RET,
   then
   RET, ;
: EMIT-X2 ( -- )
   9 0 MOVZ,
   LX2-B LABEL@ BL,
   4 9 0 LDRB, ;

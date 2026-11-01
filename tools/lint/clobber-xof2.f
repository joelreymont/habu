\ B opens in one of arm and leaves through endof; C opens in the next arm,
\ which B never enters. Real: 0.
variable LXQ-B
variable LXQ-C
: EMIT-XQ-ROUTINES ( n -- ) {: s:n :}
   s case
      1 of  LXQ-B LABEL@ LBL,  11 0 MOVZ,  endof
      2 of  LXQ-C LABEL@ LBL,  9 0 MOVZ,  RET,  endof
   endcase
   RET, ;
: EMIT-XQ ( -- )
   9 0 MOVZ,
   LXQ-B LABEL@ BL,
   4 9 0 LDRB, ;

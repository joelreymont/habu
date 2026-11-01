\ A top-level MATCH arm calls the callee; every arm reaches the read. Real: 1.
variable LMF-CALLEE
: EMIT-MF-CALLEE ( -- )
   LMF-CALLEE LABEL@ LBL,
   9 0 MOVZ,  RET, ;
: EMIT-MF ( option -- ) {: o:option :}
   9 0 MOVZ,
   o MATCH option
      none OF LMF-CALLEE LABEL@ BL, ENDOF
      some OF drop LMF-CALLEE LABEL@ BL, ENDOF
   ;MATCH
   4 9 0 LDRB, ;

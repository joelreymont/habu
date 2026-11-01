\ A MATCH arm calls the callee in the if arm; the else arm reads x9, written
\ before the if and never clobbered on its path. Real: 0.
variable LME-CALLEE
: EMIT-ME-CALLEE ( -- )
   LME-CALLEE LABEL@ LBL,
   9 0 MOVZ,  RET, ;
: EMIT-ME ( bool option -- ) {: f:bool o:option :}
   9 0 MOVZ,
   f if
      o MATCH option
         none OF LME-CALLEE LABEL@ BL, ENDOF
         some OF drop ENDOF
      ;MATCH
      BRK,
   else
      4 9 0 LDRB,
   then ;

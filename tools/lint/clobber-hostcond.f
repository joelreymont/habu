\ The 9 in the host test is no register: x9 is never written here. Real: 0.
variable LHC-CALLEE
: EMIT-HC-CALLEE ( -- )
   LHC-CALLEE LABEL@ LBL,
   9 0 MOVZ,  RET, ;
: EMIT-HC ( n -- ) {: n:n :}
   n 9 = if 10 0 MOVZ, then
   LHC-CALLEE LABEL@ BL,
   4 9 0 LDRB, ;

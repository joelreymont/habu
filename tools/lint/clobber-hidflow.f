\ The group hides the 9 in its run, which ends at `if`; the MOVZ in the arm
\ reads its own operands, so x10 is written, clobbered and read. Real: 1.
variable LHF-CALLEE
: EMIT-HF-CALLEE ( -- )
   LHF-CALLEE LABEL@ LBL,
   10 0 MOVZ,  RET, ;
: EMIT-HF ( n bool -- ) {: n:n f:bool :}
   9 n SOMEHOST {: r:n :}
   f if 10 0 MOVZ, then
   LHF-CALLEE LABEL@ BL,
   4 10 0 LDRB, ;

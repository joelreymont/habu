\ clobber-host-epi2.f - fixture for a host epilogue ending a path before a
\ label only a branch reaches, read as text by clobber-lint-test.f (never
\ loaded).

variable LEPI2-CALLEE

: EMIT-EPI2-CALLEE ( -- )
   LEPI2-CALLEE LABEL@ LBL,
   13 0 MOVZ,  RET, ;               \ answers in x13

: EPILOGUE ( -- )
   30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;

\ The x13 write ends in EPILOGUE and never reaches live, so x13 after the BL
\ is the answer: no finding.
: EMIT-EPI2 ( -- )
   LBL {: live :}
   9 live CBNZ,
   13 0 MOVZ,  EPILOGUE
   live LBL,
   LEPI2-CALLEE LABEL@ BL,
   4 13 0 LDRB, ;

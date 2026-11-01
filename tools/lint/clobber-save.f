\ A full-width store to the machine stack saves x5, not a read; a byte store
\ there writes a buffer, so x6 is written, clobbered and read. Real: 1.
variable LSV-CALLEE
: EMIT-SV-CALLEE ( -- )
   LSV-CALLEE LABEL@ LBL,
   5 0 MOVZ,  6 0 MOVZ,  RET, ;
: EMIT-SV ( -- )
   5 7 MOVZ,  6 7 MOVZ,
   LSV-CALLEE LABEL@ BL,
   SP SP 16 SUBI,  5 SP 0 STR,  6 SP 8 STRB,  SP SP 16 ADDI, ;

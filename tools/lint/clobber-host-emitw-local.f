\ clobber-host-emitw-local.f - fixture for a local named emitw, read as text
\ by clobber-lint-test.f (never loaded).

variable LEW2-CALLEE

: EMIT-EW2-CALLEE ( -- )
   LEW2-CALLEE LABEL@ LBL,
   9 0 MOVZ,  RET, ;                \ clobbers x9

\ x9 is held across the BL and read: one real finding.
: EMIT-EW2 ( n -- )
   {: emitw :}
   9 0 MOVZ,
   LEW2-CALLEE LABEL@ BL,
   4 9 emitw LDRB, ;

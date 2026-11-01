\ clobber-host-emitw-qualified.f - fixture for EMITW called by its qualified
\ name, read as text by clobber-lint-test.f (never loaded).

variable LQE-CALLEE

: EMIT-QE-CALLEE ( -- )
   LQE-CALLEE LABEL@ LBL,
   9 0 MOVZ,  RET, ;

\ EMITW rewrites x9 (mrs x9, CNTVCT_EL0): no finding.
: EMIT-QE ( -- )
   9 0 MOVZ,
   LQE-CALLEE LABEL@ BL,
   $D53BE049 A64ICODE:EMITW
   4 9 0 LDRB, ;

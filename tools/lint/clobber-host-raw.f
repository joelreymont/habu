\ clobber-host-raw.f - fixture for EMITW rewriting a clobbered register, read
\ as text by clobber-lint-test.f (never loaded).

variable LRAW-CALLEE

: EMIT-RAW-CALLEE ( -- )
   LRAW-CALLEE LABEL@ LBL,
   9 0 MOVZ,  RET, ;

: READ-CLOCK ( -- )
   $D53BE049 EMITW ;                \ mrs x9, CNTVCT_EL0

\ READ-CLOCK emits through EMITW, so its call is unknown: no finding.
: EMIT-RAW-WORD ( -- )
   9 0 MOVZ,
   LRAW-CALLEE LABEL@ BL,           \ clobbers x9
   READ-CLOCK
   4 9 0 LDRB, ;

\ EMITW itself is unknown: no finding.
: EMIT-RAW-INLINE ( -- )
   9 0 MOVZ,
   LRAW-CALLEE LABEL@ BL,           \ clobbers x9
   $D53BE049 EMITW                  \ mrs x9, CNTVCT_EL0
   4 9 0 LDRB, ;

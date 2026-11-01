\ clobber-barrier.f - fixture for a barrier between a clobbering call and a
\ read, read as text by clobber-lint-test.f (never loaded).

variable LST-CALLEE

: EMIT-ST-CALLEE ( -- )
   LST-CALLEE LABEL@ LBL,
   9 0 MOVZ,  RET, ;                \ clobbers x9

\ x9 is held across the BL and stored after ISB: one finding.
: EMIT-ST ( -- )
   9 0 MOVZ,
   LST-CALLEE LABEL@ BL,
   ISB,
   9 0 STLR, ;

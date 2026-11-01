\ clobber-host-hidden.f - fixture for a host word placing the label a branch
\ reaches after a host epilogue, read as text by clobber-lint-test.f (never
\ loaded).

: EPILOGUE ( -- )
   30 SP 0 LDR,  SP SP 16 ADDI,  RET, ;

: PLACE ( label -- ) {: l:label :}
   l LBL, ;

variable LHD-CALLEE

: EMIT-HD-CALLEE ( -- )
   LHD-CALLEE LABEL@ LBL,
   13 0 MOVZ,  RET, ;               \ answers in x13

\ again is reached only from the CBNZ, where x13 is unwritten: no finding.
: EMIT-HD ( -- )
   LBL {: again :}
   9 again CBNZ,
   13 0 MOVZ,
   EPILOGUE
   again PLACE
   LHD-CALLEE LABEL@ BL,
   4 13 0 LDRB, ;

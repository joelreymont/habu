\ clobber-hidden-ret.f - fixture for a RET ending a run a group hid, read as
\ text by clobber-lint-test.f (never loaded).

variable LHR-A
variable LHR-B

\ The group hides the run RET ends, yet A still returns before the barrier
\ and B: A preserves x9.
: EMIT-HR-ROUTINES ( n -- ) {: u:n :}
   LHR-A LABEL@ LBL,
   u 9 + {: off:n :} RET,
   ISB,
   LHR-B LABEL@ LBL,
   9 0 MOVZ,  RET, ;

\ No finding.
: EMIT-HR ( -- )
   9 0 MOVZ,
   LHR-A LABEL@ BL,
   4 9 0 LDRB, ;

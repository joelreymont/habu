\ clobber-barrier-fall.f - fixture for a barrier after a routine's RET, read as
\ text by clobber-lint-test.f (never loaded).

variable LBF-A
variable LBF-B

\ The ISB after A's RET never runs, so B's write of x9 is not A's.
: EMIT-BF-ROUTINES ( -- )
   LBF-A LABEL@ LBL,
   RET,
   ISB,
   LBF-B LABEL@ LBL,
   9 0 MOVZ,  RET, ;

\ A preserves x9: no finding.
: EMIT-BF ( -- )
   9 0 MOVZ,
   LBF-A LABEL@ BL,
   4 9 0 LDRB, ;

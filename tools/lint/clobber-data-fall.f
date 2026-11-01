\ clobber-data-fall.f - fixture for a routine's literal pool before the next
\ routine, read as text by clobber-lint-test.f (never loaded).

variable LDF-A
variable LDF-B

\ A returns before its pool, so B's write of x9 is not A's.
: EMIT-DF-ROUTINES ( -- )
   LDF-A LABEL@ LBL,
   RET,
   0 DCQ,
   LDF-B LABEL@ LBL,
   9 0 MOVZ,  RET, ;

\ A preserves x9: no finding.
: EMIT-DF ( -- )
   9 0 MOVZ,
   LDF-A LABEL@ BL,
   4 9 0 LDRB, ;

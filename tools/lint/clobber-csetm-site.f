\ clobber-csetm-site.f - fixture for CSETM, rewriting x11 after a BL clobbers
\ it, read as text by clobber-lint-test.f (never loaded).

variable LCS-CALLEE

: EMIT-CS-CALLEE ( -- )
   LCS-CALLEE LABEL@ LBL,
   11 0 MOVZ,  RET, ;               \ clobbers x11

\ CSETM rewrites x11 before the read: no finding.
: EMIT-CS ( -- )
   11 0 MOVZ,
   LCS-CALLEE LABEL@ BL,
   11 0 CSETM,
   4 11 0 LDRB, ;

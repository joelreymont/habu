\ clobber-csetm-routine.f - fixture for a routine writing x11 only by a CSETM,
\ instruction, read as text by clobber-lint-test.f (never loaded).

variable LCS1

: EMIT-CS1 ( -- )
   LCS1 LABEL@ LBL,
   11 0 CSETM,  RET, ;

\ LCS1 clobbers x11, held across its BL: one real finding.
: EMIT-CS1-CALLER ( -- )
   11 0 MOVZ,
   LCS1 LABEL@ BL,
   4 11 0 LDRB, ;

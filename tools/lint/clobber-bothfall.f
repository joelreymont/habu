\ clobber-bothfall.f - fixture read as text by clobber-lint-test.f (never loaded).

\ Both arms of A return, so A never reaches B. Real: 0.
variable LBF-A
variable LBF-B
: EMIT-BF-ROUTINES ( bool -- ) {: early:bool :}
   LBF-A LABEL@ LBL,
   early if RET, else RET, then
   LBF-B LABEL@ LBL,
   9 0 MOVZ,  RET, ;
: EMIT-BF ( -- )
   9 0 MOVZ,
   LBF-A LABEL@ BL,
   4 9 0 LDRB, ;

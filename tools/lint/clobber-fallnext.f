\ clobber-fallnext.f - fixture read as text by clobber-lint-test.f (never loaded).

\ When early is false A falls straight into B, so A clobbers x9. Real: 1.
variable LFN-A
variable LFN-B
: EMIT-FN-ROUTINES ( bool -- ) {: early:bool :}
   LFN-A LABEL@ LBL,
   early if RET, then
   LFN-B LABEL@ LBL,
   9 0 MOVZ,  RET, ;
: EMIT-FN ( -- )
   9 0 MOVZ,
   LFN-A LABEL@ BL,
   4 9 0 LDRB, ;

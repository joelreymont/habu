\ clobber-arm-fall.f - fixture for a fall past a RET in a host `if`, read as
\ text by clobber-lint-test.f (never loaded).

variable LAF-A
variable LAF-B

\ When early is false A falls into B, so A clobbers x9.
: EMIT-AF-ROUTINES ( bool -- ) {: early:bool :}
   LAF-A LABEL@ LBL,
   early if RET, then
   10 0 MOVZ,
   LAF-B LABEL@ LBL,
   9 0 MOVZ,  RET, ;

\ x9 is written, clobbered by A and read. One finding.
: EMIT-AF ( -- )
   9 0 MOVZ,
   LAF-A LABEL@ BL,
   4 9 0 LDRB, ;

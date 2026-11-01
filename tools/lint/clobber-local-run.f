\ clobber-local-run.f - fixture for a local group mid-body consuming its
\ literal, read as text by clobber-lint-test.f (never loaded).

variable LOR-CALLEE

: EMIT-OR-CALLEE ( -- )
   LOR-CALLEE LABEL@ LBL,
   12 0 MOVZ,  RET, ;               \ clobbers x12

\ x12 is never written here, so its read after the BL is no clobber.
: EMIT-OR ( -- )
   12 {: k :}
   9 k MOVZ,
   LOR-CALLEE LABEL@ BL,
   4 12 0 LDRB, ;

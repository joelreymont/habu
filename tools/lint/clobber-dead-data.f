\ clobber-dead-data.f - fixture for a data word on a dead path, read as text by
\ clobber-lint-test.f (never loaded).

variable LDX-CALLEE

: EMIT-DX-CALLEE ( -- )
   LDX-CALLEE LABEL@ LBL,
   9 0 MOVZ,  RET, ;

\ Everything after the first RET is unreachable, the BL's x30 included: no
\ finding.
: EMIT-DX ( -- )
   RET,
   0 DCQ,
   LDX-CALLEE LABEL@ BL,
   RET, ;

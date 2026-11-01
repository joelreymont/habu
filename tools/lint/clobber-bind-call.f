\ clobber-bind-call.f - fixture for a group binding a computed cell, read as
\ text by clobber-lint-test.f (never loaded).

variable LBC-CALLEE

: EMIT-BC-CALLEE ( -- )
   LBC-CALLEE LABEL@ LBL,
   15 0 MOVZ,  RET, ;               \ clobbers x15

\ off binds the cell u 15 + leaves; the STR reads x9 and SP, never x15: no
\ finding.
: EMIT-BC ( n -- ) {: u:n :}
   15 0 MOVZ,
   LBC-CALLEE LABEL@ BL,
   u 15 + {: off:n :} 9 SP off STR, ;

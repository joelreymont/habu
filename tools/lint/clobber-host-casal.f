\ clobber-host-casal.f - fixture for a scanned definition of a modeled
\ mnemonic whose body emits, read as text by clobber-lint-test.f (never
\ loaded).

: CASAL, ( n n n -- ) {: s:n t:n n:n :}
   s n 0 LDAR, ;

variable LCAS-CALLEE

: EMIT-CAS-CALLEE ( -- )
   LCAS-CALLEE LABEL@ LBL,
   13 0 MOVZ,  RET, ;

\ CASAL, keeps its model and compares the clobbered x13: one finding.
: EMIT-CAS ( -- )
   13 0 MOVZ,
   LCAS-CALLEE LABEL@ BL,           \ clobbers x13
   13 12 11 CASAL, ;

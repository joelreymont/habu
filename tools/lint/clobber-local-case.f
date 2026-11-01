\ clobber-local-case.f - fixture for a local and a host word whose spellings
\ differ only in case, read as text by clobber-lint-test.f (never loaded).

: REWRITE9 ( -- )
   9 0 MOVZ, ;

variable LCI-CALLEE

: EMIT-CI-CALLEE ( -- )
   LCI-CALLEE LABEL@ LBL,
   9 0 MOVZ,  RET, ;                \ clobbers x9

\ REWRITE9 rewrites x9 after the BL: no finding.
: EMIT-CI ( n -- )
   {: rewrite9 :}
   9 0 MOVZ,
   LCI-CALLEE LABEL@ BL,
   REWRITE9
   4 9 rewrite9 LDRB, ;

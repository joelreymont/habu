\ clobber-host-call.f - fixture for a call of clobber-host-def.f's host word
\ after a clobbering BL, read as text by clobber-lint-test.f (never loaded).

variable LHOST-CALLEE

: EMIT-HOST-CALLEE ( -- )
   LHOST-CALLEE LABEL@ LBL,
   9 0 MOVZ,  12 0 MOVZ,  RET, ;

\ REWRITE-ARG may rewrite any register, so neither later read is a finding.
: EMIT-HOST-CALLER ( -- )
   9 0 MOVZ,  12 0 MOVZ,
   LHOST-CALLEE LABEL@ BL,        \ clobbers x9 and x12
   9 REWRITE-ARG
   12 9 0 LDR,
   4 12 0 LDRB, ;

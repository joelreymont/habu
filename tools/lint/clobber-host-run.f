\ clobber-host-run.f - fixture for a host call ending a routine's operand
\ run, read as text by clobber-lint-test.f (never loaded).

: HOSTW ( n n -- ) {: rd:n x:n :}
   rd 21 x 8 * LDR, ;

\ Emits nothing, so it stays inside the operand run.
: OFF ( n -- n )
   8 * ;

variable LHOST-RUN

\ HOSTW ends the operand run, so the LDR's operands are 10 9 3 OFF and the
\ routine writes x10.
: EMIT-HOST-RUN ( -- )
   LHOST-RUN LABEL@ LBL,
   31 23 HOSTW  10 9 3 OFF LDR,  RET, ;

\ One finding: the routine clobbers x10.
: EMIT-HOST-RUN-CALLER ( -- )
   10 0 MOVZ,
   LHOST-RUN LABEL@ BL,           \ clobbers x10
   4 10 0 LDRB, ;

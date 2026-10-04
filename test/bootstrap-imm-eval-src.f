\ bootstrap-imm-eval-src.f - an immediate word's evaluate compiles into the open
\ definition. This is an input fixture, not a standalone test.

\ BIE-INC runs `1 +` through evaluate-closed while BIE-N or BIE-W is open, so
\ the buffer compiles into that definition. The stage0 compile loop calls an
\ immediate word with the code region read-execute (EMIT-COMPILE-CALL), and its
\ evaluate compiled the buffer's first word into that region: SIGBUS at
\ EMIT-CEMIT's `str w9, [x28]`, rc 134, after the armed marker; evaluate and
\ evaluate-closed enter the buffer through the same EVAL-ENTER. BIE-W's wide
\ local also sends its body through pass 2, which skips the immediate word and
\ recompiles the captured body, so the evaluated `1 +` has to be in it. Both
\ definitions then run and add the 1: 42 and 42.

SUMTYPE bie 1
  VARIANT some a ;VARIANT
;SUMTYPE

: BIE-INC ( -- ) s" 1 +" evaluate-closed ; immediate
s" BIE-INC" 0 parse-imm

s" BOOTSTRAP-IMM-EVAL-ARMED" type cr

: BIE-N ( n -- n ) BIE-INC ;
: BIE-W ( bie<n> n -- n )
   {: o:bie<n> k:n :}
   k BIE-INC ;

: BIE-RUN ( -- ) 41 BIE-N . 5 BIE:SOME 41 BIE-W . ;
BIE-RUN

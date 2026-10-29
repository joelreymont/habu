\ bootstrap-imm-eval-src.f - input fixture, not a standalone test: an immediate
\ word's evaluate compiles into the open definition.

\ BIE-INC runs `1 +` through evaluate-closed while BIE-N or BIE-W is open. The
\ stage0 compile loop calls an immediate word with the code region read-execute
\ (EMIT-COMPILE-CALL); an evaluate that compiles there dies with SIGBUS at
\ EMIT-CEMIT's `str w9, [x28]`, rc 134. evaluate and evaluate-closed share
\ EVAL-ENTER. BIE-W's wide local sends its body through pass 2, which skips the
\ immediate word and recompiles the captured body, so the evaluated `1 +` must
\ be in it. Both definitions print 42.

SUMTYPE bie<a>
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

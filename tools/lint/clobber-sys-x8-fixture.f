\ clobber-sys-x8-fixture.f - negative fixture for syscall scratch liveness.

variable LBAD
variable LCALL
variable LEXITGROUP
variable LRET

: EMIT-SYS-X8-BAD ( -- )
   LBAD @ LBL,
   8 9 0 ADDI,
   NR-CLOSE SYS,
   4 8 0 LDRB,
   RET, ;

: EMIT-LR-BAD ( -- )
   LCALL @ LBL,
   LRET @ BL,
   RET,
   LRET @ LBL,
   RET, ;

: EMIT-EXIT-GROUP-OK ( -- )
   LEXITGROUP @ LBL,
   16 9 0 ADDI,
   NR-EXIT-GROUP SYS,
   4 16 0 LDRB,
   RET, ;

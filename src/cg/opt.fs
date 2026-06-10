\ opt.fs — ICode peephole optimizer: rewrites IR records in place (kills
\ no-ops, dead stores, redundant branches). asm.fs emits nothing for killed
\ records, and labels keep their bindings, so optimization never breaks
\ branch resolution. Rules examine adjacent RECORDS (not emitted words).

require icode.fs

: IC-KILL ( i -- )  IC-ADDR IOP-DEAD swap ! ;
: NEXT-OP ( i -- op|-1 )  1+ dup #IC @ < if IC-OP else drop -1 then ;

\ MOV rd,rd — no-op
: OPT-SELF-MOV ( i -- )
   dup IC-OP IOP-MOV = if
      dup IC-A over IC-B = if dup IC-KILL then
   then drop ;

\ ADDI/SUBI rd,rd,#0 — no-op
: OPT-ARITH0 ( i -- )
   dup IC-OP dup IOP-ADDI = swap IOP-SUBI = or if
      dup IC-C 0= if dup IC-A over IC-B = if dup IC-KILL then then
   then drop ;

\ LIT rd,a directly followed by LIT rd,b — the first is a dead store
: OPT-DEAD-LIT ( i -- )
   dup IC-OP IOP-LIT = if
      dup NEXT-OP IOP-LIT = if
         dup IC-A over 1+ IC-A = if dup IC-KILL then
      then
   then drop ;

\ B to the label that is the very next record — fall-through
: OPT-B-NEXT ( i -- )
   dup IC-OP IOP-B = if
      dup NEXT-OP IOP-LABEL = if
         dup IC-A over 1+ IC-A = if dup IC-KILL then
      then
   then drop ;

create OPT-RULES
   ' OPT-SELF-MOV ,  ' OPT-ARITH0 ,  ' OPT-DEAD-LIT ,  ' OPT-B-NEXT ,
4 constant #OPT-RULES

: OPTIMIZE ( -- )
   #IC @ 0 ?do
      #OPT-RULES 0 ?do  j OPT-RULES i cells + @ execute  loop
   loop ;

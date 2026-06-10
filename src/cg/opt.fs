\ opt.fs — ICode peephole optimizer: rewrites IR records in place (kills
\ no-ops, dead stores, redundant branches). asm.fs emits nothing for killed
\ records, and labels keep their bindings, so optimization never breaks
\ branch resolution. Rules examine adjacent RECORDS (not emitted words).

require icode.fs

: IC-KILL ( i -- )  IC-ADDR IOP-DEAD swap ! ;
: NEXT-OP ( i -- op|-1 )  1+ dup #IC @ < if IC-OP else drop -1 then ;

\ field setters (rewrite a record in place — register allocation, peepholes)
: IC-OP! ( op i -- )  IC-ADDR ! ;
: IC-A!  ( v i -- )   IC-ADDR cell+ ! ;
: IC-B!  ( v i -- )   IC-ADDR 2 cells + ! ;

19 constant XDS-R     \ data-stack pointer register (matches templ.fs XDS)

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

\ TOS-in-register: a g-push reg immediately followed by a g-pop reg round-trips
\ a value through data-stack memory. The four records are
\   STR rA,[Xds,0]  ADDI Xds,Xds,8   SUBI Xds,Xds,8   LDR rB,[Xds,0]
\ Net Xds change is zero and [Xds] is above TOS (dead), so the pair is exactly
\ MOV rB,rA — or nothing when rA==rB. Collapsing it keeps the value in a register
\ across adjacent inlined primitives (a branch/BL between them breaks the match,
\ so it never crosses a control-flow edge). Returns ( rA rB f ).
: PUSHPOP? {: i -- rA rB f :}
   i 3 + #IC @ < 0= if 0 0 false exit then
   i      IC-OP IOP-STR  =
   i      IC-B XDS-R = and   i      IC-C 0= and
   i 1+   IC-OP IOP-ADDI = and   i 1+ IC-A XDS-R = and   i 1+ IC-B XDS-R = and   i 1+ IC-C 8 = and
   i 2 +  IC-OP IOP-SUBI = and   i 2 + IC-A XDS-R = and   i 2 + IC-B XDS-R = and   i 2 + IC-C 8 = and
   i 3 +  IC-OP IOP-LDR  = and   i 3 + IC-B XDS-R = and   i 3 + IC-C 0= and
   if  i IC-A   i 3 + IC-A   true  else  0 0 false  then ;

: OPT-PUSHPOP {: i -- :}
   i PUSHPOP? {: rA rB ok :}
   ok 0= if exit then
   rA rB = if
      i IC-KILL  i 1+ IC-KILL  i 2 + IC-KILL  i 3 + IC-KILL
   else
      rB i IC-A!   rA i IC-B!   IOP-MOV i IC-OP!
      i 1+ IC-KILL  i 2 + IC-KILL  i 3 + IC-KILL
   then ;

create OPT-RULES
   ' OPT-SELF-MOV ,  ' OPT-ARITH0 ,  ' OPT-DEAD-LIT ,  ' OPT-B-NEXT ,  ' OPT-PUSHPOP ,
5 constant #OPT-RULES

: OPTIMIZE ( -- )
   #IC @ 0 ?do
      #OPT-RULES 0 ?do  j OPT-RULES i cells + @ execute  loop
   loop ;

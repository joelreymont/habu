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
: IC-C!  ( v i -- )   IC-ADDR 3 cells + ! ;

19 constant XDS-R     \ data-stack pointer register (matches templ.fs XDS)

\ Pinned registers: loop-carried values (the index/limit and a register-resident
\ loop's carry homes) whose live ranges WRAP the back-edge. This linear optimizer
\ models straight-line liveness only, so it must never prove a pinned register
\ dead or coalesce a copy into it — that would drop a definition the back-edge
\ re-reads. walk.fs pins x27/x28 and the carry homes around each register loop.
create PINNED 32 cells allot
: PIN-RESET ( -- )   32 0 ?do  0 PINNED i cells + !  loop ;
: REG-PIN   ( r -- ) dup 0 32 within if  1 swap cells PINNED + !  else drop then ;
: REG-PINNED? ( r -- f )  dup 0 32 within if  cells PINNED + @ 0<>  else drop false then ;
PIN-RESET

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

\ --- block-local store-forwarding + dead-store elimination -------------------
\ The biggest cost in stack-machine code is round-tripping values through the
\ x19 data stack (STR/LDR). This pass tracks the symbolic x19 offset and, per
\ stack slot, the register that last stored it. A LDR from a known slot is
\ forwarded to a MOV from that register; a store overwritten before it is
\ observed (read from memory / crosses a boundary) is killed (DSE). Boundaries:
\ HARD (label/call/uncond-branch/ret) reset everything; SOFT (cond branch) keeps
\ registers for the fall-through but forces pending stores live; MEM (any non-x19
\ memory op) drops register forwarding (it may alias the stack) and forces stores
\ live. Correctness rests on clobbering a slot's forward-register whenever that
\ register is redefined — the differential native-exe suite is the backstop.
512 constant NSLOT   256 constant SLOT-BIAS
create SLOT-REG   NSLOT cells allot      \ reg holding each slot's value, -1 = unknown
create SLOT-STIDX NSLOT cells allot      \ IC index of the killable store, -1 = none/observed
variable SF-XOFF                          \ x19 offset (BYTES) from block origin
: SF-RESET ( -- )  0 SF-XOFF !
   NSLOT 0 ?do  -1 SLOT-REG i cells + !  -1 SLOT-STIDX i cells + !  loop ;
: SF-FWD-FLUSH ( -- )  NSLOT 0 ?do  -1 SLOT-REG   i cells + !  loop ;
: SF-ST-LIVE   ( -- )  NSLOT 0 ?do  -1 SLOT-STIDX i cells + !  loop ;  \ pending stores observed
: SF-HARD ( -- )  SF-FWD-FLUSH SF-ST-LIVE  0 SF-XOFF ! ;
: SF-SOFT ( -- )  SF-ST-LIVE ;
: SF-MEM  ( -- )  SF-FWD-FLUSH SF-ST-LIVE ;
: SF-CLOBBER ( r -- )                     \ a register was redefined: drop slots that forward it
   NSLOT 0 ?do  dup SLOT-REG i cells + @ = if -1 SLOT-REG i cells + ! then  loop  drop ;
: SF-SLOT ( ix -- s|-1 )                  \ slot index for a [x19,#off] access, -1 if out of range
   IC-C SF-XOFF @ +  8 /  SLOT-BIAS +  dup 0 NSLOT within 0= if drop -1 then ;

: SF-STORE {: ix -- :}
   ix SF-SLOT dup 0< if drop exit then  {: s :}
   SLOT-STIDX s cells + @ dup 0>= if IC-KILL else drop then   \ DSE the overwritten store
   ix    SLOT-STIDX s cells + !
   ix IC-A  SLOT-REG s cells + ! ;
: SF-LOAD {: ix -- :}
   ix SF-SLOT dup 0< if drop ix IC-A SF-CLOBBER exit then  {: s :}
   ix IC-A {: rB :}
   SLOT-REG s cells + @ dup 0>= if          {: fr :}    \ forward: LDR -> MOV rB, fr
      fr ix IC-B!  IOP-MOV ix IC-OP!  rB SF-CLOBBER
   else drop
      -1 SLOT-STIDX s cells + !             \ memory read observes the store (keep it)
      rB SF-CLOBBER  rB SLOT-REG s cells + !
   then ;
: SF-DEFINES? ( op -- f )                   \ op writes IC-A as a value register?
   dup IOP-CMP = over IOP-CMPI = or swap IOP-NOP = or 0= ;

\ op category for the boundary classes (STR/LDR/ADDI/SUBI handled inline)
0 constant CAT-OTHER  1 constant CAT-HARD  2 constant CAT-SOFT  3 constant CAT-MEM
create OPCAT #IOPS cells allot
: CAT! ( cat iop -- )  cells OPCAT + ! ;
: OPCAT-INIT ( -- )  #IOPS 0 ?do  CAT-OTHER i cells OPCAT + !  loop
   CAT-HARD IOP-LABEL CAT!  CAT-HARD IOP-BL CAT!  CAT-HARD IOP-BLR CAT!
   CAT-HARD IOP-BR CAT!  CAT-HARD IOP-RET CAT!  CAT-HARD IOP-B CAT!
   CAT-SOFT IOP-BCOND CAT!  CAT-SOFT IOP-CBZ CAT!  CAT-SOFT IOP-CBNZ CAT!
   CAT-MEM IOP-LDRB CAT!  CAT-MEM IOP-STRB CAT!  CAT-MEM IOP-LDRW CAT!  CAT-MEM IOP-STRW CAT!
   CAT-MEM IOP-LDRPO CAT!  CAT-MEM IOP-STRPR CAT!  CAT-MEM IOP-LDPPO CAT!  CAT-MEM IOP-STPPR CAT!
   CAT-MEM IOP-SVC CAT!  CAT-MEM IOP-ICIV CAT!  CAT-MEM IOP-DCCV CAT!
   CAT-MEM IOP-DSB CAT!  CAT-MEM IOP-ISB CAT!
   CAT-MEM IOP-BYTES CAT!  CAT-MEM IOP-DCQ CAT!  CAT-MEM IOP-DLBL CAT! ;
OPCAT-INIT

: STORE-FWD ( -- )
   SF-RESET
   #IC @ 0 ?do
      i IC-OP {: op :}
      op IOP-DEAD = if  \ skip
      else op IOP-ADDI = i IC-A 19 = and i IC-B 19 = and if  i IC-C SF-XOFF +!
      else op IOP-SUBI = i IC-A 19 = and i IC-B 19 = and if  i IC-C negate SF-XOFF +!
      else op IOP-STR = i IC-B 19 = and if  i SF-STORE
      else op IOP-LDR = i IC-B 19 = and if  i SF-LOAD
      else
         op cells OPCAT + @ {: cat :}
         cat CAT-HARD = if SF-HARD
         else cat CAT-SOFT = if SF-SOFT
         else cat CAT-MEM  = if SF-MEM
         else  op SF-DEFINES? if i IC-A SF-CLOBBER then    \ OTHER value op: clobber its dest
         then then then
      then then then then then
   loop ;

\ --- x19 churn cancellation -------------------------------------------------
\ Once store-forwarding has removed the stack STR/LDR, the ADDI/SUBI x19 that
\ bracketed them are often adjacent (skipping dead records) and inverse (+k then
\ -k). Such a pair is a pointless pointer excursion with nothing between it and no
\ later dependence on the intermediate offset — kill both. Iterate to a fixpoint.
: NEXT-LIVE ( i -- j|-1 )
   begin 1+ dup #IC @ < while  dup IC-OP IOP-DEAD <> if exit then  repeat  drop -1 ;
: X19-DELTA ( i -- delta )                 \ signed x19 change; 0 if not an x19 add/sub
   dup IC-A 19 = over IC-B 19 = and 0= if drop 0 exit then
   dup IC-OP IOP-ADDI = if IC-C exit then
   dup IC-OP IOP-SUBI = if IC-C negate exit then  drop 0 ;
variable X19-CHG
: X19-CANCEL-PASS ( -- changed? )
   X19-CHG off
   #IC @ 0 ?do
      i X19-DELTA {: d :}  d if
         i NEXT-LIVE {: j :}  j 0>= if
            j X19-DELTA {: e :}  e 0<>  d e + 0=  and if
               i IC-KILL  j IC-KILL  X19-CHG on
            then
         then
      then
   loop  X19-CHG @ ;
: X19-CANCEL ( -- )  begin X19-CANCEL-PASS 0= until ;

\ --- shifted-operand fusion -------------------------------------------------
\ With values register-resident, an in-place immediate shift feeding an ALU op
\   LSLI/LSRI rd,rd,#k ;  <ALU> rx,ry,rd      (rd dead after the ALU)
\ fuses to the ARM shifted-register form  <ALU> rx,ry,rd,LSL/LSR #k  (one instr,
\ matching LLVM). The shift is killed; the ALU keeps rd as rm (now holding the
\ PRE-shift value) and gets the shift in IC-D.
: ALU-SHIFTABLE? ( op -- f )                  \ ADD/SUB/AND/ORR/EOR take a shifted rm
   dup IOP-ADD = over IOP-SUB = or over IOP-AND = or over IOP-ORR = or swap IOP-EOR = or ;
: SHIFT-AT ( shtype amt j -- )  >r  swap 6 lshift or  r> IC-ADDR 4 cells + ! ;
: REG-DEAD-AFTER? {: j rd -- f :}             \ rd not read before written/boundary after j
   rd REG-PINNED? if false exit then          \ loop-carried: never provably dead (back-edge)
   j 1+ {: k :}
   begin k #IC @ < while
      k IC-OP {: op :}
      op IOP-DEAD <> if
         op cells OPCAT + @ dup CAT-HARD = swap dup CAT-SOFT = swap CAT-MEM = or or if true exit then
         k IC-B rd =  k IC-C rd = or  op IOP-STR = k IC-A rd = and or if false exit then
         op SF-DEFINES? k IC-A rd = and if true exit then
      then
   1 +to k repeat  true ;
: OPT-SHIFT-FUSE {: i -- :}
   i IC-OP {: op :}
   op IOP-LSLI = op IOP-LSRI = or 0= if exit then
   i IC-A i IC-B <> if exit then              \ in-place shift only (rd==rn)
   i IC-A {: rd :}
   i NEXT-LIVE {: j :}  j 0< if exit then
   j IC-OP ALU-SHIFTABLE? 0= if exit then
   j IC-C rd <> if exit then                  \ ALU's rm must be the shifted reg
   j IC-A rd =  j IC-B rd = or if exit then   \ rd must be ONLY the rm
   j rd REG-DEAD-AFTER? 0= if exit then
   op IOP-LSRI = if SH-LSR else SH-LSL then  i IC-C  j SHIFT-AT   \ fuse shift into the ALU
   i IC-KILL ;
: SHIFT-FUSE ( -- )  #IC @ 0 ?do  i OPT-SHIFT-FUSE  loop ;

\ --- copy propagation / MOV coalescing ---------------------------------------
\ `MOV rd,rs ; … <op …,rd,…>` where rs is unchanged up to the use and rd is dead
\ after it → rewrite the use rd→rs and kill the MOV. Turns the DUP-copy of an
\ in-place self-op (`MOV r2,r ; EOR r,r,r2,LSL#k`) into `EOR r,r,r,LSL#k` (LLVM).
\ Handles the first reader only (enough for the self-op pattern); safe partial.
: OPT-COPY-PROP {: i -- :}
   i IC-OP IOP-MOV <> if exit then
   i IC-A {: rd :}  i IC-B {: rs :}  rd rs = if exit then
   rd REG-PINNED? if exit then                \ never coalesce a copy into a loop-carried reg
   i 1+ {: k :}
   begin  k #IC @ < while
      k IC-OP {: op :}
      op IOP-DEAD <> if
         op cells OPCAT + @ dup CAT-HARD = swap dup CAT-SOFT = swap CAT-MEM = or or if exit then
         k IC-B rd =  k IC-C rd = or  op IOP-STR = k IC-A rd = and or if   \ READER (handle first:
            k IC-B rd = if rs k IC-B! then                    \  reads use the OLD value, even if
            k IC-C rd = if rs k IC-C! then                    \  this op also rewrites rs)
            op IOP-STR = k IC-A rd = and if rs k IC-A! then
            k rd REG-DEAD-AFTER? if i IC-KILL then  exit
         then
         op SF-DEFINES? k IC-A rs = and if exit then          \ rs redefined before a read: copy stale
         op SF-DEFINES? k IC-A rd = and if exit then          \ rd redefined before any read
      then
   1 +to k repeat ;
: COPY-PROP ( -- )  #IC @ 0 ?do  i OPT-COPY-PROP  loop ;

: OPTIMIZE ( -- )
   #IC @ 0 ?do
      #OPT-RULES 0 ?do  j OPT-RULES i cells + @ execute  loop
   loop
   SHIFT-FUSE                          \ fuse immediate shift into the next ALU op
   COPY-PROP                           \ coalesce DUP-copy MOVs into the ALU operand
   STORE-FWD
   #IC @ 0 ?do  i OPT-SELF-MOV  loop   \ clean MOV rd,rd from forwarding
   X19-CANCEL ;                        \ drop the orphaned stack-pointer churn

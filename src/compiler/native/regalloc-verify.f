\ regalloc-verify.f - decide whether a finished register assignment is true of the
\ module it claims to be about.
\
\ docs/compiler-ir-design.md section 7.9 ("validate the final assignment
\ independently") and section 11.3, which lists the register-allocation validator
\ among the checks the JIT path runs before it makes anything executable. The
\ allocator in src/compiler/native/regalloc.f publishes claims; this file is what
\ turns an accepted claim into an answer a later stage may emit code from, and it
\ is the only place that answers at all.
\
\ WHAT INDEPENDENT MEANS HERE. Every fact this file decides on is re-derived from
\ the frozen module, never read out of the allocator's working tables. It walks
\ the block itself to learn where each value is written and where it is last
\ read, and it compares that against the interval the allocator recorded - so an
\ allocator that mis-measured a live range is caught by the disagreement, not
\ excused by it. The one thing it does take from the allocator is which module
\ and which routine contract the allocation was made for, because those are what
\ it is checking the assignment against; both are checked to be the ones it was
\ handed. That contract is also where the routine's fixed registers are declared
\ - which register each argument arrives in and each returned value leaves in -
\ and every declared position is compared against the assignment itself, so an
\ allocator that pre-coloured nothing, or pre-coloured the wrong value, or
\ planned a move it never made, disagrees here rather than being taken at its
\ word. Which register fields an instruction form shares is likewise re-derived:
\ the ties come out of the module's own schema table, so this file and the
\ allocator agree because they read one declaration, not because one told the
\ other. The dialect's own identities - which type is a register, which is a
\ memory token, which attribute key carries a slot - come from the dialect
\ itself, asked while the module was still being built, for the same reason: an
\ identity taken from the allocator would be the allocator telling the checker
\ what to check.
\
\ THE FRAME, AND WHAT IS DECIDABLE ABOUT IT. A value that lost its register lives
\ in a slot of the routine's frame, and four facts about that are this file's:
\   - every slot a frame access names is one the routine can actually address.
\     A64EFF:CHECK-SLOT is the rule and it is called with the routine's own
\     contract, so a slot outside the declared frame, an unaligned one, or one
\     past the reach of the offset field is refused under A64EFF's name.
\   - the frame the module reserves is the frame the contract declares, it is
\     reserved by the block's first operation and released by the one in front of
\     the terminator, and no other operation touches the stack pointer. A module
\     that stores into a frame it never took is the failure this catches.
\   - no two values share a slot: a slot is written once. That is stronger than
\     "no two values live at once share a slot" and it is what is decidable from
\     the module alone - a module records which slot a store writes, not which
\     value a later load was meant for, so a slot handed to a second value would
\     be indistinguishable from a slot legitimately reused if reuse were allowed.
\     The allocator never reuses a slot, so this check is exact for it; an
\     allocator that starts reusing slots reddens here, which is the right way to
\     find out that this rule has to be generalised with it.
\   - every load reads a slot something stored to first, so no reload invents a
\     value out of whatever the frame happened to hold.
\ What is NOT decided here is that the loaded value is the value the program
\ wanted: that is a statement about the module this one was rewritten FROM, and
\ this file is handed one module. The owner of that comparison is the lowering
\ pass (dot habu-prove-the-spill-0294e0e8).
\
\ TWO VALUE CLASSES. A general register and a memory token, told apart by type.
\ A token lives in no register, so it is covered and measured like every other
\ value and takes part in no register rule: it is not checked against the pool,
\ it never clashes with anything, and asking for its register is refused rather
\ than answered with something that looks like one.
\
\ THE INTERFERENCE RULE, IN FULL. Two different values may share a register
\ exactly when they are never live at the same instant. Order them by where they
\ are written:
\   - two values written at the same position are always live together. Only
\     block arguments can be, and they all arrive at once, so two arguments never
\     share a register - not even when one of them is never read, because the
\     caller still has to have put both somewhere.
\   - otherwise the earlier value is dead by the time the later one is written
\     exactly when its last read is at or before that position. An operation
\     reads its operands and then writes its results, so a value read for the
\     last time by operation i and a value written by operation i do not clash;
\     that is what lets a chain run in one register, and it is what makes the
\     move-wide overwrite's tied register legal rather than a special case.
\ A value that is never read is live only where it is written, which still costs
\ a register there, because the instruction writes one.
\
\ WHAT THIS FILE DOES NOT DECIDE. Whether the module is a well-formed A64IR
\ module at all is IR-VERIFY's and the dialect's, and whether the operations
\ compute what the source said is the selector's. This file assumes a verified
\ module and asks one question about it: is this assignment of registers to its
\ values legal, complete, and consistent with its own definitions and uses.

require lib/prelude.f
require lib/errors.f
require src/compiler/a64-effect.f
require src/compiler/ir/id.f
require src/compiler/ir/context.f
require src/compiler/ir/schema.f
require src/compiler/ir/op.f
require src/compiler/ir/fun.f
require src/compiler/ir/build.f
require src/compiler/native/a64ir.f
require src/compiler/native/frozen.f
require src/compiler/native/regalloc.f

package A64RAV
using NFROZEN
private

\ The position of a block argument: before every operation of the block.
-1 constant ENTRY

0 constant ST-NONE
1 constant ST-ACCEPTED

0 constant BOUND-NO
1 constant BOUND-YES

\ The two value classes this dialect has.
0 constant C-GPR
1 constant C-TOKEN

\ This operation names no slot.
-1 constant NOSLOT

\ Slots one block may use: one per value at worst.
VMAX constant SLOTS-MAX

here CELL 1- and CELL swap - CELL 1- and allot
variable ST
ST-NONE ST !
variable BND-MODE
BOUND-NO BND-MODE !
variable A-GEN
0 A-GEN !
variable N-VALS
0 N-VALS !

1 TYPED-BUFFER BND-MOD IR-ID:ir-module-id
1 TYPED-BUFFER BND-GPR IR-ID:ir-type-id
1 TYPED-BUFFER BND-MEM IR-ID:ir-type-id
1 TYPED-BUFFER BND-SLOT IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-FRAME IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-DSLOT IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-DBYTES IR-ID:ir-symbol-id

create D-AT VMAX cells allot         \ where the module says each value is written
create L-AT VMAX cells allot         \ where the module says each value is last read
create S-AT VMAX cells allot         \ whether the block defines this value at all
create C-AT VMAX cells allot         \ which class the module gives each value
create W-AT SLOTS-MAX cells allot    \ where each slot was written, or -1

: DEF-AT ( n -- n )                  cells D-AT + @ ;
: LAST-AT ( n -- n )                 cells L-AT + @ ;
: SEEN-AT ( n -- n )                 cells S-AT + @ ;
: CLS-AT ( n -- n )                  cells C-AT + @ ;

: DEF! ( n n -- )                    {: v:n k:n :} v k cells D-AT + ! ;
: LAST! ( n n -- )                   {: v:n k:n :} v k cells L-AT + ! ;
: SEEN! ( n n -- )                   {: v:n k:n :} v k cells S-AT + ! ;
: CLS! ( n n -- )                    {: v:n k:n :} v k cells C-AT + ! ;

: TABLES-CLEAR ( -- )
   VMAX 0 ?do
      0 i SEEN!
      ENTRY i DEF!
      ENTRY i LAST!
      C-GPR i CLS!
   loop
   SLOTS-MAX 0 ?do -1 i cells W-AT + ! loop ;

\ ---- reading the frozen module -----------------------------------------------
: SLOT ( IR-ID:ir-value-id -- n )
   IR-ID:VALUE-LOCAL
   dup 0 < over VMAX >= or if E-A64RAV-COVER throw then ;

\ ---- what an operation says about the memory it reaches -----------------------
\ Nothing below asks which opcode an operation is. Every form that touches memory
\ carries its offset or its size under one of the dialect's four keys, and which
\ key it is says which region the access is in: a frame slot and a frame size are
\ counted from the machine stack pointer, a data-stack slot and a data-stack
\ adjustment from the engine's data-stack pointer. Reading the key rather than
\ the opcode is what keeps a frame rule from ever being applied to a data-stack
\ access, and it is why a form added to the dialect is judged by what it says
\ about itself.
: ATTR-INT ( IR-ID:ir-op-id IR-ID:ir-symbol-id -- n )
   {: id:IR-ID:ir-op-id want:IR-ID:ir-symbol-id :}
   NOSLOT
   id ATTRS-OF 0 ?do
      id i ATTR-KEY-AT want SAME-SYM? if
         drop
         id i ATTR-INT-AT
         leave
      then
   loop ;

: SLOT-OF ( IR-ID:ir-op-id -- n )    0 BND-SLOT @ ATTR-INT ;
: FRAME-OF ( IR-ID:ir-op-id -- n )   0 BND-FRAME @ ATTR-INT ;
: DSLOT-OF ( IR-ID:ir-op-id -- n )   0 BND-DSLOT @ ATTR-INT ;
: DBYTES-OF ( IR-ID:ir-op-id -- n )  0 BND-DBYTES @ ATTR-INT ;

\ Which region an operation reaches, read off the keys the dialect declares for
\ each family rather than off an opcode name. A frame access counts its offset
\ from the machine stack pointer and a data-stack access counts its offset from
\ the engine's data-stack pointer, so an operation that carries a frame key is in
\ the frame and one that carries a data-stack key is in the caller's stack, and
\ no check about one can ever be applied to the other.
: FRAME-TOUCH? ( IR-ID:ir-op-id -- bool )
   {: id:IR-ID:ir-op-id :}
   id FRAME-OF NOSLOT <>  id SLOT-OF NOSLOT <>  or ;

: DSTACK-TOUCH? ( IR-ID:ir-op-id -- bool )
   {: id:IR-ID:ir-op-id :}
   id DBYTES-OF NOSLOT <>  id DSLOT-OF NOSLOT <>  or ;

\ Does this operation write a value into a slot, or read one out of one?
: STORES? ( IR-ID:ir-op-id -- bool )
   {: id:IR-ID:ir-op-id :}
   V-SCHR VW id OPCODE-AT IR-SCHEMA:FEFFECT@
   IR--SCHEMA-EFFECT:WRITE IR--SCHEMA-EFFECT:EQ ;

\ The straight-line subset, re-derived rather than taken on trust.
: BLOCK-OF ( -- IR-ID:ir-block-id )
   FUN-COUNT 1 <> if E-A64RAV-SHAPE throw then
   MKEY 0 IR-ID:PACK-FUN {: f:IR-ID:ir-fun-id :}
   f BLOCK-COUNT 1 <> if E-A64RAV-SHAPE throw then
   f 0 BLOCK-AT ;

\ ---- what the module says about each value -----------------------------------
: NOTE-DEF ( IR-ID:ir-value-id n -- )
   {: id:IR-ID:ir-value-id pos:n :}
   id SLOT {: k:n :}
   k SEEN-AT 0<> if E-A64RAV-COVER throw then
   1 k SEEN!
   pos k DEF!
   pos k LAST! ;

: NOTE-USE ( IR-ID:ir-value-id n -- )
   {: id:IR-ID:ir-value-id pos:n :}
   id SLOT {: k:n :}
   k SEEN-AT 0= if E-A64RAV-COVER throw then
   pos k LAST! ;

: DEFS-OF-OP ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id pos:n :}
   id RESULTS-OF {: n:n :}
   n 0 ?do id i RESULT-AT pos NOTE-DEF loop ;

: USES-OF-OP ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id pos:n :}
   id OPERANDS-OF {: n:n :}
   n 0 ?do id i OPERAND-AT pos NOTE-USE loop ;

: MEASURE-ARGS ( IR-ID:ir-block-id -- )
   {: bk:IR-ID:ir-block-id :}
   bk ARG-COUNT {: n:n :}
   n 0 ?do
      bk i ARG-AT ENTRY NOTE-DEF
   loop ;

: MEASURE ( IR-ID:ir-block-id -- )
   {: bk:IR-ID:ir-block-id :}
   TABLES-CLEAR
   bk MEASURE-ARGS
   bk OP-COUNT {: n:n :}
   n 0 ?do
      bk i OP-AT {: id:IR-ID:ir-op-id :}
      id i USES-OF-OP
      id i DEFS-OF-OP
   loop ;

\ ---- the checks --------------------------------------------------------------
\ Every value of the module is a value of this block, the allocation covers
\ exactly those values, and the interval it recorded for each one is the interval
\ the module gives.
: COVER-CK ( -- )
   V-VALR VW IR-OP:FVALUES {: n:n :}
   n A64RA:VALUES <> if E-A64RAV-COVER throw then
   n VMAX > if E-A64RAV-COVER throw then
   n 0 ?do i SEEN-AT 0= if E-A64RAV-COVER throw then loop
   n N-VALS ! ;

: INTERVAL-CK ( -- )
   N-VALS @ 0 ?do
      i DEF-AT i A64RA:DEF@ <> if E-A64RAV-INTERVAL throw then
      i LAST-AT i A64RA:LAST@ <> if E-A64RAV-INTERVAL throw then
   loop ;

\ Two register classes: every value of this dialect is a general register or the
\ memory token the frame forms thread, and the class is decided by the type the
\ module gives the value against the two the dialect answered. A value of any
\ third type has been given a register that cannot hold it.
: CLASS-CK ( -- )
   N-VALS @ 0 ?do
      MKEY i IR-ID:PACK-VALUE VALUE-TYPE-AT
      {: t:IR-ID:ir-type-id :}
      t 0 BND-GPR @ SAME-TYPE? if C-GPR i CLS! then
      t 0 BND-MEM @ SAME-TYPE? if C-TOKEN i CLS! then
      t 0 BND-GPR @ SAME-TYPE? t 0 BND-MEM @ SAME-TYPE? or
      0= if E-A64RAV-CLASS throw then
   loop ;

: GPR? ( n -- bool )
   CLS-AT C-GPR = ;

\ Every assigned register is one the routine's contract says it may destroy. The
\ contract cannot name x18, x30 or register 31 at all - A64EFF refuses them in
\ any general-register set - so a reserved register fails this check for the same
\ reason an unrelated callee-saved register does. A memory token holds no
\ register, and one that was given a real register is refused as loudly as a
\ register outside the pool: the emitter would then be reading a machine object
\ out of something that is only an ordering. That second refusal is fail-closed
\ rather than reachable and says so: a claim is the allocator's own, no module
\ built by hand can forge one, and the allocator gives a token none. It is still
\ written, because a check that only looks at the registers it expects to see is
\ not a check, and it is not claimed to be tested - only mutating the allocator
\ reaches it.
: REGISTER-CK ( -- )
   A64RA:POOL A64EFF:GPRS-N {: pool:n :}
   N-VALS @ 0 ?do
      i A64RA:CLAIM@ {: r:n :}
      i GPR? 0= if
         r 0 >= r A64EFF:FILE-SIZE < and if E-A64RAV-CLASS throw then
      else
         r 0 < r A64EFF:FILE-SIZE >= or if E-A64RAV-REGISTER throw then
         pool 1 r lshift and 0= if E-A64RAV-REGISTER throw then
      then
   loop ;

\ Are these two values ever live at the same instant? See the header: values
\ written at the same position always are, and otherwise the earlier one has to
\ be read for the last time at or before the later one is written.
: CLASH? ( n n -- bool )
   {: a:n b:n :}
   a DEF-AT b DEF-AT = if true exit then
   a DEF-AT b DEF-AT < if
      a LAST-AT b DEF-AT > exit
   then
   b LAST-AT a DEF-AT > ;

: OVERLAP-CK ( -- )
   N-VALS @ {: n:n :}
   n 0 ?do
      n i 1+ ?do
         j GPR? i GPR? and  j i CLASH? and if
            j A64RA:CLAIM@ i A64RA:CLAIM@ = if E-A64RAV-OVERLAP throw then
         then
      loop
   loop ;

\ A form that names one register field twice - the move-wide overwrite keeps the
\ bits of its destination it does not write - declares that tie in its own
\ operation schema. Every declared tie is checked here on its own terms, read out
\ of the module's schema table rather than out of anything the allocator kept: an
\ assignment that gives a tied result and its operand two registers describes an
\ instruction the machine cannot execute.
: OP-TIE-CK ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id i:n :}
   V-SCHP VW V-SCHR VW  id OPCODE-AT  i IR-SCHEMA:FTIE-RESULT@ {: rs:n :}
   V-SCHP VW V-SCHR VW  id OPCODE-AT  i IR-SCHEMA:FTIE-OPERAND@ {: op:n :}
   id op OPERAND-AT SLOT A64RA:CLAIM@
   id rs RESULT-AT SLOT A64RA:CLAIM@
   <> if E-A64RAV-TIE throw then ;

: TIE-CK ( IR-ID:ir-block-id -- )
   {: bk:IR-ID:ir-block-id :}
   bk OP-COUNT {: n:n :}
   n 0 ?do
      bk i OP-AT {: id:IR-ID:ir-op-id :}
      V-SCHR VW id OPCODE-AT IR-SCHEMA:FTIES 0 ?do
         id i OP-TIE-CK
      loop
   loop ;

\ ---- the routine's declared registers ------------------------------------------
\ The contract this check was handed says which register each argument arrives in
\ and which each returned value leaves in. Both are decided here from that list
\ and the assignment alone: nothing is read out of the allocator's own view of
\ its constraints, so an allocator that pre-coloured the wrong value, pre-coloured
\ nothing at all, or planned a move it never made is caught by the disagreement.
\ A contract naming more positions than the module has arguments or returned
\ values is not this module's contract and is refused before any position is
\ compared.
\ How many positions of one side are register places. Re-derived here rather than
\ read out of the allocator, for the same reason every other fact is: a side that
\ mixes register places with data-stack places has no pairing rule anywhere in
\ this chain, and a side declared entirely in data-stack slots constrains no
\ register at all - the selector turned every one of those places into an
\ operation, and DSTACK-CK below is what judges those.
: REG-POSITIONS ( A64EFF:placeseq -- n )
   {: s:A64EFF:placeseq :}
   s A64EFF:SEQ-LEN {: len:n :}
   s A64EFF:SEQ-SLOTS {: sl:n :}
   sl 0= if len exit then
   sl len <> if E-A64RAV-PLACE throw then
   0 ;

: ARG-CK ( IR-ID:ir-block-id A64EFF:placeseq -- )
   {: bk:IR-ID:ir-block-id args:A64EFF:placeseq :}
   args A64EFF:SEQ-SLOTS 0<> bk ARG-COUNT 0<> and
   if E-A64RAV-PLACE throw then
   args REG-POSITIONS {: n:n :}
   bk ARG-COUNT n < if E-A64RAV-FIXED throw then
   n 0 ?do
      bk i ARG-AT SLOT A64RA:CLAIM@
      args i A64EFF:SEQ-REG@ <> if E-A64RAV-FIXED throw then
   loop ;

\ Where control leaves, the register holding returned value j is the one declared
\ for position j. The terminator is read off the block's own row, and its
\ operands are the values the routine returns, so this is the assignment's answer
\ at exactly the instant the convention talks about.
: OUT-CK ( IR-ID:ir-block-id A64EFF:placeseq -- )
   {: bk:IR-ID:ir-block-id outs:A64EFF:placeseq :}
   V-BLKR VW V-OPR VW MKEY bk IR-FUN:FTERMINATOR@ {: id:IR-ID:ir-op-id :}
   outs A64EFF:SEQ-SLOTS 0<> id OPERANDS-OF 0<> and
   if E-A64RAV-PLACE throw then
   outs REG-POSITIONS {: n:n :}
   id OPERANDS-OF n < if E-A64RAV-FIXED throw then
   n 0 ?do
      id i OPERAND-AT SLOT A64RA:CLAIM@
      outs i A64EFF:SEQ-REG@ <> if E-A64RAV-FIXED throw then
   loop ;

\ ---- the frame -----------------------------------------------------------------
\ A block that touches the frame at all takes it with its first operation and
\ gives it back with the one in front of its terminator, and both name the frame
\ the routine's contract declares. Any other operation that moves the stack
\ pointer - one that declares a memory effect and names no slot - is a second
\ frame inside the first, and there is no rule for that here.
: FRAMES? ( IR-ID:ir-block-id -- bool )
   {: bk:IR-ID:ir-block-id :}
   false
   bk OP-COUNT 0 ?do
      bk i OP-AT FRAME-TOUCH? if drop true leave then
   loop ;

: FRAME-AT? ( IR-ID:ir-block-id n n -- )
   {: bk:IR-ID:ir-block-id at:n want:n :}
   bk at OP-AT {: id:IR-ID:ir-op-id :}
   id SLOT-OF NOSLOT <> if E-A64RAV-FRAME throw then
   id FRAME-OF want <> if E-A64RAV-FRAME throw then ;

: FRAME-CK ( IR-ID:ir-block-id n -- )
   {: bk:IR-ID:ir-block-id want:n :}
   bk FRAMES? 0= if exit then
   bk OP-COUNT {: n:n :}
   n 3 < if E-A64RAV-FRAME throw then
   bk 0 want FRAME-AT?
   bk n 2 - want FRAME-AT?
   n 0 ?do
      i 0 <> i n 2 - <> and if
         bk i OP-AT FRAME-OF NOSLOT <> if E-A64RAV-FRAME throw then
      then
   loop ;

\ ---- the data stack ----------------------------------------------------------
\ A routine whose convention names data-stack slots reaches the caller's stack
\ with a fixed sequence, and this is where the module is measured against the
\ declaration that sequence came from. Four facts are decidable from one module
\ and one contract, and all four are checked: the pointer is moved down over
\ exactly the arguments the contract declares and up over exactly the results;
\ each load names the slot the argument place at its position names, in that
\ order; each store names the slot the result place at its position names, in
\ that order; and nothing else in the block touches the data stack.
\
\ WHAT IS NOT DECIDABLE HERE, and its owner. Whether the value a store publishes
\ is the value the program computed for that result is a statement about the
\ module the selector read, and this file is handed one module - the same gap the
\ spill lowering has (dot habu-prove-the-spill-0294e0e8), with the same owner
\ (dot habu-prove-the-data-stack-8f0d3f65).
\
\ THE TWO REGIONS DO NOT MEET YET. A routine that both reaches the caller's data
\ stack and reserves a frame would need one operation at position zero to be both
\ the frame reserve and the data-stack take, and there is no rule here for
\ nesting them. It is refused by name rather than checked half-way, and no pass
\ in the chain builds one (dot habu-spill-a-data-stack-6c1b73f2).
: DTAKE-AT? ( IR-ID:ir-block-id n n -- )
   {: bk:IR-ID:ir-block-id at:n want:n :}
   bk at OP-AT {: id:IR-ID:ir-op-id :}
   id DSLOT-OF NOSLOT <> if E-A64RAV-DSTACK throw then
   id DBYTES-OF want <> if E-A64RAV-DSTACK throw then ;

: DSLOT-AT? ( IR-ID:ir-block-id n n -- )
   {: bk:IR-ID:ir-block-id at:n want:n :}
   bk at OP-AT {: id:IR-ID:ir-op-id :}
   id DBYTES-OF NOSLOT <> if E-A64RAV-DSTACK throw then
   id DSLOT-OF want <> if E-A64RAV-DSTACK throw then ;

\ Every position of the block that is allowed to touch the data stack: the take
\ at the top, the loads after it, the stores in front of the publish, and the
\ publish itself.
: DSTACK-POS? ( n n n n -- bool )
   {: n:n a:n r:n at:n :}
   at 0 = if true exit then
   at a <= if true exit then
   at n 2 - = if true exit then
   at n 2 - r - >= at n 2 - < and ;

: DSTACK-CK ( IR-ID:ir-block-id A64EFF:placeseq A64EFF:placeseq -- )
   {: bk:IR-ID:ir-block-id args:A64EFF:placeseq outs:A64EFF:placeseq :}
   args A64EFF:SEQ-SLOTS {: a:n :}
   outs A64EFF:SEQ-SLOTS {: r:n :}
   bk OP-COUNT {: n:n :}
   a 0= r 0= and if
      n 0 ?do bk i OP-AT DSTACK-TOUCH? if E-A64RAV-DSTACK throw then loop
      exit
   then
   bk FRAMES? if E-A64RAV-DSTACK throw then
   n a r + 3 + < if E-A64RAV-DSTACK throw then
   bk 0  a A64IR:SLOT-WIDTH *  DTAKE-AT?
   a 0 ?do
      bk i 1+  args i A64EFF:SEQ-SLOT@ A64IR:SLOT-WIDTH *  DSLOT-AT?
   loop
   bk n 2 -  r A64IR:SLOT-WIDTH *  DTAKE-AT?
   r 0 ?do
      bk  n 2 - r - i +  outs i A64EFF:SEQ-SLOT@ A64IR:SLOT-WIDTH *  DSLOT-AT?
   loop
   n 0 ?do
      n a r i DSTACK-POS? 0= if
         bk i OP-AT DSTACK-TOUCH? if E-A64RAV-DSTACK throw then
      then
   loop ;

\ Every slot the module names is written before it is read, and no slot is
\ written twice. See the header for why the second rule is the decidable form of
\ "no two values share a slot".
: FLOW-CK ( IR-ID:ir-block-id -- )
   {: bk:IR-ID:ir-block-id :}
   bk OP-COUNT 0 ?do
      bk i OP-AT {: id:IR-ID:ir-op-id :}
      id SLOT-OF {: off:n :}
      off NOSLOT <> if
         off A64IR:SLOT-WIDTH / {: s:n :}
         s 0 < s SLOTS-MAX >= or if E-A64RAV-SLOT throw then
         id STORES? if
            s cells W-AT + @ 0 >= if E-A64RAV-SHARE throw then
            i s cells W-AT + !
         else
            s cells W-AT + @ 0 < if E-A64RAV-RELOAD throw then
         then
      then
   loop ;

\ Every slot a frame access names, measured against the routine that has to
\ address it. The rule is A64EFF's, so a slot outside the declared frame, an
\ unaligned one, or one past the reach of the offset field is refused under
\ A64EFF's own name; the contract is rebuilt per access because a value of more
\ than one cell cannot be held in a local.
: SLOT-CK ( IR-ID:ir-block-id A64EFF:routine -- )
   A64EFF:VALIDATE A64EFF-ROUTINE:UNMAKE
   {: gi:A64EFF:placeseq gr:A64EFF:placeseq gc:A64EFF:gprs
      fi:A64EFF:fprs fr:A64EFF:fprs fc:A64EFF:fprs
      z:A64EFF:nzcv l:A64EFF:link ct:A64EFF:control t:A64EFF:traits
      size:n delta:n :}
   {: bk:IR-ID:ir-block-id :}
   bk OP-COUNT 0 ?do
      bk i OP-AT SLOT-OF {: off:n :}
      off NOSLOT <> if
         off A64IR:SLOT-WIDTH
         gi gr gc fi fr fc z l ct t size delta A64EFF-ROUTINE:MAKE
         A64EFF:CHECK-SLOT
      then
   loop ;

\ ---- what the acceptance is bound to -----------------------------------------
: STATE-CK ( -- )
   A64RA:SEALED? 0= if E-A64RAV-STATE throw then ;

: MODULE-CK ( IR-BUILD:module -- )
   IR-BUILD:FMODULE A64RA:MODULE@ IR-ID:MODULE-SAME?
   0= if E-A64RAV-MODULE throw then ;

\ The allocation depends on one fact of the contract: which general registers the
\ routine may write, which is what it destroys together with what it returns a
\ value in. A contract that names a different set is a different allocation
\ problem, and this one is not an answer to it.
: CONTRACT-CK ( A64EFF:gprs -- )
   A64RA:POOL A64EFF-GPRS:EQ 0= if E-A64RAV-CONTRACT throw then ;

\ An accepted answer is about one sealed walk. A later walk raises the
\ allocator's generation, so the acceptance stops answering rather than answering
\ about a walk nobody checked.
: FRESH-CK ( -- )
   ST @ ST-ACCEPTED <> if E-A64RAV-STATE throw then
   STATE-CK
   A64RA:GEN A-GEN @ <> if E-A64RAV-STATE throw then ;

\ WHY THIS BINDING IS NOT SPENT. The allocator and the lowering pass take a
\ one-shot binding because nothing else in them says which module the next call
\ is about, so a caller could otherwise run against a module it never bound. Here
\ that hole is closed by an identity check instead: the binding records the module
\ it was taken over and ACCEPT refuses any other one by name. A binding left
\ behind by a refused check is therefore harmless - it can only ever be used
\ against the module it belongs to - and the one state worth refusing is having
\ been asked for a check before any binding was taken at all.
: BND-TAKE ( -- )
   BND-MODE @ BOUND-YES <> if E-A64RAV-STATE throw then ;

: BND-MODULE-CK ( IR-BUILD:module -- )
   IR-BUILD:FMODULE  0 BND-MOD @  IR-ID:MODULE-SAME?
   0= if E-A64RAV-MODULE throw then ;

: DIALECT-CK ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b  c b IR-BUILD:DIALECT@  A64IR:NAME IR-BUILD:SYMBOL-IS?
   0= if E-A64RAV-MODULE throw then
   c b IR-BUILD:SCHEMA-MAJOR@ A64IR:MAJOR <> if E-A64RAV-MODULE throw then
   c b IR-BUILD:SCHEMA-MINOR@ A64IR:MINOR <> if E-A64RAV-MODULE throw then ;

: WALK ( IR-BUILD:module A64EFF:gprs A64EFF:placeseq A64EFF:placeseq n -- IR-ID:ir-block-id )
   {: m:IR-BUILD:module pool:A64EFF:gprs
      args:A64EFF:placeseq outs:A64EFF:placeseq frame:n :}
   ST-NONE ST !
   BND-TAKE
   m BND-MODULE-CK
   STATE-CK
   m MODULE-CK
   pool CONTRACT-CK
   m VIEWS!
   BLOCK-OF {: bk:IR-ID:ir-block-id :}
   bk MEASURE
   COVER-CK
   INTERVAL-CK
   CLASS-CK
   REGISTER-CK
   OVERLAP-CK
   bk TIE-CK
   bk args ARG-CK
   bk outs OUT-CK
   bk args outs DSTACK-CK
   bk frame FRAME-CK
   bk FLOW-CK
   bk ;

public

\ ---- binding the dialect -----------------------------------------------------
\ Learn the identities this check needs from the dialect itself, while the module
\ is still being built: which type a general register is, which is the memory
\ token, and the two attribute keys the frame forms carry their fields under. A
\ module's symbols and types are its own ordinals, so this is the only moment any
\ of them can be asked for, and taking them from the allocator instead would be
\ the thing being checked telling the checker what to check. A second binding
\ replaces the first; see BND-TAKE for why that is safe here and not in the
\ passes that take a one-shot binding.
: BIND-DIALECT ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b DIALECT-CK
   b IR-BUILD:MODULE@ 0 BND-MOD !
   c b A64IR:GPR-TYPE  0 BND-GPR !
   c b A64IR:MEM-TYPE  0 BND-MEM !
   c b A64IR:KEY-SLOT   0 BND-SLOT !
   c b A64IR:KEY-FRAME  0 BND-FRAME !
   c b A64IR:KEY-DSLOT  0 BND-DSLOT !
   c b A64IR:KEY-DBYTES 0 BND-DBYTES !
   BOUND-YES BND-MODE ! ;

\ ---- the check ---------------------------------------------------------------
\ Accept the sealed allocation as a true assignment for this module under this
\ routine contract, or refuse it by name. Nothing is answered until this returns.
: ACCEPT ( IR-BUILD:module A64EFF:routine -- )
   A64EFF:VALIDATE A64EFF-ROUTINE:UNMAKE
   {: gi:A64EFF:placeseq gr:A64EFF:placeseq gc:A64EFF:gprs
      fi:A64EFF:fprs fr:A64EFF:fprs fc:A64EFF:fprs
      z:A64EFF:nzcv l:A64EFF:link ct:A64EFF:control
      t:A64EFF:traits size:n delta:n :}
   gi gr gc fi fr fc z l ct t size delta A64EFF-ROUTINE:MAKE
   A64EFF:GPR-WRITABLE {: pool:A64EFF:gprs :}
   pool gi gr size WALK {: bk:IR-ID:ir-block-id :}
   bk
   gi gr gc fi fr fc z l ct t size delta A64EFF-ROUTINE:MAKE
   SLOT-CK
   A64RA:GEN A-GEN !
   ST-ACCEPTED ST ! ;

: ACCEPTED? ( -- bool )
   ST @ ST-ACCEPTED = ;

\ The register that holds this value. This is the only checked answer in the
\ chain: it exists only after ACCEPT has agreed with the module, and it stops
\ existing the moment a later allocation replaces the one that was accepted. A
\ memory token holds no register, so asking for one is refused rather than
\ answered with a number that is not a register.
: REG@ ( n -- n )
   FRESH-CK
   dup 0 < over N-VALS @ >= or if E-A64RAV-COVER throw then
   dup GPR? 0= if E-A64RAV-CLASS throw then
   A64RA:CLAIM@ ;

\ Is this value one that lives in a register at all? The emitter asks before it
\ asks for a register, and a caller that wants to probe an accepted answer for
\ staleness asks about the module rather than about one value's class.
: REGISTERED? ( n -- bool )
   FRESH-CK
   dup 0 < over N-VALS @ >= or if E-A64RAV-COVER throw then
   GPR? ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;using
;package

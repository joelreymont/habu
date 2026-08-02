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
require src/compiler/native/frame.f
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

\ The three value classes this dialect has, spelled exactly as the allocator
\ spells them: a general register, a floating register, and the memory token the
\ frame forms thread. The two register classes are two FILES, and a register
\ number names a register of one of them - d0 and x0 are two registers and both
\ are number zero - so every question below about a register is asked of the file
\ its value belongs to.
0 constant C-GPR
1 constant C-TOKEN
2 constant C-FPR

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
variable NB-N                        \ blocks in the function being checked
0 NB-N !

1 TYPED-BUFFER S-FUN IR-ID:ir-fun-id

: FUN ( -- IR-ID:ir-fun-id )         0 S-FUN @ ;

1 TYPED-BUFFER BND-MOD IR-ID:ir-module-id
1 TYPED-BUFFER BND-GPR IR-ID:ir-type-id
1 TYPED-BUFFER BND-FPR IR-ID:ir-type-id
1 TYPED-BUFFER BND-MEM IR-ID:ir-type-id
1 TYPED-BUFFER BND-SLOT IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-FRAME IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-DSLOT IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-DBYTES IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-DBACK IR-ID:ir-symbol-id

create D-AT VMAX cells allot         \ where the module says each value is written
create L-AT VMAX cells allot         \ where the module says each value is last read
create S-AT VMAX cells allot         \ whether the block defines this value at all
create C-AT VMAX cells allot         \ which class the module gives each value
create W-AT SLOTS-MAX cells allot    \ where each slot was written, or -1
create U-AT VMAX cells allot         \ how many operands of the function name each value
create UB VMAX cells allot           \ which blocks name each value, one bit per block
create DB VMAX cells allot           \ which block defines each value, or -1
create RCH BMAX cells allot          \ blocks one reachability question has reached

: DEF-AT ( n -- n )                  cells D-AT + @ ;
: LAST-AT ( n -- n )                 cells L-AT + @ ;
: SEEN-AT ( n -- n )                 cells S-AT + @ ;
: CLS-AT ( n -- n )                  cells C-AT + @ ;
: USES-AT ( n -- n )                 cells U-AT + @ ;

: DEF! ( n n -- )                    {: v:n k:n :} v k cells D-AT + ! ;
: LAST! ( n n -- )                   {: v:n k:n :} v k cells L-AT + ! ;
: SEEN! ( n n -- )                   {: v:n k:n :} v k cells S-AT + ! ;
: CLS! ( n n -- )                    {: v:n k:n :} v k cells C-AT + ! ;
: USES! ( n n -- )                   {: v:n k:n :} v k cells U-AT + ! ;

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
: DBACK-OF ( IR-ID:ir-op-id -- n )   0 BND-DBACK @ ATTR-INT ;

\ The take-back count is the field only the call form carries, so carrying it is
\ what makes an operation a call - asked of the operation itself rather than of
\ its opcode, which is the rule every other reader here follows.
: DCALL? ( IR-ID:ir-op-id -- bool )
   DBACK-OF NOSLOT <> ;

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
   id DBYTES-OF NOSLOT <>  id DSLOT-OF NOSLOT <>  or
   id DBACK-OF NOSLOT <> or ;

\ Does this operation write a value into a slot, or read one out of one?
: STORES? ( IR-ID:ir-op-id -- bool )
   {: id:IR-ID:ir-op-id :}
   V-SCHR VW id OPCODE-AT IR-SCHEMA:FEFFECT@
   IR--SCHEMA-EFFECT:WRITE IR--SCHEMA-EFFECT:EQ ;

\ The straight-line subset, re-derived rather than taken on trust.
: FUN-OF ( -- IR-ID:ir-fun-id )
   FUN-COUNT 1 <> if E-A64RAV-SHAPE throw then
   MKEY 0 IR-ID:PACK-FUN ;

\ The block control leaves the routine through: the one whose terminator names
\ no successor. Exactly one, re-derived here rather than taken as the last block
\ or as whatever the allocator thought.
: RET-ORD ( IR-ID:ir-fun-id -- n )
   {: f:IR-ID:ir-fun-id :}
   -1
   f BLOCK-COUNT 0 ?do
      f i BLOCK-AT TERM-AT SUCCS-OF 0= if
         dup 0 < 0= if E-A64RAV-SHAPE throw then
         drop i
      then
   loop
   dup 0 < if E-A64RAV-SHAPE throw then ;

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

\ Three register classes: every value of this dialect is a general register, a
\ floating register, or the memory token the frame forms thread, and the class is
\ decided by the type the module gives the value against the three the dialect
\ answered. A value of any fourth type has been given a register that cannot hold
\ it. The class is RE-DERIVED here, from the module rather than from the
\ allocator's tables, which is what makes an allocation that gave a double a
\ general register a refusal rather than an agreement.
: CLASS-CK ( -- )
   N-VALS @ 0 ?do
      MKEY i IR-ID:PACK-VALUE VALUE-TYPE-AT
      {: t:IR-ID:ir-type-id :}
      t 0 BND-GPR @ SAME-TYPE? if C-GPR i CLS! then
      t 0 BND-FPR @ SAME-TYPE? if C-FPR i CLS! then
      t 0 BND-MEM @ SAME-TYPE? if C-TOKEN i CLS! then
      t 0 BND-GPR @ SAME-TYPE?  t 0 BND-FPR @ SAME-TYPE? or
      t 0 BND-MEM @ SAME-TYPE? or
      0= if E-A64RAV-CLASS throw then
   loop ;

: GPR? ( n -- bool )
   CLS-AT C-GPR = ;

: FPR? ( n -- bool )
   CLS-AT C-FPR = ;

\ Does this value live in a register at all - in either file?
: REGGED? ( n -- bool )
   dup GPR? swap FPR? or ;

\ ---- the memory order --------------------------------------------------------
\ Every memory order this module mints is passed on exactly once ON EVERY PATH
\ THROUGH THE ROUTINE. An access takes the order as it stands and answers the
\ order as it now stands, so along any one run the orders form a chain: the
\ routine's first memory operation mints one, its last ends it, and every access
\ in between reads exactly the answer of the access before it.
\
\ WHY THE RULE IS PER PATH AND NOT PER MODULE. It used to be simply "used exactly
\ once", counted over the whole function, which is the same statement for a
\ routine of one block. It stops being the same statement the moment a routine
\ branches. A two-way branch of this dialect hands its successors nothing, so
\ both of them read the order the block above them left - two uses of one value,
\ and no fork of memory at all, because only one of the two blocks runs. A loop
\ is the same shape once more: the order the body leaves is read by the latch,
\ which goes round again, and by the block the loop exits through. Counting uses
\ over the whole function would refuse both, and refusing them would mean the
\ order could not cross an edge - which is exactly what a loop with a memory word
\ in it needs it to do.
\
\ WHAT IS CHECKED INSTEAD, AND WHY IT IS NOT WEAKER. Three things, and together
\ they say "consumed exactly once on every path":
\   the order is never dropped     - every token value is read at least once;
\   no block reads one twice       - a block's own straight line is a chain;
\   no two readers are on one path - for any two blocks that read one order,
\                                    neither can be reached from the other
\                                    without passing the block that DEFINES it
\                                    again, which would give the reader a new
\                                    order rather than the one it read before.
\ The third is what the old count really stood for. Two accesses on one path each
\ claiming to follow the same order is still refused - they would either be in
\ one block, or in two blocks one of which reaches the other without redefining -
\ and a token nothing reads is still refused, so nothing the old rule caught gets
\ through. What it no longer refuses is the honest case: mutually exclusive
\ readers of one order.
\
\ THE THIRD CLAUSE IS A BACKSTOP, AND SAYS SO. The first two are reached by
\ ordinary mistakes and were falsified by making them: a selection pass that
\ builds an access and forgets to pass its order on fails the first, and one that
\ hands two accesses of a block the same order fails the second. The third was
\ not reachable from any mutation of this compiler, because a module with two
\ readers of one order on one path is refused before it gets here - either by the
\ freeze verifier's dominance rule, or by the allocator's edge rule, which finds
\ the order and the block argument it feeds live at the same time. It is written
\ anyway, and it is not claimed to be tested: the rule this file states is a rule
\ about the module, and a check that only looks at the shapes its neighbours
\ happen to catch is not a check of that rule. Removing the two clauses above it
\ and the allocator's edge rule reaches it. Dot habu-reach-the-mem-05c529af is
\ the case that would reach it from a module instead.
\
\ WHY THIS IS WORTH CHECKING WHEN NOTHING REORDERS YET. A pass that built an
\ access and forgot to pass its order on leaves a module in which the accesses
\ after it are not ordered against it - and the instructions would still be
\ emitted in the printed order, so the routine would compute the right answer and
\ every execution test would pass. What is broken is the module's claim, not
\ today's output, and a claim is exactly what a validator is for: the first pass
\ that is allowed to move an instruction would move it, and the failure would
\ appear a leaf away from its cause.
: UB-AT ( n -- n )                   cells UB + @ ;
: UB! ( n n -- )                     {: v:n k:n :} v k cells UB + ! ;
: DB-AT ( n -- n )                   cells DB + @ ;
: DB! ( n n -- )                     {: v:n k:n :} v k cells DB + ! ;

: UB-HAS? ( n n -- bool )
   {: k:n b:n :}
   k UB-AT  1 b lshift  and 0<> ;

: UB-ADD ( n n -- )
   {: k:n b:n :}
   k UB-AT  1 b lshift or  k UB! ;

\ How many distinct blocks read this value. Held against the total number of
\ reads, it says whether some block read it twice without a count per block.
: UB-BLOCKS ( n -- n )
   {: k:n :}
   0
   NB-N @ 0 ?do k i UB-HAS? if 1+ then loop ;

\ ---- reachability between blocks, with one block held out --------------------
\ The question a path rule asks is "can control get from this reader to that one
\ WITHOUT passing the operation that defines the order again". Holding the
\ defining block out of the walk answers exactly that: a route that goes back
\ through it arrives with a new order, which is not the value being checked.
: RCH? ( n -- bool )                 cells RCH + @ 0<> ;
: RCH-MARK ( n -- )                  1 swap cells RCH + ! ;

: RCH-CLEAR ( -- )
   NB-N @ 0 ?do 0 i cells RCH + ! loop ;

: SUCC-ORD ( IR-ID:ir-op-id n -- n )
   SUCC-AT IR-ID:BLOCK-LOCAL
   dup 0 < over NB-N @ >= or if E-A64RAV-SHAPE throw then ;

\ Mark every successor of this block except the one held out, and answer whether
\ any mark was new.
: RCH-EXPAND ( n n -- bool )
   {: b:n d:n :}
   FUN b BLOCK-AT TERM-AT {: t:IR-ID:ir-op-id :}
   false
   t SUCCS-OF 0 ?do
      t i SUCC-ORD
      dup d = over RCH? or if drop else RCH-MARK drop true then
   loop ;

\ Everything control can reach from one block without entering the held-out one.
: REACH-FILL ( n n -- )
   {: from:n d:n :}
   RCH-CLEAR
   from d RCH-EXPAND drop
   begin
      false
      NB-N @ 0 ?do
         i RCH? if i d RCH-EXPAND or then
      loop
      0=
   until ;

\ No other block that reads this order is reachable from this one.
: ORDER-FROM-CK ( n n -- )
   {: k:n u:n :}
   u  k DB-AT  REACH-FILL
   NB-N @ 0 ?do
      i u <>  k i UB-HAS?  and  i RCH?  and if E-A64RAV-ORDER throw then
   loop ;

: ORDER-VALUE-CK ( n -- )
   {: k:n :}
   k USES-AT 1 < if E-A64RAV-ORDER throw then
   k USES-AT  k UB-BLOCKS  <> if E-A64RAV-ORDER throw then
   k DB-AT 0 < if E-A64RAV-ORDER throw then
   NB-N @ 0 ?do
      k i UB-HAS? if k i ORDER-FROM-CK then
   loop ;

\ ---- who reads and who writes each value -------------------------------------
: COUNT-OP ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id b:n :}
   id OPERANDS-OF 0 ?do
      id i OPERAND-AT SLOT {: k:n :}
      k USES-AT 1+ k USES!
      k b UB-ADD
   loop
   id RESULTS-OF 0 ?do
      id i RESULT-AT SLOT {: k:n :}
      b k DB!
   loop ;

: COUNT-BLOCK ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   f b BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk ARG-COUNT 0 ?do
      bk i ARG-AT SLOT {: k:n :}
      b k DB!
   loop
   bk OP-COUNT 0 ?do
      bk i OP-AT b COUNT-OP
   loop ;

: ORDER-CK ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   f 0 S-FUN !
   f BLOCK-COUNT NB-N !
   NB-N @ BMAX > if E-A64RAV-SHAPE throw then
   N-VALS @ 0 ?do 0 i USES! 0 i UB! -1 i DB! loop
   NB-N @ 0 ?do f i COUNT-BLOCK loop
   N-VALS @ 0 ?do
      i CLS-AT C-TOKEN = if i ORDER-VALUE-CK then
   loop ;

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
   A64RA:FPOOL A64EFF:FPRS-N {: fpool:n :}
   N-VALS @ 0 ?do
      i A64RA:CLAIM@ {: r:n :}
      i REGGED? 0= if
         r 0 >= r A64EFF:FILE-SIZE < and if E-A64RAV-CLASS throw then
      else
         r 0 < r A64EFF:FILE-SIZE >= or if E-A64RAV-REGISTER throw then
         i FPR? if fpool else pool then
         1 r lshift and 0= if E-A64RAV-REGISTER throw then
      then
   loop ;

\ Are these two values ever live at the same instant? See the header: values
\ written at the same position always are, and otherwise the earlier one has to
\ be read for the last time at or before the later one is written.
\
\ THIS IS ALSO THE WHOLE CHECK ON COALESCING, AND IT NEEDS TO KNOW NOTHING ABOUT
\ IT. src/compiler/native/regalloc.f gives a copy's two ends one register
\ wherever their classes hold no interfering pair, and it decides that by walking
\ the copies in the module's own order, so which merges get made depends on that
\ order. None of that is re-derived here and none of it needs to be. What a merge
\ can do wrong is put two values that ARE live at the same instant into one
\ register, and OVERLAP-CK below refuses exactly that, from the module's own
\ liveness and the assignment's own registers. It is a statement about the
\ answer, so it holds whatever order the answer was reached in - and it would
\ catch a coalescer with no order at all. A validator that instead re-derived the
\ merge sequence would be checking that the allocator did what the allocator
\ does.
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
         j CLS-AT i CLS-AT =  j REGGED? and  j i CLASH? and if
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
\ (dot habu-prove-a-data-df458151).
\
\ THE TWO REGIONS DO NOT MEET YET. A routine that both reaches the caller's data
\ stack and reserves a frame would need one operation at position zero to be both
\ the frame reserve and the data-stack take, and there is no rule here for
\ nesting them. It is refused by name rather than checked half-way, and no pass
\ in the chain builds one (dot habu-let-a-data-edb3ba26).
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
\ than one cell cannot be held in a local. It is asked of every block of the
\ function rather than of one, because a routine that branches reaches its frame
\ from both ends of itself.
: SLOT-CK ( IR-ID:ir-fun-id A64EFF:routine -- )
   A64EFF:VALIDATE A64EFF-ROUTINE:UNMAKE
   {: gi:A64EFF:placeseq gr:A64EFF:placeseq gc:A64EFF:gprs
      fi:A64EFF:fprs fr:A64EFF:fprs fc:A64EFF:fprs
      z:A64EFF:nzcv l:A64EFF:link ct:A64EFF:control t:A64EFF:traits
      size:n delta:n :}
   {: f:IR-ID:ir-fun-id :}
   f BLOCK-COUNT 0 ?do
      f i BLOCK-AT {: bk:IR-ID:ir-block-id :}
      bk OP-COUNT 0 ?do
         bk i OP-AT SLOT-OF {: off:n :}
         off NOSLOT <> if
            off A64IR:SLOT-WIDTH
            gi gr gc fi fr fc z l ct t size delta A64EFF-ROUTINE:MAKE
            A64EFF:CHECK-SLOT
         then
      loop
   loop ;

\ ---- re-deriving the allocation of a routine with control flow ---------------
\ Everything above measures ONE straight-line block. A routine with control flow
\ is measured here, and every step is re-derived from the module rather than read
\ off the allocator: the linear block order, the liveness, the hull interval of
\ each value, and one clause the straight-line validator has no need for. If this
\ file asked A64RA which order it chose or which values it thought were live, it
\ would be checking the allocator's belief against the allocator's belief.
\
\ The rule it re-derives is the one src/compiler/native/regalloc.f states, and it
\ is stated again here in its own terms so the two can disagree: blocks in the
\ order the module records them, one position for each block's arguments and one
\ per operation; live-out of a block is the union of live-in over its successors
\ and live-in is what the block reads before it writes, plus live-out minus what
\ it defines; a value's range reaches back to the entry of every block it is live
\ in to and forward to the last operation of every block it is live out of.
\
\ THE EDGE CLAUSE IS THIS FILE'S REASON FOR EXISTING ON THIS PATH. A branch moves
\ nothing: the value a terminator hands over at position i and the argument it
\ lands in at the destination are one physical register, or the routine computes
\ with whatever happened to be in the destination's register instead. Checking it
\ here - against the module's own edges and the accepted registers - is what makes
\ a swapped successor pair, a mis-wired operand and a block argument left in the
\ wrong register a refusal rather than a wrong answer at run time.
64 constant SET-BITS
VMAX SET-BITS / constant SETC
0 constant P-IN
1 constant P-OUT
2 constant P-USE
3 constant P-DEF
4 constant PLANES

here CELL 1- and CELL swap - CELL 1- and allot
variable V-BLKS
0 V-BLKS !
variable V-AT
0 V-AT !
variable V-CHANGED
0 V-CHANGED !
variable V-CALLS                     \ whether the contract says this routine calls
0 V-CALLS !
variable V-FRAME                     \ whether the module reaches a frame at all
0 V-FRAME !
variable V-BASE                      \ the first frame byte the allocator's slots may use
0 V-BASE !
variable VD-AT                       \ the position the data-stack scan stands on
0 VD-AT !

create VB-ST BMAX cells allot
create VB-EN BMAX cells allot
create V-SETS PLANES BMAX * SETC * cells allot
create V-TMP SETC cells allot

: VBIT-CELL ( n -- n )   SET-BITS / ;
: VBIT-MASK ( n -- n )   SET-BITS mod 1 swap lshift ;

: VS-IX ( n n n -- n )
   {: pl:n b:n w:n :}
   pl BMAX * b + SETC * w + ;

: VS@ ( n n n -- n )     VS-IX cells V-SETS + @ ;

: VS! ( n n n n -- )
   {: val:n pl:n b:n w:n :}
   val  pl b w VS-IX cells V-SETS + ! ;

: VS-HAS? ( n n n -- bool )
   {: pl:n b:n v:n :}
   pl b v VBIT-CELL VS@  v VBIT-MASK and 0<> ;

: VS-SET ( n n n -- )
   {: pl:n b:n v:n :}
   pl b v VBIT-CELL VS@  v VBIT-MASK or  pl b v VBIT-CELL VS! ;

: VTMP-CLEAR ( -- )
   SETC 0 ?do 0 i cells V-TMP + ! loop ;

: VTMP-HAS? ( n -- bool )
   {: v:n :}
   v VBIT-CELL cells V-TMP + @  v VBIT-MASK and 0<> ;

: VTMP-SET ( n -- )
   {: v:n :}
   v VBIT-CELL cells V-TMP + @  v VBIT-MASK or
   v VBIT-CELL cells V-TMP + ! ;

: VSETS-CLEAR ( -- )
   PLANES BMAX * SETC * 0 ?do 0 i cells V-SETS + ! loop ;

\ ---- the linear order --------------------------------------------------------
: VLAY1 ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   V-AT @ b cells VB-ST + !
   V-AT @  f b BLOCK-AT OP-COUNT  + {: e:n :}
   e b cells VB-EN + !
   e 1+ V-AT ! ;

: VLAYOUT ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   f BLOCK-COUNT {: n:n :}
   n BMAX > if E-A64RAV-COVER throw then
   n V-BLKS !
   0 V-AT !
   n 0 ?do f i VLAY1 loop ;

: VOP-POS ( n n -- n )
   {: b:n i:n :}
   b cells VB-ST + @ 1+ i + ;

\ ---- liveness ----------------------------------------------------------------
: VUSE1 ( n IR-ID:ir-value-id -- )
   {: b:n id:IR-ID:ir-value-id :}
   id SLOT {: v:n :}
   v VTMP-HAS? if exit then
   P-USE b v VS-SET ;

: VDEF1 ( n IR-ID:ir-value-id -- )
   {: b:n id:IR-ID:ir-value-id :}
   id SLOT {: v:n :}
   P-DEF b v VS-SET
   v VTMP-SET ;

: VOP-UD ( n IR-ID:ir-op-id -- )
   {: b:n id:IR-ID:ir-op-id :}
   id OPERANDS-OF 0 ?do b  id i OPERAND-AT  VUSE1 loop
   id RESULTS-OF 0 ?do  b  id i RESULT-AT   VDEF1 loop ;

: VBLOCK-UD ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   f b BLOCK-AT {: bk:IR-ID:ir-block-id :}
   VTMP-CLEAR
   bk ARG-COUNT 0 ?do b  bk i ARG-AT  VDEF1 loop
   bk OP-COUNT 0 ?do  b  bk i OP-AT   VOP-UD loop ;

: VSUCC-ORD ( IR-ID:ir-op-id n -- n )
   SUCC-AT IR-ID:BLOCK-LOCAL
   dup 0 < over V-BLKS @ >= or if E-A64RAV-SHAPE throw then ;

: VOUT-ADD ( n n -- )
   {: b:n s:n :}
   SETC 0 ?do
      P-OUT b i VS@  P-IN s i VS@ or  P-OUT b i VS!
   loop ;

: VOUT ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   SETC 0 ?do 0 P-OUT b i VS! loop
   f b BLOCK-AT TERM-AT {: t:IR-ID:ir-op-id :}
   t SUCCS-OF 0 ?do
      b  t i VSUCC-ORD  VOUT-ADD
   loop ;

: VIN1 ( n n -- bool )
   {: b:n w:n :}
   P-USE b w VS@   P-OUT b w VS@  P-DEF b w VS@ invert and   or {: nv:n :}
   nv  P-IN b w VS@ = if false exit then
   nv P-IN b w VS!
   true ;

: VIN ( n -- bool )
   {: b:n :}
   false
   SETC 0 ?do b i VIN1 or loop ;

: VPASS1 ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   f b VOUT
   b VIN if 1 V-CHANGED ! then ;

: VLIVENESS ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   VSETS-CLEAR
   V-BLKS @ 0 ?do f i VBLOCK-UD loop
   begin
      0 V-CHANGED !
      V-BLKS @ 0 ?do
         f  V-BLKS @ 1- i -  VPASS1
      loop
      V-CHANGED @ 0=
   until ;

\ ---- the hull ranges ---------------------------------------------------------
: VOP-RANGE ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id pos:n :}
   id OPERANDS-OF 0 ?do id i OPERAND-AT pos NOTE-USE loop
   id RESULTS-OF 0 ?do  id i RESULT-AT  pos NOTE-DEF loop ;

: VBLOCK-RANGE ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   f b BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk ARG-COUNT 0 ?do
      bk i ARG-AT  b cells VB-ST + @  NOTE-DEF
   loop
   bk OP-COUNT 0 ?do
      bk i OP-AT  b i VOP-POS  VOP-RANGE
   loop ;

: VEXTEND1 ( n n -- )
   {: b:n k:n :}
   P-IN b k VS-HAS? if
      b cells VB-ST + @  k DEF-AT min  k DEF!
   then
   P-OUT b k VS-HAS? if
      b cells VB-EN + @  k LAST-AT max  k LAST!
   then ;

: VEXTEND-V ( n -- )
   {: k:n :}
   V-BLKS @ 0 ?do i k VEXTEND1 loop ;

: VMEASURE ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   TABLES-CLEAR
   f VLAYOUT
   f VLIVENESS
   V-BLKS @ 0 ?do f i VBLOCK-RANGE loop
   COVER-CK
   N-VALS @ 0 ?do i VEXTEND-V loop ;

\ ---- the edge clause ---------------------------------------------------------
\ A branch moves nothing, so the register holding the value a terminator hands
\ over at position i has to be the register holding the destination's argument at
\ that position. Both are read out of the accepted assignment, and the positions
\ are read out of the module's own operand list and the destination's own
\ argument list - so a successor named in the wrong order, an operand wired to
\ the wrong argument, and an argument left in a register nothing wrote all fail
\ here.
: VEDGE1 ( IR-ID:ir-op-id IR-ID:ir-block-id n -- )
   {: t:IR-ID:ir-op-id sb:IR-ID:ir-block-id i:n :}
   t i OPERAND-AT SLOT A64RA:CLAIM@
   sb i ARG-AT SLOT A64RA:CLAIM@
   <> if E-A64RAV-EDGE throw then ;

\ A terminator with more than one successor hands nothing over - its operands are
\ its own, the register it tests - so every one of those successors has to be a
\ block that takes no arguments. src/compiler/ir/verify.f cannot state this yet
\ (dot habu-state-what-a-2f99fb94), so the assignment is where it is decided:
\ an edge that was supposed to carry values and does not go through a block of
\ its own would leave those values in whatever registers the destination happened
\ to be given.
: VMULTI-CK ( IR-ID:ir-fun-id IR-ID:ir-op-id -- )
   {: f:IR-ID:ir-fun-id t:IR-ID:ir-op-id :}
   t SUCCS-OF 0 ?do
      f  t i VSUCC-ORD  BLOCK-AT ARG-COUNT 0<> if E-A64RAV-EDGE throw then
   loop ;

: VEDGE-OF ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   f b BLOCK-AT TERM-AT {: t:IR-ID:ir-op-id :}
   t SUCCS-OF 1 <> if f t VMULTI-CK exit then
   f  t 0 VSUCC-ORD  BLOCK-AT {: sb:IR-ID:ir-block-id :}
   t OPERANDS-OF sb ARG-COUNT <> if E-A64RAV-EDGE throw then
   t OPERANDS-OF 0 ?do t sb i VEDGE1 loop ;

: VEDGE-CK ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   V-BLKS @ 0 ?do f i VEDGE-OF loop ;

\ ---- the frame of a routine with control flow --------------------------------
\ A routine of more than one block reaches its frame in exactly TWO blocks: the
\ one the caller enters and the one control leaves through. That pair is not a
\ convenience. The frame forms thread a memory order and that order has to be
\ read exactly once on every run, so two frame-touching blocks where one can be
\ reached from the other would be two readers of one order on one path; the entry
\ block dominates every block of the routine and every run that returns passes
\ through the exit block, so those two - in that order - are the pair that can
\ never have that problem. It is also what makes "a slot is written before it is
\ read" decidable across a routine that branches: reading the two blocks in
\ linear order reads them in the order every run makes them in.
\
\ THE FRAME HAS ONE LAYOUT AND, WHEN THE ROUTINE CALLS, TWO OWNERS.
\ src/compiler/native/frame.f draws the line: the caller's return address is the
\ bottom slot of a calling routine's frame and the register allocator's slots
\ start above it. Both halves are decided here from the contract's own traits -
\ the link save and the link restore name the link slot and no other access may,
\ and every other frame slot named is at or above the base the allocator starts
\ at. A spill placed on top of a return address fails the second clause under its
\ own name rather than being found by a routine that returns into its own data.
\
\ AND THE FRAME ITSELF IS TAKEN AND GIVEN BACK IN THOSE TWO PLACES. The entry
\ block takes it with its first operation and the exit block gives it back with
\ the one in front of its terminator, both naming the frame the contract
\ declares, and nothing anywhere else moves the stack pointer. A routine that
\ calls adds the save and the restore inside that bracket; a routine that only
\ spills has the bracket and nothing else in it.
: VNO-FRAME ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   V-BLKS @ 0 ?do
      f i BLOCK-AT FRAMES? if E-A64RAV-FRAME throw then
   loop ;

\ Does this module reach a frame at all? Re-derived from the operations, because
\ it decides where the data-stack entry and exit sequences stand.
: VANY-FRAME ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   0 V-FRAME !
   V-BLKS @ 0 ?do
      f i BLOCK-AT FRAMES? if 1 V-FRAME ! then
   loop ;

\ One frame access at this position, moving the link register into or out of the
\ slot src/compiler/native/frame.f keeps it in.
: VLINK-AT? ( IR-ID:ir-block-id n bool -- )
   {: bk:IR-ID:ir-block-id at:n store:bool :}
   bk at OP-AT {: id:IR-ID:ir-op-id :}
   id FRAME-OF NOSLOT <> if E-A64RAV-CALL throw then
   id SLOT-OF A64FRAME:LINK-SLOT <> if E-A64RAV-CALL throw then
   store if
      id STORES? 0= if E-A64RAV-CALL throw then exit
   then
   id STORES? if E-A64RAV-CALL throw then ;

\ Only the two blocks above may reach the frame at all.
: VFRAME-BLOCKS-CK ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id rb:n :}
   V-BLKS @ 0 ?do
      i 0 <> i rb <> and if
         f i BLOCK-AT FRAMES? if E-A64RAV-FRAME throw then
      then
   loop ;

\ Nothing but the operations named here carries a frame SIZE: an operation that
\ moves the stack pointer anywhere else is a second frame inside the first. Two
\ positions are kept rather than one, because when the routine's entry block is
\ also the block control leaves through, the reserve and the release are two
\ positions of THAT ONE block; a caller with only one to keep passes NOPOS for
\ the other.
-1 constant NOPOS

: VNO-SIZE ( IR-ID:ir-block-id n n -- )
   {: bk:IR-ID:ir-block-id keep:n also:n :}
   bk OP-COUNT 0 ?do
      i keep <> i also <> and if
         bk i OP-AT FRAME-OF NOSLOT <> if E-A64RAV-FRAME throw then
      then
   loop ;

\ Is this position of this block one of the routine's two link accesses? A
\ routine that does not call has none anywhere; a routine that calls saves the
\ link register as the entry block's second operation and restores it two in
\ front of the exit block's terminator. It is a question about a POSITION rather
\ than a word answering "the" position, because when the entry block and the exit
\ block are one block both accesses are in it and one answer could not name them
\ both.
: VLINK-HERE? ( IR-ID:ir-block-id n n n -- bool )
   {: bk:IR-ID:ir-block-id b:n rb:n at:n :}
   V-CALLS @ 0= if false exit then
   b 0 = at 1 = and if true exit then
   b rb = at bk OP-COUNT 3 - = and if true exit then
   false ;

\ The partition, one access at a time: a link access names the link slot and
\ every other access names a slot the allocator was allowed to start at.
: VOWNER1 ( IR-ID:ir-block-id n n n -- )
   {: bk:IR-ID:ir-block-id b:n rb:n at:n :}
   bk at OP-AT SLOT-OF {: off:n :}
   off NOSLOT = if exit then
   bk b rb at VLINK-HERE? if
      off A64FRAME:LINK-SLOT <> if E-A64RAV-OWNER throw then
      exit
   then
   off V-BASE @ < if E-A64RAV-OWNER throw then ;

: VOWNER-BLOCK ( IR-ID:ir-block-id n n -- )
   {: bk:IR-ID:ir-block-id b:n rb:n :}
   bk OP-COUNT 0 ?do bk b rb i VOWNER1 loop ;

: VOWNER-CK ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id rb:n :}
   V-BLKS @ 0 ?do
      f i BLOCK-AT {: bk:IR-ID:ir-block-id :}
      bk i rb VOWNER-BLOCK
   loop ;

\ The bracket both shapes share: the frame is taken by the entry block's first
\ operation and given back by the one in front of the exit block's terminator,
\ both naming the frame the contract declares, and no other block reaches it.
\
\ THE TWO BLOCKS MAY BE ONE, AND THEN IT IS ONE WINDOW RATHER THAN TWO. A routine
\ with no control flow has a single block, which is both the block its caller
\ enters and the block control leaves through - `: A ( n -- n ) B 1+ ;` is
\ exactly that shape and is the commonest call site there is. The rule does not
\ change: the reserve is still the first operation and the release still stands
\ in front of the terminator, and the only difference is that both are positions
\ of one block, so the two are kept from ONE scan instead of one each from two.
\ They still have to be different positions - a block short enough for the reserve
\ and the release to be the same operation is not a frame taken and given back -
\ and that is what the length test below says.
: VBRACKET-CK ( IR-ID:ir-fun-id n n n -- )
   {: f:IR-ID:ir-fun-id rb:n want:n bad:n :}
   f 0 BLOCK-AT {: eb:IR-ID:ir-block-id :}
   f rb BLOCK-AT {: xb:IR-ID:ir-block-id :}
   xb OP-COUNT {: n:n :}
   n 2 < if bad throw then
   rb 0 = n 3 < and if bad throw then
   eb 0 want FRAME-AT?
   xb n 2 - want FRAME-AT?
   rb 0 = if
      eb 0 n 2 - VNO-SIZE
      f rb VFRAME-BLOCKS-CK
      exit
   then
   eb 0 NOPOS VNO-SIZE
   xb n 2 - NOPOS VNO-SIZE
   f rb VFRAME-BLOCKS-CK ;

\ A routine that does not call has nothing in its frame but what the register
\ allocator put there, and may have no frame at all - a routine whose values all
\ fit, which is every routine this half saw before it could spill.
: VSPILL-CK ( IR-ID:ir-fun-id n n -- )
   {: f:IR-ID:ir-fun-id rb:n want:n :}
   V-FRAME @ 0= if f VNO-FRAME exit then
   f rb want E-A64RAV-FRAME VBRACKET-CK ;

\ A routine that DOES call keeps its caller's return address in the bottom slot,
\ because the first call would otherwise destroy it. The save is the entry
\ block's second operation and the restore stands two in front of the exit
\ block's terminator, inside the bracket above. A routine that saved its return
\ address and did not restore it, or restored it from another slot, returns to
\ whatever the frame happened to hold.
\
\ AND THE TWO WINDOWS BECOME ONE WHEN THE ROUTINE HAS ONE BLOCK, which is what a
\ word that calls another word without branching is: `: A ( n -- n ) B 1+ ;`. The
\ save is still at position one and the restore still stands two in front of the
\ terminator; when the entry block and the exit block are the same block those
\ are two positions of it, and the rule is the same rule read once instead of
\ twice. They still have to be DIFFERENT positions, so the block has to be long
\ enough to hold the reserve, the save, the restore, the release and the return -
\ five operations - or the save and the restore would be one operation claiming
\ to be both.
: VLINK-CK ( IR-ID:ir-fun-id n n -- )
   {: f:IR-ID:ir-fun-id rb:n want:n :}
   f 0 BLOCK-AT {: eb:IR-ID:ir-block-id :}
   f rb BLOCK-AT {: xb:IR-ID:ir-block-id :}
   eb OP-COUNT 2 < if E-A64RAV-CALL throw then
   xb OP-COUNT {: n:n :}
   n 3 < if E-A64RAV-CALL throw then
   rb 0 = n 5 < and if E-A64RAV-CALL throw then
   f rb want E-A64RAV-CALL VBRACKET-CK
   eb 1 true VLINK-AT?
   xb n 3 - false VLINK-AT? ;

: VFRAME-CK ( IR-ID:ir-fun-id n n -- )
   {: f:IR-ID:ir-fun-id rb:n want:n :}
   V-CALLS @ 0= if f rb want VSPILL-CK else f rb want VLINK-CK then
   f rb VOWNER-CK ;

\ ---- the data stack, across blocks -------------------------------------------
\ The entry sequence is at the top of the block the caller enters, the exit
\ sequence is in front of the terminator of the block control leaves through, and
\ nothing anywhere else may touch the caller's stack.
: VNO-DSTACK ( IR-ID:ir-block-id -- )
   {: bk:IR-ID:ir-block-id :}
   bk OP-COUNT 0 ?do
      bk i OP-AT DSTACK-TOUCH? if E-A64RAV-DSTACK throw then
   loop ;

: VB-OPS ( n -- n )
   {: b:n :}
   b cells VB-EN + @  b cells VB-ST + @ - ;

\ How many operations the routine's own frame costs at each end. A routine that
\ calls takes its frame and saves the return address before it reads a single
\ argument, and restores and gives the frame back after it has published its
\ results, so both sequences sit two operations further in than they otherwise
\ would. A routine that only spills has the frame without the save, so it costs
\ one at each end; one that never reaches a frame costs none. The first is the
\ contract's own declaration and the second is re-derived from the module, for
\ the same reason every other fact here is.
: PRO-N ( -- n )
   V-CALLS @ 0<> if 2 exit then
   V-FRAME @ 0<> if 1 exit then
   0 ;

: VDEXIT-POS? ( n n n -- bool )
   {: n:n r:n at:n :}
   n 2 - PRO-N - {: p:n :}
   at p = if true exit then
   at p r - >= at p < and ;

: VDPOS? ( n n n n n n -- bool )
   {: b:n at:n eb:n rb:n a:n r:n :}
   b eb = at a PRO-N + <= and if true exit then
   b rb = if rb VB-OPS r at VDEXIT-POS? exit then
   false ;

\ ---- a call site, re-derived -------------------------------------------------
\ What the dialect lowers a call to, measured from the module rather than taken
\ from the selector: a run of data-stack stores naming slots zero upwards in
\ order, the call, and a run of data-stack loads naming slots zero upwards in
\ order. The call's own two byte counts have to be exactly those two runs - it
\ moves the pointer up over what it stores and back down over what it loads - so
\ a store the selector forgot, a slot named out of order, and a byte count that
\ does not stand for its run are three different disagreements and all three are
\ refused here. That is the whole of what makes a caller's live values survive:
\ the pointer ends where it started and every value comes back out of the slot it
\ went into.
: DSTORE-RUN ( IR-ID:ir-block-id n -- n )
   {: bk:IR-ID:ir-block-id at:n :}
   bk OP-COUNT {: n:n :}
   0
   n at - 0 ?do
      bk at i + OP-AT {: id:IR-ID:ir-op-id :}
      id DSLOT-OF  i A64IR:SLOT-WIDTH * =  id STORES?  and
      0= if leave then
      drop i 1+
   loop ;

: DLOAD-RUN ( IR-ID:ir-block-id n -- n )
   {: bk:IR-ID:ir-block-id at:n :}
   bk OP-COUNT {: n:n :}
   0
   n at - 0 ?do
      bk at i + OP-AT {: id:IR-ID:ir-op-id :}
      id DSLOT-OF  i A64IR:SLOT-WIDTH * =  id STORES? 0=  and
      0= if leave then
      drop i 1+
   loop ;

: VCALL-SITE ( IR-ID:ir-block-id n -- n )
   {: bk:IR-ID:ir-block-id at:n :}
   bk at DSTORE-RUN {: g:n :}
   at g + {: cp:n :}
   cp bk OP-COUNT >= if E-A64RAV-CALL throw then
   bk cp OP-AT {: id:IR-ID:ir-op-id :}
   id DCALL? 0= if E-A64RAV-CALL throw then
   id DBYTES-OF  g A64IR:SLOT-WIDTH * <> if E-A64RAV-CALL throw then
   bk cp 1+ DLOAD-RUN {: b:n :}
   id DBACK-OF  b A64IR:SLOT-WIDTH * <> if E-A64RAV-CALL throw then
   cp 1+ b + ;

\ Every position of every block that touches the caller's data stack is either
\ part of the routine's own entry or exit, or part of one call site. The scan is
\ forward and consumes a whole call site at a time, so a store left over between
\ two sites, or a call with no stores in front of it, stops at the first position
\ the scan cannot account for.
: VDCLEAN1 ( IR-ID:ir-fun-id n n n n n -- )
   {: f:IR-ID:ir-fun-id b:n eb:n rb:n a:n r:n :}
   f b BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk OP-COUNT {: n:n :}
   0 VD-AT !
   begin VD-AT @ n < while
      b VD-AT @ eb rb a r VDPOS? if
         VD-AT @ 1+ VD-AT !
      else
         bk VD-AT @ OP-AT DSTACK-TOUCH? if
            bk VD-AT @ VCALL-SITE VD-AT !
         else
            VD-AT @ 1+ VD-AT !
         then
      then
   repeat ;

: VDSTACK-CK ( IR-ID:ir-fun-id n A64EFF:placeseq A64EFF:placeseq -- )
   {: f:IR-ID:ir-fun-id rb:n args:A64EFF:placeseq outs:A64EFF:placeseq :}
   args A64EFF:SEQ-SLOTS {: a:n :}
   outs A64EFF:SEQ-SLOTS {: r:n :}
   a 0= r 0= and if
      V-BLKS @ 0 ?do f i BLOCK-AT VNO-DSTACK loop
      exit
   then
   f 0 BLOCK-AT {: eb:IR-ID:ir-block-id :}
   eb OP-COUNT a 1+ PRO-N + < if E-A64RAV-DSTACK throw then
   eb PRO-N  a A64IR:SLOT-WIDTH *  DTAKE-AT?
   a 0 ?do
      eb PRO-N i + 1+  args i A64EFF:SEQ-SLOT@ A64IR:SLOT-WIDTH *  DSLOT-AT?
   loop
   f rb BLOCK-AT {: xb:IR-ID:ir-block-id :}
   xb OP-COUNT PRO-N - {: n:n :}
   n r 2 + < if E-A64RAV-DSTACK throw then
   xb n 2 -  r A64IR:SLOT-WIDTH *  DTAKE-AT?
   r 0 ?do
      xb  n 2 - r - i +  outs i A64EFF:SEQ-SLOT@ A64IR:SLOT-WIDTH *  DSLOT-AT?
   loop
   V-BLKS @ 0 ?do f i 0 rb a r VDCLEAN1 loop ;

\ ---- the whole re-derivation -------------------------------------------------
: VBLOCK-CKS ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   V-BLKS @ 0 ?do
      f i BLOCK-AT TIE-CK
      f i BLOCK-AT FLOW-CK
   loop ;

\ ---- which re-derivation a routine gets --------------------------------------
\ The same question src/compiler/native/regalloc.f asks, asked from the same two
\ facts: a routine of more than one block, and a routine that CALLS whatever its
\ shape. It has to be the same question, because the two passes number a
\ routine's positions differently - within the block for the straight-line walk,
\ across the whole routine for the other - and this file re-derives the numbering
\ the allocator used. A routine sent one way there and the other way here would
\ be measured in one numbering and checked in another, so the rule is written in
\ both files from the contract and the module rather than inferred in either.
\
\ AND A CALLING ROUTINE OF ONE BLOCK IS WHY THE SECOND HALF IS THERE. It has a
\ frame holding its caller's return address AND it reaches the caller's data
\ stack, and the straight-line re-derivation refuses that combination by name
\ (DSTACK-CK above) because its frame rule and its data-stack rule both want the
\ block's first operation. The re-derivation below has the rule already: the
\ prologue first, the entry sequence after it, counted by PRO-N. Unifying the two
\ numberings so this question disappears is dot habu-unify-the-two-d4f93e83.
: CALLS-MB? ( IR-ID:ir-fun-id -- bool )
   BLOCK-COUNT 1 <> if true exit then
   V-CALLS @ 0<> ;

: MB-VERIFY ( IR-ID:ir-fun-id n A64EFF:placeseq A64EFF:placeseq n -- )
   {: f:IR-ID:ir-fun-id rb:n args:A64EFF:placeseq outs:A64EFF:placeseq frame:n :}
   f VMEASURE
   f VANY-FRAME
   INTERVAL-CK
   CLASS-CK
   f ORDER-CK
   REGISTER-CK
   OVERLAP-CK
   f VEDGE-CK
   f rb frame VFRAME-CK
   f VBLOCK-CKS
   f 0 BLOCK-AT args ARG-CK
   f rb BLOCK-AT outs OUT-CK
   f rb args outs VDSTACK-CK ;

\ ---- what the acceptance is bound to -----------------------------------------
: STATE-CK ( -- )
   A64RA:SEALED? 0= if E-A64RAV-STATE throw then ;

: MODULE-CK ( IR-BUILD:module -- )
   IR-BUILD:FMODULE A64RA:MODULE@ IR-ID:MODULE-SAME?
   0= if E-A64RAV-MODULE throw then ;

\ The allocation depends on two facts of the contract: which registers of each
\ file the routine may write, which in both cases is what it destroys together
\ with what it returns a value in. A contract that names a different set is a
\ different allocation problem, and this one is not an answer to it. Both are
\ checked, because an allocation made against one pool and accepted against
\ another would hand out registers the routine promised to keep.
: CONTRACT-CK ( A64EFF:gprs A64EFF:fprs -- )
   {: pool:A64EFF:gprs fpool:A64EFF:fprs :}
   pool A64RA:POOL A64EFF-GPRS:EQ 0= if E-A64RAV-CONTRACT throw then
   fpool A64RA:FPOOL A64EFF-FPRS:EQ 0= if E-A64RAV-CONTRACT throw then ;

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

: WALK ( IR-BUILD:module A64EFF:gprs A64EFF:fprs A64EFF:placeseq A64EFF:placeseq n -- IR-ID:ir-fun-id )
   {: m:IR-BUILD:module pool:A64EFF:gprs fpool:A64EFF:fprs
      args:A64EFF:placeseq outs:A64EFF:placeseq frame:n :}
   ST-NONE ST !
   BND-TAKE
   m BND-MODULE-CK
   STATE-CK
   m MODULE-CK
   pool fpool CONTRACT-CK
   m VIEWS!
   FUN-OF {: f:IR-ID:ir-fun-id :}
   f RET-ORD {: rb:n :}
   f CALLS-MB? if
      f rb args outs frame MB-VERIFY
      f exit
   then
   f 0 BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk MEASURE
   COVER-CK
   INTERVAL-CK
   CLASS-CK
   f ORDER-CK
   REGISTER-CK
   OVERLAP-CK
   bk TIE-CK
   bk args ARG-CK
   bk outs OUT-CK
   bk args outs DSTACK-CK
   bk frame FRAME-CK
   bk FLOW-CK
   f ;

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
   c b A64IR:FPR-TYPE  0 BND-FPR !
   c b A64IR:MEM-TYPE  0 BND-MEM !
   c b A64IR:KEY-SLOT   0 BND-SLOT !
   c b A64IR:KEY-FRAME  0 BND-FRAME !
   c b A64IR:KEY-DSLOT  0 BND-DSLOT !
   c b A64IR:KEY-DBYTES 0 BND-DBYTES !
   c b A64IR:KEY-DBACK  0 BND-DBACK !
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
   gi gr gc fi fr fc z l ct t size delta A64EFF-ROUTINE:MAKE
   A64EFF:FPR-WRITABLE {: fpool:A64EFF:fprs :}
   t A64EFF:T-CALL A64EFF:TRAITS-HAS? if 1 else 0 then V-CALLS !
   t A64FRAME:SPILL-BASE V-BASE !
   pool fpool gi gr size WALK {: f:IR-ID:ir-fun-id :}
   f
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
   dup REGGED? 0= if E-A64RAV-CLASS throw then
   A64RA:CLAIM@ ;

\ Is this value one that lives in a register at all? The emitter asks before it
\ asks for a register, and a caller that wants to probe an accepted answer for
\ staleness asks about the module rather than about one value's class.
: REGISTERED? ( n -- bool )
   FRESH-CK
   dup 0 < over N-VALS @ >= or if E-A64RAV-COVER throw then
   REGGED? ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;using
;package

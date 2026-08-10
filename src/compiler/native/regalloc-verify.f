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

\ No position at all: what the tables hold for a value this check has not
\ measured yet. Every position a measured value carries is one of the linear
\ order re-derived below, which starts at zero.
-1 constant NOPOS

0 constant ST-NONE
1 constant ST-ACCEPTED

0 constant BOUND-NO
1 constant BOUND-YES

\ The three value classes this dialect has, spelled exactly as the allocator
\ spells them: a general register, a floating register, and the memory token the
\ frame forms thread.
0 constant C-GPR
1 constant C-TOKEN
2 constant C-FPR

\ The register FILES those classes live in. A register number names a register of
\ ONE file - d0 and x0 are two registers and both are number zero - so every
\ question below about a register is asked of the FILE the value's class belongs
\ to, and never of the class itself.
\
\ WHY THE TWO ARE WRITTEN DOWN SEPARATELY WHILE THEY STILL AGREE. Today the map
\ below is one-to-one, so "same class" and "same file" pick out the same pairs of
\ values and every check here would read the same either way. They stop agreeing
\ the moment one file holds two classes, which is the shape this machine's vector
\ registers already have - v3 and d3 are ONE register - and a check keyed on the
\ class would then quietly stop comparing a vector against a double and let the
\ allocator put both in that one register. So the file is what every register
\ question is asked of, ahead of any class that shares one, and the agreement of
\ today's two answers is a fact about the map rather than a thing anything relies
\ on.
\
\ AND A CLASS THIS MAP DOES NOT NAME IS REFUSED. FILE-OF is total over the
\ classes it knows and throws for every other one, so a class added without a
\ file cannot be silently treated as a general register: it fails on the first
\ module that carries one, loudly, which is the only behaviour that keeps the
\ paragraph above true of a dialect that grows.
2 constant FILES-N
0 constant F-GPR
1 constant F-FPR

\ The class is held in no register file at all.
-1 constant NOFILE

: FILE-OF ( n -- n )
   {: cls:n :}
   cls C-GPR = if F-GPR exit then
   cls C-FPR = if F-FPR exit then
   cls C-TOKEN = if NOFILE exit then
   E-A64RAV-CLASS throw ;

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
variable V-DSTACK                    \ whether the contract declares the data-stack convention
0 V-DSTACK !

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
1 TYPED-BUFFER BND-ENTRY IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-TRAP IR-ID:ir-symbol-id
1 TYPED-BUFFER V-POOL A64EFF:gprs
1 TYPED-BUFFER V-FPOOL A64EFF:fprs

create D-AT VMAX cells allot         \ where the module says each value is written
create L-AT VMAX cells allot         \ where the module says each value is last read
create S-AT VMAX cells allot         \ whether the block defines this value at all

\ Where each function's positions begin on the module's one number line, with
\ the line's end filed one past the last function. A value belongs to the
\ function whose window its definition position falls in, which is the only sense
\ in which "belongs to" is needed here: the intervals of two functions are
\ disjoint, so the window is a fact about the line rather than a second copy of
\ the module's structure.
create F-VB NFROZEN:FMAX 1 + cells allot
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
: FILE-AT ( n -- n )                 CLS-AT FILE-OF ;
: USES-AT ( n -- n )                 cells U-AT + @ ;

: DEF! ( n n -- )                    {: v:n k:n :} v k cells D-AT + ! ;
: LAST! ( n n -- )                   {: v:n k:n :} v k cells L-AT + ! ;
: SEEN! ( n n -- )                   {: v:n k:n :} v k cells S-AT + ! ;
: CLS! ( n n -- )                    {: v:n k:n :} v k cells C-AT + ! ;
: USES! ( n n -- )                   {: v:n k:n :} v k cells U-AT + ! ;

: TABLES-CLEAR ( -- )
   VMAX 0 ?do
      0 i SEEN!
      NOPOS i DEF!
      NOPOS i LAST!
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

\ Does this operation carry the trap form's own target key? The trap branch is
\ told from the tail branch by a key of its own, for the reason
\ src/compiler/native/a64ir.f gives where that key is declared: under `a64.entry`
\ it would answer TAILBR? below, and this pass would then demand that the
\ routine's contract declare a tail call and measure the routine's results
\ against a data-stack run whose one value is a family ordinal.
: TRAP-AT? ( IR-ID:ir-op-id -- bool )
   0 BND-TRAP @ ATTR-INT NOSLOT <> ;

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

\ The functions this check measures, re-derived rather than taken on trust. Every
\ one is measured and every one is checked: an assignment is accepted for the
\ whole module, so a function left out would be a routine emitted under registers
\ nothing agreed with - and a quotation body is exactly such a routine, reached
\ only through an address some caller executes.
: FUN-AT ( n -- IR-ID:ir-fun-id )
   {: k:n :}
   k 0 < k FUN-COUNT >= or if E-A64RAV-SHAPE throw then
   MKEY k IR-ID:PACK-FUN ;

: FUNS-CK ( -- n )
   FUN-COUNT {: n:n :}
   n 1 < if E-A64RAV-SHAPE throw then
   n NFROZEN:FMAX > if E-A64RAV-COVER throw then
   n ;

\ THE BLOCK THE ROUTINE'S RESULTS LEAVE THROUGH, and NO-RET when there is none.
\ The rule and the reason a trap block is not that block are written once, in
\ src/compiler/native/regalloc.f MB-RET-ORD; this is the same question asked of
\ this pass's own view, which is why it is asked again rather than taken from the
\ allocator.
-1 constant NO-RET

: RET-ORD ( IR-ID:ir-fun-id -- n )
   {: f:IR-ID:ir-fun-id :}
   NO-RET
   f BLOCK-COUNT 0 ?do
      f i BLOCK-AT TERM-AT {: t:IR-ID:ir-op-id :}
      t SUCCS-OF 0=  t TRAP-AT? 0=  and if
         dup NO-RET <> if E-A64RAV-SHAPE throw then
         drop i
      then
   loop ;

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

\ ---- the checks --------------------------------------------------------------
\ Every value of the module is a value of this function, the allocation covers
\ exactly those values, and the interval it recorded for each one is the interval
\ the module gives.
\ How many values the module holds, read before any function is measured because
\ the measuring loops over them.
: VALS-N! ( -- )
   V-VALR VW IR-OP:FVALUES {: n:n :}
   n VMAX > if E-A64RAV-COVER throw then
   n N-VALS ! ;

\ EVERY VALUE OF THE MODULE WAS DEFINED EXACTLY ONCE, ACROSS ALL ITS FUNCTIONS -
\ which is the same guarantee this made when a module held one function, said
\ over the whole module instead of over the only function in it. The two halves
\ come from two places and always did. ONCE is NOTE-DEF's refusal: it rejects a
\ value whose SEEN flag is already set, and because the flag table is now cleared
\ once for the module rather than once per function, a value defined in two
\ functions is refused there instead of quietly overwriting the first definition.
\ EVERY is the sweep below, asked after the last function has been measured
\ rather than after the first, so a value defined in no function at all is still
\ a refusal and a value defined in a LATER function is no longer mistaken for one.
\ The count is held against the allocator's own so that a module and an
\ allocation describing different numbers of values cannot agree.
: COVER-CK ( -- )
   N-VALS @ A64RA:VALUES <> if E-A64RAV-COVER throw then
   N-VALS @ 0 ?do i SEEN-AT 0= if E-A64RAV-COVER throw then loop ;

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

\ Is this value's register one of THIS file's? Asked by everything that counts
\ registers, holds one against a pool, or compares two of them.
: IN-FILE? ( n n -- bool )
   {: v:n fl:n :}
   v FILE-AT fl = ;

\ Does this value live in a register at all - in any file?
: REGGED? ( n -- bool )
   FILE-AT NOFILE <> ;

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

: IN-WINDOW? ( n n n -- bool )
   {: v:n lo:n hi:n :}
   v DEF-AT {: d:n :}
   d NOPOS = if false exit then
   d lo >= d hi < and ;

\ THE TOKEN VALUES IT ORDERS ARE THIS FUNCTION'S, and the window is how it says
\ so. The counts this rebuilds - which blocks name a value, where it is defined -
\ are counted from THIS function's blocks, so a token value belonging to another
\ function would be ordered against blocks it never appears in and refused for
\ being absent from them. On one number line a value is this function's exactly
\ when its definition position lies in this function's span, which is the same
\ disjointness the overlap check rests on rather than a second rule.
: ORDER-CK ( IR-ID:ir-fun-id n n -- )
   {: f:IR-ID:ir-fun-id lo:n hi:n :}
   f 0 S-FUN !
   f BLOCK-COUNT NB-N !
   NB-N @ BMAX > if E-A64RAV-SHAPE throw then
   N-VALS @ 0 ?do 0 i USES! 0 i UB! -1 i DB! loop
   NB-N @ 0 ?do f i COUNT-BLOCK loop
   N-VALS @ 0 ?do
      i CLS-AT C-TOKEN =  i lo hi IN-WINDOW?  and if i ORDER-VALUE-CK then
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
\ The registers one file's pool holds. Named per file with no default arm: a
\ value of a file this word does not know would otherwise be held against the
\ general registers, which is the one wrong answer that reads as an agreement.
: FILE-POOL ( n n n -- n )
   {: fl:n pool:n fpool:n :}
   fl F-GPR = if pool exit then
   fl F-FPR = if fpool exit then
   E-A64RAV-CLASS throw ;

: REGISTER-CK ( -- )
   A64RA:POOL A64EFF:GPRS-N {: pool:n :}
   A64RA:FPOOL A64EFF:FPRS-N {: fpool:n :}
   N-VALS @ 0 ?do
      i A64RA:CLAIM@ {: r:n :}
      i REGGED? 0= if
         r 0 >= r A64EFF:FILE-SIZE < and if E-A64RAV-CLASS throw then
      else
         r 0 < r A64EFF:FILE-SIZE >= or if E-A64RAV-REGISTER throw then
         i FILE-AT pool fpool FILE-POOL
         1 r lshift and 0= if E-A64RAV-REGISTER throw then
      then
   loop ;

\ ---- what a caller may keep in a register across a call ----------------------
\ THE RULE. A value whose own live range spans a call site is in a register the
\ callee does not write. Nothing in a Habu word's convention is callee-saved, so
\ the only reason such a register exists is that the callee is a routine this
\ system published and recorded what it destroys (src/compiler/native/clobber.f);
\ for a callee with no record - every word the engine's own emitter compiled -
\ the set is the whole pool and nothing may cross the call in a register at all.
\
\ WHY IT IS RE-DERIVED HERE AND NOT READ OFF THE CALL SITE. The selector decided
\ how many values to leave in registers, and this file's whole job is to disagree
\ with the selector when it is wrong. So the callee's address is read off the
\ operation - which is where the branch's displacement was measured from, so it
\ IS the code that will run - and the destroyed set is asked of the one record
\ that published it. A selector that saved too little is refused here whatever it
\ believed, and so is an allocator that put a crossing value in a register the
\ callee writes.
\
\ AND THE RANGE IS OPEN AT BOTH ENDS, for the reason the allocator's own version
\ gives: a value the site stores in front of the branch is dead there and a value
\ it loads behind the branch is not alive yet, so neither is at risk. What is at
\ risk is a value defined strictly before and read strictly after.
: VCALL-ENTRY ( IR-ID:ir-op-id -- n )
   0 BND-ENTRY @ ATTR-INT ;

\ The form that leaves the routine THROUGH another routine. It is told from a
\ call to another word by the two fields, not by an opcode, which is the reading
\ every other question here is asked by: both name an ADDRESS, and only a call
\ carries a take-back count - because only a call has anything to take back.
: TAILBR? ( IR-ID:ir-op-id -- bool )
   {: id:IR-ID:ir-op-id :}
   id VCALL-ENTRY NOSLOT = if false exit then
   id DBACK-OF NOSLOT = ;

\ What the callee leaves alone in ONE file, as a mask of that file's registers.
\ Per file and with no default arm, for FILE-POOL's reason: a file this word does
\ not name would be held against the general registers' record, and a value of it
\ would cross the call looking safe.
: VCALL-BITS ( IR-ID:ir-op-id n -- n )
   {: id:IR-ID:ir-op-id fl:n :}
   id VCALL-ENTRY {: e:n :}
   fl F-FPR = if
      e NOSLOT = if 0 V-FPOOL @ A64EFF:FPRS-N exit then
      e 0 V-FPOOL @ NCLOB:FPR-CLOB A64EFF:FPRS-N exit
   then
   fl F-GPR = 0= if E-A64RAV-CLASS throw then
   e NOSLOT = if 0 V-POOL @ A64EFF:GPRS-N exit then
   e 0 V-POOL @ NCLOB:GPR-CLOB A64EFF:GPRS-N ;

: CLOB-AT ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id p:n :}
   id DCALL? 0= if exit then
   N-VALS @ 0 ?do
      i REGGED?  i DEF-AT p <  and  i LAST-AT p >  and if
         id i FILE-AT VCALL-BITS  1 i A64RA:CLAIM@ lshift  and
         0<> if E-A64RAV-CLOBBER throw then
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

\ HOW THE FILE KEYING WAS PROVED, SINCE NO FIXTURE CAN BUILD THE PAIR IT CATCHES.
\ This dialect has one class per file, so a module of it cannot hold two values
\ of two classes in one file, and the refusal below is reached the way this
\ file's other closed-world clauses are: by mutating the compiler and running the
\ gate. The mutation adds the third class the machine already has - a vector,
\ which lives in the FLOATING file because v3 and d3 are one register - and gives
\ the ALLOCATOR the plausible wrong model that a vector is a file of its own, so
\ it hands one vector and one double the same d0 while both are alive. That is an
\ allocator bug of exactly the kind this file exists to refuse.
\
\ Keyed on the CLASS, the verifier accepted it: ACCEPTED? true, both values
\ reported register 0, and a routine whose two live doubles are one register was
\ passed to the emitter with no diagnostic. Keyed on the FILE, the same tree
\ throws E-A64RAV-OVERLAP. Both runs were over TWO-FILES-CASE in
\ test/compiler/native-regalloc.f, which is the module the mutation needs and is
\ in the suite for its own sake: it is what proves this clause does not refuse a
\ cell and a double that hold register zero of two different files.
\
\ WHICH PAIRS ARE COMPARED, AND WHY IT IS THE FILE THAT DECIDES. Two values can
\ be handed one machine register exactly when their registers are numbered in the
\ same file, so that is the pair this asks about. It is deliberately NOT "the
\ same class": one file may hold more than one class - this machine's vector and
\ floating classes are one file, v3 and d3 being one register - and a class-keyed
\ question would then walk past the one pair it exists to catch and call the
\ result checked. The file is asked of both, and a value in no file is not in
\ this question at all, so the memory token's non-register is never compared with
\ anything.
: OVERLAP-CK ( -- )
   N-VALS @ {: n:n :}
   n 0 ?do
      n i 1+ ?do
         j REGGED?  j FILE-AT i FILE-AT = and  j i CLASH? and if
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
\ operation, and VDSTACK-CK below is what judges those. This asks the LIST and
\ not the contract's declared convention, because how many positions are
\ registers is a property the list answers on its own; which convention the
\ routine speaks is what the three checks under it read, and they read it from
\ the declaration.
: REG-POSITIONS ( A64EFF:placeseq -- n )
   {: s:A64EFF:placeseq :}
   s A64EFF:SEQ-LEN {: len:n :}
   s A64EFF:SEQ-SLOTS {: sl:n :}
   sl 0= if len exit then
   sl len <> if E-A64RAV-PLACE throw then
   0 ;

: ARG-CK ( IR-ID:ir-block-id A64EFF:placeseq -- )
   {: bk:IR-ID:ir-block-id args:A64EFF:placeseq :}
   V-DSTACK @ 0<> bk ARG-COUNT 0<> and
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
\ A ROUTINE THAT LEAVES THROUGH A CALLEE RETURNS NOTHING FROM HERE, so its
\ terminator's one operand is the data-stack order it ends and not a value in a
\ register. Under the data-stack convention the returned values were written into
\ the caller's cells before the branch - or are still standing in them - and the
\ callee is what will publish them, so there is no position for this rule to be
\ about. A tail branch under a REGISTER convention would be one, and there is no
\ such convention in the chain: the selector refuses a call under it, and a tail
\ branch is a call. THE QUESTION IS THE CONTRACT'S DECLARATION and not whether
\ the result list names a slot - a routine that returns nothing names no slot and
\ still leaves through the data-stack convention it was entered under.
: OUT-TAIL-CK ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   V-DSTACK @ 0= if E-A64RAV-PLACE throw then
   id OPERANDS-OF 1 <> if E-A64RAV-PLACE throw then ;

: OUT-CK ( IR-ID:ir-block-id A64EFF:placeseq -- )
   {: bk:IR-ID:ir-block-id outs:A64EFF:placeseq :}
   V-BLKR VW V-OPR VW MKEY bk IR-FUN:FTERMINATOR@ {: id:IR-ID:ir-op-id :}
   id TAILBR? if id OUT-TAIL-CK exit then
   V-DSTACK @ 0<> id OPERANDS-OF 0<> and
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

\ ---- the data stack ----------------------------------------------------------
\ A routine whose convention names data-stack slots reaches the caller's stack
\ with a fixed sequence, and this is where the module is measured against the
\ declaration that sequence came from. Four facts are decidable from one module
\ and one contract, and all four are checked by VDSTACK-CK below: the pointer is
\ moved from where the caller left it to where the body stands, and from there to
\ exactly one past the results before the routine returns; each load names the
\ slot the argument place at its position names, in that order; each store names
\ the slot the result place at its position names, in that order; and nothing
\ anywhere else touches the data stack.
\
\ WHAT IS NOT DECIDABLE HERE, and its owner. Whether the value a store publishes
\ is the value the program computed for that result is a statement about the
\ module the selector read, and this file is handed one module - the same gap the
\ spill lowering has (dot habu-prove-the-spill-0294e0e8), with the same owner
\ (dot habu-prove-a-data-df458151).
: DMOVE-AT? ( IR-ID:ir-block-id n n -- )
   {: bk:IR-ID:ir-block-id at:n want:n :}
   bk at OP-AT {: id:IR-ID:ir-op-id :}
   id DSLOT-OF NOSLOT <> if E-A64RAV-DSTACK throw then
   id DBYTES-OF want <> if E-A64RAV-DSTACK throw then ;

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
   {: cv:A64EFF:conv gi:A64EFF:placeseq gr:A64EFF:placeseq gc:A64EFF:gprs
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
            cv gi gr gc fi fr fc z l ct t size delta A64EFF-ROUTINE:MAKE
            A64EFF:CHECK-SLOT
         then
      loop
   loop ;

\ ---- re-deriving the allocation ----------------------------------------------
\ Everything above judges one operation, one slot or one pair of values. The
\ routine as a whole is measured here, and every step is re-derived from the
\ module rather than read off the allocator: the linear block order, the
\ liveness, the hull interval of each value, and the edge clause. If this file
\ asked A64RA which order it chose or which values it thought were live, it would
\ be checking the allocator's belief against the allocator's belief.
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
variable V-TAIL                      \ whether the contract says control leaves through a callee
0 V-TAIL !
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

\ THE POSITIONS RUN ACROSS THE WHOLE MODULE AND NOT FROM EACH FUNCTION'S OWN
\ ZERO, which is what lets one set of value tables describe several functions. A
\ module holds one append-only value arena (src/compiler/ir/op.f), so a second
\ function's values are new ordinals in the same space; if its POSITIONS restarted
\ at zero, two values of two functions would carry the same interval and
\ OVERLAP-CK would read a register legitimately reused by two routines that never
\ run at once as a live conflict. On one number line the intervals of two
\ functions are disjoint by construction, so the overlap question needs no notion
\ of which function a value came from and answers correctly without one.
\
\ The base is passed in because this is run twice over each function: once to
\ measure every function onto the line, and once more, from the SAME base, to put
\ the block tables back for the structural checks that read them.
: VLAYOUT ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id base:n :}
   f BLOCK-COUNT {: n:n :}
   n BMAX > if E-A64RAV-COVER throw then
   n V-BLKS !
   base V-AT !
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

\ One function onto the number line: its blocks laid out from the base it was
\ given, its liveness, the interval every value it defines gets from its own
\ blocks, and the extension of those intervals across the blocks they live
\ through. The extension is asked only about the values THIS function defined -
\ VEXTEND-V reads V-BLKS, which holds this function's blocks - so asking it about
\ another function's value would extend that value's interval over blocks it has
\ nothing to do with.
\
\ NEITHER THE TABLES NOR THE COVER CHECK BELONG HERE ANY MORE. Both are about the
\ MODULE: the tables are cleared once before the first function so that the
\ duplicate-definition refusal in VDEF-AT sees every function at once, and the
\ cover check is asked once after the last one, when every value has had its
\ chance to be defined.
: VMEASURE1 ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id base:n :}
   f base VLAYOUT
   f VLIVENESS
   V-BLKS @ 0 ?do f i VBLOCK-RANGE loop
   N-VALS @ 0 ?do
      i DEF-AT NOPOS <>  i DEF-AT base >=  and if i VEXTEND-V then
   loop ;

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
\ the other, which is the same "no position at all" the tables above start from.

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

\ A ROUTINE THAT NEVER RETURNS TAKES ITS FRAME AND DOES NOT GIVE IT BACK, and
\ that is the whole of what changes. There is no block for a release to stand in
\ front of and nothing to release it for: every path leaves through the branch
\ that ends the process, so the machine stack pointer this routine moved is never
\ read again by anybody. The half that still holds is the opening one - the frame
\ is taken by the first operation of the block the caller enters, it names the
\ frame the contract declares, and no other operation of any block moves the
\ pointer - which is what a second frame inside the first would break and is
\ therefore still measured.
\
\ THE SAVED RETURN ADDRESS IS THE SAME STORY. A routine that calls saves the link
\ register before its first call because that call destroys it; it restores it
\ before returning, and this one does not return, so the save stands alone.
: VNO-RET-BRACKET-CK ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id want:n :}
   f 0 BLOCK-AT {: eb:IR-ID:ir-block-id :}
   eb 0 want FRAME-AT?
   eb 0 NOPOS VNO-SIZE
   f NO-RET VFRAME-BLOCKS-CK ;

: VNO-RET-SPILL-CK ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id want:n :}
   V-FRAME @ 0= if f VNO-FRAME exit then
   f want VNO-RET-BRACKET-CK ;

: VNO-RET-LINK-CK ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id want:n :}
   f 0 BLOCK-AT {: eb:IR-ID:ir-block-id :}
   eb OP-COUNT 2 < if E-A64RAV-CALL throw then
   f want VNO-RET-BRACKET-CK
   eb 1 true VLINK-AT? ;

: VNO-RET-FRAME-CK ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id want:n :}
   V-CALLS @ 0= if f want VNO-RET-SPILL-CK else f want VNO-RET-LINK-CK then
   f NO-RET VOWNER-CK ;

: VFRAME-CK ( IR-ID:ir-fun-id n n -- )
   {: f:IR-ID:ir-fun-id rb:n want:n :}
   rb NO-RET = if f want VNO-RET-FRAME-CK exit then
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

\ ---- what each slot of the caller's stack holds ------------------------------
\ THE SECOND DERIVATION. The selector decides that a value already lies in the
\ slot a store would write and builds no store, and that a value nothing reads
\ out of a register need never be loaded into one. This is that decision measured
\ again, from the module alone: two facts per slot, computed over the whole
\ function by the same kind of forward walk the selector runs and by code that
\ shares none of it.
\
\   IS THE SLOT DEFINED - has some path put a value in this cell and has nothing
\     destroyed it since. Meet is `and`, so a slot is defined only when it is
\     defined however control arrived. A call publishing a slot no store in front
\     of it wrote is refused unless this says the cell already holds something,
\     which is the whole of what an omitted store claims.
\
\   WHICH VALUE THE SLOT HOLDS - the machine value the cell provably equals, or
\     nothing. Two paths disagreeing means nothing, which only ever costs a
\     refusal this check would otherwise make. What it decides is the other
\     direction: a store writing the value the cell already holds, and a load of a
\     cell whose value is already in a register, are accesses the emission had no
\     reason to make and are refused.
\
\ WHAT THIS CANNOT DECIDE, AND WHOSE IT IS. Whether the value standing in a slot
\ is the value the PROGRAM meant to publish there is a statement about the module
\ the selector read, and this file is handed one module - the same gap the store
\ run always had (dot habu-prove-a-data-df458151, named at the head of the data
\ stack section above). What is decided here is that the emission is the one
\ canonical lowering for the module's own residency: every omission justified,
\ every access necessary.
\
\ A SLOT'S CONTENT IS NAMED BY A CELL AND THE THREE KINDS DO NOT COLLIDE. A
\ machine value is its own module-local ordinal, below VMAX; the routine's own
\ argument `i`, which no operation of the module defines, is VMAX + i; and the
\ slot `j` a call publishes back is counted past both from the call's own ordinal.
\ Nothing indexes by these numbers - they are only ever compared - so the naming
\ needs no table and no bound.
64 constant VDSLOTS                  \ slots the residency is tracked over
-1 constant VD-BOT                   \ nothing is known about this slot
-2 constant VD-TOP                   \ nothing has been said about it yet
0 constant VD-UNDEF
1 constant VD-DEF

here CELL 1- and CELL swap - CELL 1- and allot
create VD-VIN BMAX VDSLOTS * cells allot    \ the value each slot holds at a block's head
create VD-VOUT BMAX VDSLOTS * cells allot   \ and at its end
create VD-DIN BMAX VDSLOTS * cells allot    \ whether each slot is defined there
create VD-DOUT BMAX VDSLOTS * cells allot
create VD-VCUR VDSLOTS cells allot
create VD-DCUR VDSLOTS cells allot
create VD-VMEET VDSLOTS cells allot
create VD-DMEET VDSLOTS cells allot
variable VD-EL                       \ argument loads the entry sequence really built
variable VD-ES                       \ result stores the exit sequence really built
variable VD-P                        \ the position one of the scans below stands at
variable VD-J                        \ the declared place one of them stands at
variable VD-S
variable VD-PREV
variable VD-MOVED

\ ---- where the pointer stands, re-derived ------------------------------------
\ THE THIRD DERIVATION. Every number the four data-stack forms carry is a
\ DISTANCE from the data-stack pointer, and the pointer is not at the base of the
\ routine's window any more: src/compiler/native/select.f stands it wherever the
\ fewest adjustments are needed and writes every offset against that place. So
\ this file cannot read a slot off an attribute; it has to know where the pointer
\ IS at each point, and it works that out from the module and the contract alone.
\
\ WHICH IS ONE NUMBER, and that is a fact about the machine rather than a
\ simplification. The pointer is a register: it holds one value at a time, and a
\ body that stood in two places would need an adjustment on every edge between
\ them, which is more instructions and never fewer. So the routine stands at ONE
\ place; it is entered at 8*in, moved to that place by the entry form, moved to
\ the callee's base and back at each call, and moved to 8*out to return. The
\ entry form's own field is what says where: the caller left the pointer at 8*in
\ and that form moves it down by what it carries, so the place is the difference,
\ and every other rule below is measured from it.
variable VD-STAND                    \ where the body's data-stack pointer stands
0 VD-STAND !
variable VD-ENTRY                    \ where the caller leaves it: 8*in
0 VD-ENTRY !
variable VD-LEAVE                    \ where the caller expects it back: 8*out
0 VD-LEAVE !

\ AND WHY THE PLACE ITSELF IS RE-DERIVED AND NOT MERELY READ. A place that is
\ inside the bound and consistent with every offset written against it is still
\ the wrong place if some other place would have cost fewer instructions: the
\ adjustments a routine does not need are exactly the adjustments this whole
\ capability exists to delete, so a module carrying one is a module whose
\ lowering is not the canonical one - the same judgement this file already makes
\ about a store that writes what the cell holds and a load of a value already in
\ a register. The places a routine REQUIRES are decidable from the module: one
\ where the caller leaves the pointer, one where it expects it back, and two per
\ call site, which are the two distances that site carries. So the choice is
\ re-derived from those and the module is refused if it stands anywhere else.
\
\ AND THE COLLECTION HAS THE SAME SIZE AS THE SELECTOR'S SURVEY, and gives up in
\ the same way. src/compiler/native/select.f keeps the base for a routine with
\ more required places than its survey holds, because standing at the base is
\ always available and always correct; a routine past that size therefore has no
\ chosen place to re-derive, and this rule stands aside for it while every other
\ rule here still applies. The two sizes are one number twice for one reason: two
\ different ones would make this rule refuse the very modules the selector
\ decided not to place.
256 constant VDREQ-MAX               \ places one routine's collection holds

here CELL 1- and CELL swap - CELL 1- and allot
create VD-REQ VDREQ-MAX cells allot
variable VD-REQ-N
variable VD-REQ-OVER
variable VD-BEST
variable VD-BCOST

: VD-WINDOW? ( n -- bool )
   dup 0 < if drop false exit then
   VDSLOTS < ;

: VDV-IN ( n n -- n )
   {: b:n s:n :}
   s VD-WINDOW? 0= if VD-BOT exit then
   b VDSLOTS * s + cells VD-VIN + @ ;

: VDV-IN! ( n n n -- )
   {: v:n b:n s:n :}
   s VD-WINDOW? 0= if exit then
   v  b VDSLOTS * s + cells VD-VIN + ! ;

: VDD-IN ( n n -- n )
   {: b:n s:n :}
   s VD-WINDOW? 0= if VD-DEF exit then
   b VDSLOTS * s + cells VD-DIN + @ ;

: VDD-IN! ( n n n -- )
   {: v:n b:n s:n :}
   s VD-WINDOW? 0= if exit then
   v  b VDSLOTS * s + cells VD-DIN + ! ;

: VDV-OUT ( n n -- n )
   {: b:n s:n :}
   s VD-WINDOW? 0= if VD-BOT exit then
   b VDSLOTS * s + cells VD-VOUT + @ ;

: VDV-OUT! ( n n n -- )
   {: v:n b:n s:n :}
   s VD-WINDOW? 0= if exit then
   v  b VDSLOTS * s + cells VD-VOUT + ! ;

: VDD-OUT ( n n -- n )
   {: b:n s:n :}
   s VD-WINDOW? 0= if VD-DEF exit then
   b VDSLOTS * s + cells VD-DOUT + @ ;

: VDD-OUT! ( n n n -- )
   {: v:n b:n s:n :}
   s VD-WINDOW? 0= if exit then
   v  b VDSLOTS * s + cells VD-DOUT + ! ;

\ ---- the running map, and what one operation does to it ----------------------
: VDCUR<IN ( n -- )
   {: b:n :}
   VDSLOTS 0 ?do
      b i VDV-IN  i cells VD-VCUR + !
      b i VDD-IN  i cells VD-DCUR + !
   loop ;

: VDOUT<CUR ( n -- )
   {: b:n :}
   VDSLOTS 0 ?do
      i cells VD-VCUR + @  b i VDV-OUT!
      i cells VD-DCUR + @  b i VDD-OUT!
   loop ;

: VDV@ ( n -- n )
   dup VD-WINDOW? 0= if drop VD-BOT exit then
   cells VD-VCUR + @ ;

: VDD@ ( n -- n )
   dup VD-WINDOW? 0= if drop VD-DEF exit then
   cells VD-DCUR + @ ;

: VDPUT ( n n n -- )
   {: v:n d:n s:n :}
   s VD-WINDOW? 0= if exit then
   v s cells VD-VCUR + !
   d s cells VD-DCUR + ! ;

\ The cell one of those distances names. It is the distance plus where the
\ pointer stands, and it has to come out a whole cell at or above the base: a
\ distance that names a cell under the caller's window, or a cell boundary the
\ stack has not got, is not an access of this routine's stack at all.
: VDCELL ( n -- n )
   VD-STAND @ + {: off:n :}
   off 0 < if E-A64RAV-DSTACK throw then
   off A64IR:SLOT-WIDTH mod 0<> if E-A64RAV-DSTACK throw then
   off A64IR:SLOT-WIDTH / ;

\ And the reach of the field it is written in, which is the whole of what makes
\ the placement's freedom safe: over the pointer an access is the scaled unsigned
\ field, under it the unscaled signed one, and A64EFF answers both. It is asked
\ of the distance rather than of the cell, because the distance is what the
\ instruction holds.
: VDREACH-CK ( n -- )
   {: off:n :}
   off A64EFF:SLOT-BACK negate < if E-A64RAV-DSTACK throw then
   off A64IR:SLOT-WIDTH A64EFF:SLOT-REACH > if E-A64RAV-DSTACK throw then ;

: VDSLOT-CELL ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id DSLOT-OF {: off:n :}
   off VDREACH-CK
   off VDCELL ;

\ Is this content a value of the module - the only kind a register can already
\ hold, and therefore the only kind that makes an access unnecessary?
: VD-NAMED? ( n -- bool )
   dup 0 >= swap N-VALS @ < and ;

: VDARG ( n -- n )
   VMAX + ;

: VDCALLRES ( IR-ID:ir-op-id n -- n )
   {: id:IR-ID:ir-op-id j:n :}
   VMAX VDSLOTS +  id IR-ID:OP-LOCAL VDSLOTS * +  j + ;

\ Everything the callee could have written stops holding anything, and the slots
\ it takes back hold what it left there. The verifier reads the take-back count
\ off the call's own field, so a site that saved live values below the callee's
\ base is covered without this check having to know how many: those cells are
\ inside the take-back run and are defined, which is all an omitted store needs.
: VDCALL-XFER ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id DBACK-OF VDCELL {: back:n :}
   VDSLOTS 0 ?do VD-BOT VD-UNDEF i VDPUT loop
   back 0 ?do
      id i VDCALLRES  VD-DEF  i VDPUT
   loop ;

\ A store through an address the program computed may have reached a data-stack
\ cell - which is exactly what the dialect declares by putting the addressed
\ forms in the same space as the data-stack forms - so no cell provably holds a
\ named value afterwards. It cannot UNdefine one: a store leaves something
\ behind wherever it landed.
: VDCLOBBER? ( IR-ID:ir-op-id -- bool )
   {: id:IR-ID:ir-op-id :}
   id STORES? 0= if false exit then
   id DSTACK-TOUCH? 0= ;

: VDCLOBBER-XFER ( -- )
   VDSLOTS 0 ?do
      i VDD@  VD-BOT swap  i VDPUT
   loop ;

\ ---- one operation, measured -------------------------------------------------
\ The refusals are here rather than in a second walk because the map they judge
\ is the map at that operation, and re-deriving it twice would be two answers.
: VDLOAD-CK ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id VDSLOT-CELL {: s:n :}
   s VDD@ VD-DEF <> if E-A64RAV-DRES throw then
   s VDV@ VD-NAMED? if E-A64RAV-DKEEP throw then
   id 0 RESULT-AT SLOT {: k:n :}
   k USES-AT 0= if E-A64RAV-DKEEP throw then
   k VD-DEF s VDPUT ;

: VDSTORE-CK ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id VDSLOT-CELL {: s:n :}
   id 0 OPERAND-AT SLOT {: k:n :}
   s VDV@ k = if E-A64RAV-DKEEP throw then
   k VD-DEF s VDPUT ;

\ Every slot the branch publishes holds something. A store in front of it is one
\ way; the cell already holding what the callee is to read is the other, and this
\ is where that claim is held against the module.
\
\ AND THIS IS ALSO WHERE THE POINTER'S PLACE AT THE BRANCH IS JUDGED. Where the
\ pointer stands when the Bl is taken IS the callee's base - one past the last
\ cell the site publishes - so a site that entered the callee one cell too high
\ would be claiming a cell nothing has written, and this rule refuses it under its
\ own name rather than letting the callee read whatever was there.
: VDPUBLISH-CK ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id DBYTES-OF VDCELL 0 ?do
      i VDD@ VD-DEF <> if E-A64RAV-DRES throw then
   loop ;

\ The routine's own publication: the cells the convention says the caller will
\ read the results out of, at the moment the pointer moves over them.
: VDOUTS-CK ( A64EFF:placeseq n -- )
   {: outs:A64EFF:placeseq r:n :}
   r 0 ?do
      outs i A64EFF:SEQ-SLOT@ VDD@ VD-DEF <> if E-A64RAV-DRES throw then
   loop ;

: VDOP-XFER ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id DCALL? if id VDCALL-XFER exit then
   id DSLOT-OF NOSLOT <> if
      id STORES? if id VDSTORE-CK exit then
      id VDLOAD-CK exit
   then
   id VDCLOBBER? if VDCLOBBER-XFER then ;

\ ---- the fixpoint ------------------------------------------------------------
: VDMEET-V ( n n -- n )
   {: a:n b:n :}
   a VD-TOP = if b exit then
   b VD-TOP = if a exit then
   a b = if a exit then
   VD-BOT ;

: VDMEET-D ( n n -- n )
   {: a:n b:n :}
   a VD-TOP = if b exit then
   b VD-TOP = if a exit then
   a b = if a exit then
   VD-UNDEF ;

\ A value the branch hands to argument `i` is read as that argument on the way
\ in, which is what lets one cell keep its name round a loop whose two edges
\ carry two different values into it.
: VDXLATE ( IR-ID:ir-op-id IR-ID:ir-block-id n -- n )
   {: t:IR-ID:ir-op-id tb:IR-ID:ir-block-id v:n :}
   v VD-NAMED? 0= if v exit then
   tb ARG-COUNT {: k:n :}
   t OPERANDS-OF k <> if v exit then
   v
   k 0 ?do
      t i OPERAND-AT SLOT v = if
         drop  tb i ARG-AT SLOT  leave
      then
   loop ;

: VDMEET-EDGE ( IR-ID:ir-op-id IR-ID:ir-block-id n -- )
   {: t:IR-ID:ir-op-id tb:IR-ID:ir-block-id p:n :}
   VDSLOTS 0 ?do
      i cells VD-VMEET + @   t tb  p i VDV-OUT  VDXLATE   VDMEET-V
      i cells VD-VMEET + !
      i cells VD-DMEET + @   p i VDD-OUT   VDMEET-D
      i cells VD-DMEET + !
   loop ;

: VDEDGE? ( IR-ID:ir-op-id n -- bool )
   {: t:IR-ID:ir-op-id b:n :}
   false
   t SUCCS-OF 0 ?do
      t i SUCC-AT IR-ID:BLOCK-LOCAL b = if drop true leave then
   loop ;

: VDMEET-FROM ( IR-ID:ir-fun-id n n -- )
   {: f:IR-ID:ir-fun-id p:n b:n :}
   f p BLOCK-AT TERM-AT {: t:IR-ID:ir-op-id :}
   t b VDEDGE? 0= if exit then
   t  f b BLOCK-AT  p VDMEET-EDGE ;

: VDIN-SET? ( n n n n -- bool )
   {: v:n d:n b:n s:n :}
   false
   b s VDV-IN v <> if v b s VDV-IN! drop true then
   b s VDD-IN d <> if d b s VDD-IN! drop true then ;

: VDMEET-BLOCK ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   VDSLOTS 0 ?do
      VD-TOP i cells VD-VMEET + !
      VD-TOP i cells VD-DMEET + !
   loop
   V-BLKS @ 0 ?do  f i b VDMEET-FROM  loop
   VDSLOTS 0 ?do
      i cells VD-VMEET + @  i cells VD-DMEET + @  b i VDIN-SET?
      if 1 VD-MOVED ! then
   loop ;

: VDXFER-BLOCK ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   b VDCUR<IN
   f b BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk OP-COUNT 0 ?do  bk i OP-AT VDOP-XFER  loop
   b VDOUT<CUR ;

\ The entry map: the caller wrote the argument cells, the pointer move at the
\ head of the entry block is what makes them slots zero upwards, and nothing in
\ the module defines them - so they are named as the routine's own arguments.
: VDENTRY-IN ( n A64EFF:placeseq -- )
   {: a:n args:A64EFF:placeseq :}
   VDSLOTS 0 ?do
      VD-BOT 0 i VDV-IN!
      VD-UNDEF 0 i VDD-IN!
   loop
   a 0 ?do
      i VDARG  0  args i A64EFF:SEQ-SLOT@  VDV-IN!
      VD-DEF   0  args i A64EFF:SEQ-SLOT@  VDD-IN!
   loop ;

: VDIN-ANY ( n -- )
   {: b:n :}
   VDSLOTS 0 ?do
      VD-TOP b i VDV-IN!
      VD-TOP b i VDD-IN!
   loop ;

BMAX VDSLOTS * 4 * 2 + constant VD-ROUNDS

: VDRES-FIX ( IR-ID:ir-fun-id n A64EFF:placeseq -- )
   {: f:IR-ID:ir-fun-id a:n args:A64EFF:placeseq :}
   V-BLKS @ 1 ?do i VDIN-ANY loop
   a args VDENTRY-IN
   V-BLKS @ 0 ?do f i VDXFER-BLOCK loop
   0
   begin
      dup VD-ROUNDS >= if E-A64RAV-DRES throw then
      0 VD-MOVED !
      V-BLKS @ 1 ?do  f i VDMEET-BLOCK  loop
      V-BLKS @ 0 ?do  f i VDXFER-BLOCK  loop
      1+
      VD-MOVED @ 0=
   until
   drop ;

\ ---- the checked pass --------------------------------------------------------
\ The same walk once more with the refusals turned on. It is a second walk rather
\ than a flag on the first because the map has to have STOPPED moving before an
\ omission is judged against it: a round of the descent above is an answer that
\ may still fall.
: VDCK-OP ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id DCALL? if id VDPUBLISH-CK  id VDCALL-XFER exit then
   id DSLOT-OF NOSLOT <> if
      id STORES? if id VDSTORE-CK exit then
      id VDLOAD-CK exit
   then
   id VDCLOBBER? if VDCLOBBER-XFER then ;

: VDCK-BLOCK ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   b VDCUR<IN
   f b BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk OP-COUNT 0 ?do  bk i OP-AT VDCK-OP  loop ;

\ How far in front of the epilogue the moment being judged stands: the returning
\ form is judged where the pointer moves over the results, and the tail form at
\ the branch itself, which has no such move in front of it.
: VDPUB-BACK ( -- n )
   V-TAIL @ 0<> if 1 exit then
   2 ;

\ The exit publication is judged in its own block, at the position the shape
\ check already found it: the map there is the one the walk above rebuilt, and
\ the cells the convention names have to hold something at exactly that point.
: VDCK-EXIT ( IR-ID:ir-fun-id n n A64EFF:placeseq -- )
   {: f:IR-ID:ir-fun-id rb:n r:n outs:A64EFF:placeseq :}
   rb VDCUR<IN
   f rb BLOCK-AT {: xb:IR-ID:ir-block-id :}
   xb OP-COUNT PRO-N - VDPUB-BACK - {: pub:n :}
   pub 0 ?do  xb i OP-AT VDOP-XFER  loop
   outs r VDOUTS-CK ;

: VDRES-CK ( IR-ID:ir-fun-id n n n A64EFF:placeseq A64EFF:placeseq -- )
   {: f:IR-ID:ir-fun-id rb:n a:n r:n args:A64EFF:placeseq outs:A64EFF:placeseq :}
   f a args VDRES-FIX
   V-BLKS @ 0 ?do f i VDCK-BLOCK loop
   rb NO-RET = if exit then
   f rb r outs VDCK-EXIT ;

\ ---- the entry and exit sequences, re-derived --------------------------------
\ Which loads and stores the two sequences really carry is not fixed any more:
\ an argument no operation reads out of a register has no load and a result
\ already standing in the cell it publishes from has no store. What is fixed is
\ that the ones present name the declared places IN ORDER and name no other
\ place, so the run is a rising subsequence of the convention rather than a
\ prefix of it. The counts are recorded because the scan that accounts for every
\ data-stack position of the routine has to know where these two windows end.
: VDLOAD? ( IR-ID:ir-block-id n -- bool )
   {: bk:IR-ID:ir-block-id at:n :}
   bk at OP-AT {: id:IR-ID:ir-op-id :}
   id DSLOT-OF NOSLOT = if false exit then
   id STORES? 0= ;

: VDSTORE? ( IR-ID:ir-block-id n -- bool )
   {: bk:IR-ID:ir-block-id at:n :}
   bk at OP-AT {: id:IR-ID:ir-op-id :}
   id DSLOT-OF NOSLOT = if false exit then
   id STORES? ;

: VDSLOT-AT ( IR-ID:ir-block-id n -- n )
   OP-AT VDSLOT-CELL ;

: VDSEQ-FIND ( A64EFF:placeseq n n -- )
   {: seq:A64EFF:placeseq len:n s:n :}
   begin
      VD-J @ len >= if E-A64RAV-DSTACK throw then
      seq VD-J @ A64EFF:SEQ-SLOT@ s =
      VD-J @ 1+ VD-J !
   until ;

\ The entry form is where the pointer's place comes FROM. The caller left the
\ pointer one past the arguments, at 8*in; this form moves it down by what it
\ carries; so where the body stands is the difference, and every distance the
\ module holds afterwards is read against it. It is bounded as well as derived: a
\ place under the base names cells this routine does not own, and a place further
\ above the base than the unscaled field reaches would put an access of the base
\ itself out of reach - so a module standing outside [0, SLOT-BACK] is refused
\ here rather than at whichever access happened to notice.
: VDSTAND-AT ( IR-ID:ir-block-id n n -- )
   {: bk:IR-ID:ir-block-id at:n entry:n :}
   bk at OP-AT {: id:IR-ID:ir-op-id :}
   id DSLOT-OF NOSLOT <> if E-A64RAV-DSTACK throw then
   id DBYTES-OF NOSLOT = if E-A64RAV-DSTACK throw then
   entry id DBYTES-OF - {: stand:n :}
   stand 0 < if E-A64RAV-DSTACK throw then
   stand A64EFF:SLOT-BACK > if E-A64RAV-DSTACK throw then
   stand A64IR:SLOT-WIDTH mod 0<> if E-A64RAV-DSTACK throw then
   stand VD-STAND ! ;

: VDENTRY-CK ( IR-ID:ir-fun-id n A64EFF:placeseq -- )
   {: f:IR-ID:ir-fun-id a:n args:A64EFF:placeseq :}
   f 0 BLOCK-AT {: eb:IR-ID:ir-block-id :}
   eb OP-COUNT PRO-N 1+ < if E-A64RAV-DSTACK throw then
   eb PRO-N  a A64IR:SLOT-WIDTH *  VDSTAND-AT
   PRO-N 1+ VD-P !
   0 VD-J !
   0 VD-EL !
   begin
      VD-P @ eb OP-COUNT < if eb VD-P @ VDLOAD? else false then
   while
      args a  eb VD-P @ VDSLOT-AT  VDSEQ-FIND
      VD-P @ 1+ VD-P !
      VD-EL @ 1+ VD-EL !
   repeat ;

\ WHY WALKING BACK FROM THE PUBLICATION CANNOT MISREAD A CALL SITE'S STORES. A
\ site's store run is always followed by the branch itself, so the operation in
\ front of the pointer move that publishes the results is either one of these
\ stores or something that is not a data-stack store at all.
\
\ AND THE PUBLICATION IS WHERE THE RETURN IS JUDGED. The body stands where the
\ entry form put it, this form moves it up by what it carries, and it has to
\ arrive at 8*out - which is the place the caller will read the results from and
\ go on computing against. A routine that returns with the pointer anywhere else
\ hands its caller a stack whose top is not where the caller believes it is, so
\ the wanted distance is stated as one past the results LESS where the body
\ stands, and a module carrying any other one is refused.
: VDEXIT-CK ( IR-ID:ir-fun-id n n A64EFF:placeseq -- )
   {: f:IR-ID:ir-fun-id rb:n r:n outs:A64EFF:placeseq :}
   f rb BLOCK-AT {: xb:IR-ID:ir-block-id :}
   xb OP-COUNT PRO-N - {: n:n :}
   n 2 < if E-A64RAV-DSTACK throw then
   xb n 2 -  r A64IR:SLOT-WIDTH * VD-STAND @ -  DMOVE-AT?
   n 2 - VD-P !
   0 VD-ES !
   begin
      VD-P @ 0 > if xb VD-P @ 1- VDSTORE? else false then
   while
      VD-P @ 1- VD-P !
      VD-ES @ 1+ VD-ES !
   repeat
   0 VD-J !
   VD-ES @ 0 ?do
      outs r  xb  VD-P @ i +  VDSLOT-AT  VDSEQ-FIND
   loop ;

\ THE SAME WINDOW FOR A ROUTINE THAT LEAVES THROUGH A CALLEE, and the two
\ differences are the whole of what a tail branch is. There is no publication:
\ the pointer is not moved over the results here, because the callee is what will
\ move it, so the window ends at the terminator itself rather than one before it.
\ And where the pointer stands is not derived from a field this time, it is
\ DEMANDED: a tail branch enters the callee at the place the pointer is standing
\ at, and the callee will leave the pointer one past ITS results and return to
\ OUR caller - which reads its results at 8*out. A routine whose body stands
\ anywhere else hands its caller a stack top that is not where the caller
\ believes it is, and no instruction after the branch could put it right, so it
\ is refused here.
\
\ WHAT THE STORE RUN IN FRONT OF IT IS. The cells the callee will read its
\ arguments out of are the very cells this routine's caller will read its results
\ out of - that coincidence IS the tail call - so the run is measured against the
\ declared OUT places exactly as the publication's run is, in rising order and
\ naming no other place.
: VDTAIL-CK ( IR-ID:ir-fun-id n n A64EFF:placeseq -- )
   {: f:IR-ID:ir-fun-id rb:n r:n outs:A64EFF:placeseq :}
   f rb BLOCK-AT {: xb:IR-ID:ir-block-id :}
   xb OP-COUNT PRO-N - {: n:n :}
   n 1 < if E-A64RAV-DSTACK throw then
   xb  xb OP-COUNT 1-  OP-AT TAILBR? 0= if E-A64RAV-DSTACK throw then
   VD-STAND @  r A64IR:SLOT-WIDTH *  <> if E-A64RAV-DSTACK throw then
   n 1 - VD-P !
   0 VD-ES !
   begin
      VD-P @ 0 > if xb VD-P @ 1- VDSTORE? else false then
   while
      VD-P @ 1- VD-P !
      VD-ES @ 1+ VD-ES !
   repeat
   0 VD-J !
   VD-ES @ 0 ?do
      outs r  xb  VD-P @ i +  VDSLOT-AT  VDSEQ-FIND
   loop ;

\ Which positions of the block control leaves through belong to that window. The
\ WHICH OF THE TWO EXIT RUNS THIS ROUTINE HAS, AND THE THIRD ANSWER: NEITHER. A
\ routine that never returns publishes nothing, so there is no run to measure and
\ no place for it to require. Nothing is lost by leaving the measurement out:
\ VDCLEAN1 below accounts for every data-stack position of every block, so a
\ publication standing anywhere in such a routine is a position it can make
\ neither an entry window nor a site out of, and is refused there by name.
: VDLEAVE-CK ( IR-ID:ir-fun-id n n A64EFF:placeseq -- )
   {: f:IR-ID:ir-fun-id rb:n r:n outs:A64EFF:placeseq :}
   rb NO-RET = if exit then
   V-TAIL @ 0<> if f rb r outs VDTAIL-CK exit then
   f rb r outs VDEXIT-CK ;

\ tail form has no publication position of its own, so its window is the store
\ run alone; the returning form has the publication as well.
: VDTAIL-POS? ( n n -- bool )
   {: n:n at:n :}
   n 1 - PRO-N - {: q:n :}
   at q VD-ES @ - >= at q < and ;

: VDEXIT-POS? ( n n -- bool )
   {: n:n at:n :}
   V-TAIL @ 0<> if n at VDTAIL-POS? exit then
   n 2 - PRO-N - {: p:n :}
   at p = if true exit then
   at p VD-ES @ - >= at p < and ;

: VDPOS? ( n n n n -- bool )
   {: b:n at:n eb:n rb:n :}
   b eb = at VD-EL @ PRO-N + <= and if true exit then
   b rb = if rb VB-OPS at VDEXIT-POS? exit then
   false ;

\ ---- a call site, re-derived -------------------------------------------------
\ What the dialect lowers a call to, measured from the module rather than taken
\ from the selector: a run of data-stack stores naming slots the call publishes,
\ in rising order, the call, and a run of data-stack loads naming slots it takes
\ back, in rising order. Neither run has to be complete any more - an omitted
\ store is one the residency above has to justify and an omitted load is one
\ nothing reads - so what is measured is that every access present names a slot
\ inside its own run and that no two of them name the same slot or name them out
\ of order. A store the emission put after the branch, a slot named twice, and a
\ store naming a cell past the ones the call publishes are three different
\ disagreements and all three are refused here.
: DSTORE-RUN ( IR-ID:ir-block-id n -- n )
   {: bk:IR-ID:ir-block-id at:n :}
   bk OP-COUNT {: n:n :}
   -1 VD-PREV !
   0
   n at - 0 ?do
      bk at i + VDSTORE? 0= if leave then
      bk at i + VDSLOT-AT VD-S !
      VD-S @ VD-PREV @ <= if E-A64RAV-CALL throw then
      VD-S @ VD-PREV !
      drop i 1+
   loop ;

: DLOAD-RUN ( IR-ID:ir-block-id n -- n )
   {: bk:IR-ID:ir-block-id at:n :}
   bk OP-COUNT {: n:n :}
   -1 VD-PREV !
   0
   n at - 0 ?do
      bk at i + VDLOAD? 0= if leave then
      bk at i + VDSLOT-AT VD-S !
      VD-S @ VD-PREV @ <= if E-A64RAV-CALL throw then
      VD-S @ VD-PREV !
      drop i 1+
   loop ;

: VDRUN-BOUND ( IR-ID:ir-block-id n n n -- )
   {: bk:IR-ID:ir-block-id at:n k:n limit:n :}
   k 0 ?do
      bk at i + VDSLOT-AT limit >= if E-A64RAV-CALL throw then
   loop ;

\ THE CALL'S OWN ARITHMETIC, as far as it can be re-derived. A branch-with-link
\ leaves the pointer at the callee's RESULT base, which is its argument base less
\ what the callee takes and plus what it leaves - so the two places one site
\ carries differ by exactly the callee's net effect. For a call to another word
\ that effect is the callee's own declaration and this file is handed no callee,
\ so nothing here can check it, and the site is covered by the two run bounds and
\ by the publication rule instead. For a call to THIS routine the callee IS the
\ routine being measured: its net effect is 8*out less 8*in, and a site claiming
\ any other one is refused. Which of the two a site is, is read off the field that
\ names an address - a call carrying one enters somebody else - exactly as every
\ other reader here asks the operation rather than its opcode.
: VDNET-CK ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id VCALL-ENTRY NOSLOT <> if exit then
   id DBACK-OF  id DBYTES-OF -  {: net:n :}
   net  VD-LEAVE @ VD-ENTRY @ -  <> if E-A64RAV-CALL throw then ;

\ One place the routine requires, recorded as the re-derivation of the placement
\ needs it. A place required twice is recorded twice, because two points each pay
\ their own adjustment.
: VDREQ+ ( n -- )
   {: at:n :}
   VD-REQ-N @ VDREQ-MAX >= if 1 VD-REQ-OVER ! exit then
   at  VD-REQ-N @ cells VD-REQ + !
   VD-REQ-N @ 1+ VD-REQ-N ! ;

\ A TRAP SITE IS A CALL SITE WITH NOTHING TO TAKE BACK, and it is measured as
\ one: a run of data-stack stores naming slots below the base the callee is
\ entered at, and then the branch. What it does not have is the second half.
\ Control never comes back, so there is no load run to measure, no take-back
\ count to hold the site's arithmetic against, and nothing after the branch at
\ all - the trap IS the block's terminator, which is what the first line holds it
\ to. A trap carrying a take-back count would be a call staged under the wrong
\ form and is refused rather than measured as one.
: VTRAP-SITE ( IR-ID:ir-block-id n n -- n )
   {: bk:IR-ID:ir-block-id at:n cp:n :}
   cp bk OP-COUNT 1- <> if E-A64RAV-CALL throw then
   bk cp OP-AT {: id:IR-ID:ir-op-id :}
   id DBACK-OF NOSLOT <> if E-A64RAV-CALL throw then
   id DBYTES-OF VD-STAND @ + VDREQ+
   bk at  cp at -  id DBYTES-OF VDCELL  VDRUN-BOUND
   cp 1+ ;

: VCALL-SITE ( IR-ID:ir-block-id n -- n )
   {: bk:IR-ID:ir-block-id at:n :}
   bk at DSTORE-RUN {: g:n :}
   at g + {: cp:n :}
   cp bk OP-COUNT >= if E-A64RAV-CALL throw then
   bk cp OP-AT TRAP-AT? if bk at cp VTRAP-SITE exit then
   bk cp OP-AT {: id:IR-ID:ir-op-id :}
   id DCALL? 0= if E-A64RAV-CALL throw then
   id VDNET-CK
   id DBYTES-OF VD-STAND @ + VDREQ+
   id DBACK-OF VD-STAND @ + VDREQ+
   bk at g  id DBYTES-OF VDCELL  VDRUN-BOUND
   bk cp 1+ DLOAD-RUN {: b:n :}
   bk cp 1+ b  id DBACK-OF VDCELL  VDRUN-BOUND
   cp 1+ b + ;

\ Every position of every block that touches the caller's data stack is either
\ part of the routine's own entry or exit, or part of one call site. The scan is
\ forward and consumes a whole call site at a time, so a store left over between
\ two sites, or a call with no stores in front of it, stops at the first position
\ the scan cannot account for.
: VDCLEAN1 ( IR-ID:ir-fun-id n n n -- )
   {: f:IR-ID:ir-fun-id b:n eb:n rb:n :}
   f b BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk OP-COUNT {: n:n :}
   0 VD-AT !
   begin VD-AT @ n < while
      b VD-AT @ eb rb VDPOS? if
         VD-AT @ 1+ VD-AT !
      else
         bk VD-AT @ OP-AT DSTACK-TOUCH? if
            bk VD-AT @ VCALL-SITE VD-AT !
         else
            VD-AT @ 1+ VD-AT !
         then
      then
   repeat ;

\ ---- the placement, re-derived -----------------------------------------------
\ The same choice src/compiler/native/select.f makes, made again here from the
\ places this file collected while it walked the module: fewest adjustments wins,
\ a place outside the bound is not a candidate, the base is the incumbent because
\ it is where the pass stood before there was a choice, and a tie goes to the
\ lower place so the answer does not depend on the order the walk found them in.
: VDPLACE-COST ( n -- n )
   {: c:n :}
   0
   VD-REQ-N @ 0 ?do
      i cells VD-REQ + @ c <> if 1+ then
   loop ;

: VDPLACE-OK? ( n -- bool )
   {: c:n :}
   c 0 >=  c A64EFF:SLOT-BACK <=  and ;

: VDPLACE-BETTER? ( n n -- bool )
   {: c:n k:n :}
   k VD-BCOST @ < if true exit then
   k VD-BCOST @ =  c VD-BEST @ <  and ;

: VDPLACE-TRY ( n -- )
   {: c:n :}
   c VDPLACE-OK? 0= if exit then
   c VDPLACE-COST {: k:n :}
   c k VDPLACE-BETTER? 0= if exit then
   c VD-BEST !
   k VD-BCOST ! ;

: VDPLACE-CK ( -- )
   VD-REQ-OVER @ 0<> if exit then
   0 VD-BEST !
   0 VDPLACE-COST VD-BCOST !
   VD-REQ-N @ 0 ?do  i cells VD-REQ + @ VDPLACE-TRY  loop
   VD-BEST @ VD-STAND @ <> if E-A64RAV-DSTACK throw then ;

\ WHICH OF THE TWO SHAPES THIS MODULE HAS TO HAVE IS THE CONTRACT'S DECLARATION.
\ A register-convention routine touches the caller's stack nowhere; a data-stack
\ one has an entry run, an exit run and a call site's runs and nothing else. The
\ counts below are how LONG those runs are and they can both be zero: a ( -- )
\ word entered through the data stack takes the pointer by nothing and publishes
\ nothing, which is two operations that emit no instruction and are still the
\ shape this measures.
: VDSTACK-CK ( IR-ID:ir-fun-id n A64EFF:placeseq A64EFF:placeseq -- )
   {: f:IR-ID:ir-fun-id rb:n args:A64EFF:placeseq outs:A64EFF:placeseq :}
   args A64EFF:SEQ-SLOTS {: a:n :}
   outs A64EFF:SEQ-SLOTS {: r:n :}
   V-DSTACK @ 0= if
      V-BLKS @ 0 ?do f i BLOCK-AT VNO-DSTACK loop
      exit
   then
   a A64IR:SLOT-WIDTH * VD-ENTRY !
   r A64IR:SLOT-WIDTH * VD-LEAVE !
   0 VD-REQ-N !
   0 VD-REQ-OVER !
   VD-ENTRY @ VDREQ+
   rb NO-RET <> if VD-LEAVE @ VDREQ+ then
   f a args VDENTRY-CK
   f rb r outs VDLEAVE-CK
   V-BLKS @ 0 ?do f i 0 rb VDCLEAN1 loop
   VDPLACE-CK
   f rb a r args outs VDRES-CK ;

\ ---- the contract's control and the module's terminator ----------------------
\ The contract says how control leaves this routine and the module shows it, and
\ the two are held against each other here rather than either being believed. A
\ contract declaring a return over a module that branches away would describe a
\ routine whose caller is never come back to; a contract declaring a tail call
\ over a module that returns would leave a frame reserved and a link saved for a
\ branch that is not there. And a tail branch anywhere but at the end of the
\ block control leaves through is a routine abandoned in the middle of itself:
\ every other block would be unreachable code the layout still writes.
: VTAIL1 ( IR-ID:ir-fun-id n n -- )
   {: f:IR-ID:ir-fun-id b:n rb:n :}
   f b BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk OP-COUNT 0 ?do
      bk i OP-AT TAILBR? if
         V-TAIL @ 0= if E-A64RAV-SHAPE throw then
         b rb <> if E-A64RAV-SHAPE throw then
         i bk OP-COUNT 1- <> if E-A64RAV-SHAPE throw then
      then
   loop ;

\ A ROUTINE THAT NEVER RETURNS LEAVES THROUGH NO CALLEE EITHER. A tail branch is
\ this routine returning THROUGH somebody else - the callee publishes into the
\ very cells this routine's caller reads - so a routine with no block that hands
\ its caller anything has no place for one. VTAIL1 above already refuses one
\ wherever it stands, because no block is the block control leaves through; what
\ is left is the contract, and a contract declaring a tail call over such a
\ module is the same mismatch the returning arm names.
: VNO-TAIL-CK ( -- )
   V-TAIL @ 0<> if E-A64RAV-SHAPE throw then ;

: VTAIL-CK ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id rb:n :}
   V-BLKS @ 0 ?do f i rb VTAIL1 loop
   rb NO-RET = if VNO-TAIL-CK exit then
   f rb BLOCK-AT TERM-AT TAILBR? 0= if VNO-TAIL-CK exit then
   V-TAIL @ 0= if E-A64RAV-SHAPE throw then ;

\ ---- the whole re-derivation -------------------------------------------------
: VBLOCK-CKS ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   V-BLKS @ 0 ?do
      f i BLOCK-AT TIE-CK
      f i BLOCK-AT FLOW-CK
   loop ;

\ The call rule over the whole linear order. A live range is measured in global
\ positions, so the operation index inside its block is turned into one before
\ the rule is asked.
: VCLOB-BLOCK ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   f b BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk OP-COUNT 0 ?do
      bk i OP-AT  b i VOP-POS  CLOB-AT
   loop ;

: VCLOB-CK ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   V-BLKS @ 0 ?do f i VCLOB-BLOCK loop ;

\ The checks that are about VALUES, asked once for the module after every
\ function has been measured onto the number line. Each of them sweeps the value
\ table, and the value table is the module's, so asking them per function would
\ either ask about values the function does not hold or ask the same question as
\ many times as there are functions.
: VALUE-CKS ( -- )
   COVER-CK
   INTERVAL-CK
   CLASS-CK
   REGISTER-CK
   OVERLAP-CK ;

\ The checks that are about one FUNCTION's structure: its blocks, its edges, its
\ frame, the registers its arguments arrive in and its results leave in. They read
\ the block tables, which hold one function at a time, so each is asked with that
\ function's layout restored.
: VERIFY ( IR-ID:ir-fun-id n A64EFF:placeseq A64EFF:placeseq n n n -- )
   {: f:IR-ID:ir-fun-id rb:n args:A64EFF:placeseq outs:A64EFF:placeseq frame:n
      lo:n hi:n :}
   f VANY-FRAME
   f lo hi ORDER-CK
   f VEDGE-CK
   f rb VTAIL-CK
   f rb frame VFRAME-CK
   f VBLOCK-CKS
   f 0 BLOCK-AT args ARG-CK
   rb NO-RET <> if f rb BLOCK-AT outs OUT-CK then
   f rb args outs VDSTACK-CK
   f VCLOB-CK ;

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

: WALK ( IR-BUILD:module A64EFF:gprs A64EFF:fprs A64EFF:placeseq A64EFF:placeseq n -- )
   {: m:IR-BUILD:module pool:A64EFF:gprs fpool:A64EFF:fprs
      args:A64EFF:placeseq outs:A64EFF:placeseq frame:n :}
   ST-NONE ST !
   BND-TAKE
   m BND-MODULE-CK
   STATE-CK
   m MODULE-CK
   pool 0 V-POOL !
   fpool 0 V-FPOOL !
   pool fpool CONTRACT-CK
   m VIEWS!
   VALS-N!
   TABLES-CLEAR
   FUNS-CK {: nf:n :}
   0
   nf 0 ?do
      dup i cells F-VB + !
      i FUN-AT over VMEASURE1
      drop V-AT @
   loop
   nf cells F-VB + !
   VALUE-CKS
   nf 0 ?do
      i FUN-AT {: f:IR-ID:ir-fun-id :}
      f  i cells F-VB + @  VLAYOUT
      f VLIVENESS
      f  f RET-ORD  args outs frame
      i cells F-VB + @  i 1 + cells F-VB + @  VERIFY
   loop ;

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
   c b A64IR:KEY-ENTRY  0 BND-ENTRY !
   c b A64IR:KEY-TRAP-ENTRY 0 BND-TRAP !
   BOUND-YES BND-MODE ! ;

\ ---- the check ---------------------------------------------------------------
\ Accept the sealed allocation as a true assignment for this module under this
\ routine contract, or refuse it by name. Nothing is answered until this returns.
: ACCEPT ( IR-BUILD:module A64EFF:routine -- )
   A64EFF:VALIDATE A64EFF-ROUTINE:UNMAKE
   {: cv:A64EFF:conv gi:A64EFF:placeseq gr:A64EFF:placeseq gc:A64EFF:gprs
      fi:A64EFF:fprs fr:A64EFF:fprs fc:A64EFF:fprs
      z:A64EFF:nzcv l:A64EFF:link ct:A64EFF:control
      t:A64EFF:traits size:n delta:n :}
   cv gi gr gc fi fr fc z l ct t size delta A64EFF-ROUTINE:MAKE
   A64EFF:GPR-WRITABLE {: pool:A64EFF:gprs :}
   cv gi gr gc fi fr fc z l ct t size delta A64EFF-ROUTINE:MAKE
   A64EFF:FPR-WRITABLE {: fpool:A64EFF:fprs :}
   t A64EFF:T-CALL A64EFF:TRAITS-HAS? if 1 else 0 then V-CALLS !
   ct A64EFF-CONTROL:TAIL-CALL A64EFF-CONTROL:EQ if 1 else 0 then V-TAIL !
   cv A64EFF-CONV:DSTACK A64EFF-CONV:EQ if 1 else 0 then V-DSTACK !
   t A64FRAME:SPILL-BASE V-BASE !
   pool fpool gi gr size WALK
   FUNS-CK 0 ?do
      i FUN-AT
      cv gi gr gc fi fr fc z l ct t size delta A64EFF-ROUTINE:MAKE
      SLOT-CK
   loop
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

\ Which register FILE this value lives in. The emitter asks after it has asked
\ for the register, because the number alone does not say which file it names -
\ d3 and x3 are two registers and both are number three - and what a routine
\ destroys has to be counted per file.
: FLOATING? ( n -- bool )
   FRESH-CK
   dup 0 < over N-VALS @ >= or if E-A64RAV-COVER throw then
   dup REGGED? 0= if E-A64RAV-CLASS throw then
   F-FPR IN-FILE? ;

\ ---- what the accepted allocation says this routine destroys ------------------
\ THE DERIVATION, AS ONE STATEMENT. The general registers a routine emitted from
\ this allocation writes, and that a CALLER could be holding a value in, are
\ exactly the registers this allocation assigns to its values.
\
\ THE ARGUMENT IS THE EMITTER'S, READ OFF ITS SOURCE. Every general register that
\ reaches an instruction in src/compiler/native/emit.f comes from one of four
\ places and no other: REG-OF, which is A64RAV:REG@ and answers only this
\ allocation's claims; A64EFF:LINK-GPR, the link register the prologue saves and
\ the epilogue restores; A64EFF:SP-GPR, operand 31, which the frame accesses name
\ as the stack pointer; and A64EFF:DSTACK-GPR, the register the running engine
\ keeps its data-stack pointer in, which the entry, the exit and every call site
\ step. The last three are members of the reserved set A64EFF's GPR-MASK
\ excludes - which since CG-13 is the target's x18/x30/31 plus EVERY register
\ the running engine occupies (src/habu/layout.f ENGINE-GPR:MASK - the
\ data-stack pointer and, beside it, DATA/RBASE, DBASE, NDICT and CP, none of
\ which reaches an emitted instruction at all) - so no routine contract can
\ hand any of them to an allocator and no caller can be holding a value in
\ one - which is why they are not in the answer here rather than being
\ subtracted from it.
\
\ WHAT A CALLER MAY THEREFORE RELY ON. The data-stack pointer is not preserved
\ and is not clobbered either: a routine moves it down over its arguments and up
\ over its results, which is the convention every call site already accounts for
\ in the two byte counts it carries. The link register is written at every call
\ site by the branch itself, whatever the callee does, so it is the CALLER's
\ business and not a fact about any callee. Both are outside this answer for the
\ same reason: no value of any routine ever lives in them.
\
\ AND THIS IS ONE AUTHORITY AND NOT TWO. It is derived from the assignment this
\ file ACCEPTED, so a register that reaches an instruction without being in it
\ would have had to reach it without being a claim - which is what the emitter
\ holds its own count of written registers against before it seals.
\ Both answers are counted per FILE and not per class, which is what makes them
\ stay right when one file holds two classes: what a caller can be holding in x3
\ is decided by everything this routine puts in the general file, whatever kind
\ of value each one is.
: GPR-WRITTEN ( -- A64EFF:gprs )
   FRESH-CK
   0
   N-VALS @ 0 ?do
      i F-GPR IN-FILE? if
         1  i A64RA:CLAIM@  lshift or
      then
   loop
   A64EFF:GPR-SET ;

: FPR-WRITTEN ( -- A64EFF:fprs )
   FRESH-CK
   0
   N-VALS @ 0 ?do
      i F-FPR IN-FILE? if
         1  i A64RA:CLAIM@  lshift or
      then
   loop
   A64EFF:FPR-SET ;

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

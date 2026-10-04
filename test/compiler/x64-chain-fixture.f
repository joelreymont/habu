\ x64-chain.f - the x86-64 pass chain, driven the way the native driver drives
\ it: through the rows src/arch/x86-64/passes.f (X64PASS) installs in
\ src/compiler/native/backend.f, reached by the architecture each case's own
\ target contract resolves to. No case names X64PASS, because the driver does
\ not either.
\
\ WHAT THIS SUITE MEASURES THAT test/compiler/x64-regalloc.f CANNOT. That suite
\ selects and allocates one module and stops at the allocator's PLAN: a routine
\ whose plan is not empty is left as a plan. Here the plan becomes operations.
\ The shared spill pass (src/compiler/native/spill.f, A64SPILL) rebuilds the
\ module with this dialect's own reserve, stores and loads - it is told
\ X64IR:LOWERING and X64IR:ENSURE-NAMED and names no machine itself - and the
\ rebuilt module is allocated again and accepted by the validator. The stores,
\ their frame slots and the reserve's frame size are read back off the module
\ that comes out, by NAME, through the same frozen-module cursor the pass reads.
\
\ PRUNE IS A PASS-THROUGH HERE: src/compiler/native/prune.f rewrites nothing on
\ the corpus and this machine has no prune pass, so the row answers the module
\ it was given, and one case pins that by identity. EVERY ROW OF THIS BACKEND IS
\ NOW FILLED: the placed case runs the driver's whole order - declare, select,
\ prune, fixpoint, emit at a named slot, retire - and reads the sealed image from
\ its owned artifact, which says the x86-64 chain is reachable end to end
\ from src/compiler/native/compiler.f.
\
\ WHAT PUBLICATION IS HANDED. The emit row states a sealed emission that the
\ context copies to NART before the backend retires. The row cases read the
\ image, the function starts and both kinds of site from that copy. Nothing
\ commits here: the publisher writes through this engine's own rows, which take
\ whole ARM64 words, so the x86-64 commit is the x86-64 engine's.
\
\ ONE FIXTURE PER CONTEXT, and scoped session work releases and retires the
\ passes on return or throw: a context that dies holding them
\ gives its registry slots back only when a live enclosing one leaves normally
\ (src/compiler/ir/context.f, the note on stale handles).

require lib/test.f
require lib/string.f
require src/compiler/ir/id.f
require src/compiler/ir/symbol.f
require src/compiler/ir/build.f
require src/compiler/native/backend.f
require src/compiler/native/emission.f
require src/compiler/native/publish.f
require src/compiler/session/emission.f
require src/compiler/native/frozen.f
require src/compiler/native/hir.f
require src/compiler/native/x64ir.f
require src/compiler/native/regalloc.f
require src/compiler/native/regalloc-verify.f
require src/arch/x86-64/abi.f
require src/arch/x86-64/passes.f

package X64CHAIN-TEST
private

\ ---- the binding every case compiles under -----------------------------------
\ A linux x86-64 contract whose integer overflow wraps, which is what makes the
\ driver's row resolve to this backend and nothing else.
: WBND ( -- CBIND:binding )
   CTARGET-ARCH:X86-64 CTARGET-ABI:SYSV-AMD64 CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64
   CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH CTARGET:CONTRACT
   CNUM-OVERFLOW:WRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY
   CBIND:BIND ;

\ ---- the fixture's source text -----------------------------------------------
\ One text stands behind every fixture, so each span a fixture attaches is a real
\ byte range in bytes the module has really registered.
create TXT
   58 c, 32 c, 76 c, 69 c, 65 c, 70 c, 32 c, 45 c,            \ ": LEAF -"
   32 c, 100 c, 117 c, 112 c, 32 c, 43 c, 32 c, 59 c,         \ " dup + ;"
16 constant TXT-N

2 constant NAME-ST                   \ the defined name inside TXT
4 constant NAME-LN
0 constant OPEN-ST                   \ the opening `:`
1 constant OPEN-LN
7 constant BODY-ST                   \ the body word
1 constant BODY-LN
15 constant CLOSE-ST                 \ the closing `;`
1 constant CLOSE-LN

\ ---- the module a fixture builds into ----------------------------------------
1 TYPED-BUFFER W-CTX IR-CTX:ctx
1 TYPED-BUFFER W-BLD IR-BUILD:builder
1 TYPED-BUFFER W-SRC IR-ID:ir-source-id
TYPED-VARIABLE W-SESSION NSESSION:session
TYPED-VARIABLE W-ART NART:emission

: CC ( -- IR-CTX:ctx )               0 W-CTX @ ;
: BB ( -- IR-BUILD:builder )         0 W-BLD @ ;
: SS ( -- IR-ID:ir-source-id )       0 W-SRC @ ;
: NS ( -- NSESSION:session )         W-SESSION @ ;

: BAD-OFFSET ( -- )
   W-ART @ 2 NART:FUNCTION-OFFSET@ drop ;

: PUBLISH-FOREIGN ( -- )
   W-ART @ NPUB:PUBLISH-PENDING ;

: CLEAN-WORK ( -- )
   NS NBACK:RELEASE
   NS NBACK:RETIRE ;

: CASE-WORK ( R IR-CTX:ctx [ R IR-CTX:ctx -- S ] NSESSION:session -- S )
   W-SESSION !
   [: CLEAN-WORK ;] finally ;

: CASE-CONTEXT ( R NLEASE:lease [ R IR-CTX:ctx -- S ] IR-CTX:ctx -- S )
   {: l:NLEASE:lease body c:IR-CTX:ctx :}
   c body c l NSESSION:NEW [: CASE-WORK ;] NSESSION:WITH-WORK ;

: CASE-LEASE ( R [ R IR-CTX:ctx -- S ] NLEASE:lease -- S )
   {: l:NLEASE:lease :}
   l swap WBND [: CASE-CONTEXT ;] IR-CTX:WITH-CONTEXT ;

public
: SESSION ( -- NSESSION:session ) NS ;

: WITH-CASE ( R [ R IR-CTX:ctx -- S ] -- S )
   [: CASE-LEASE ;] NLEASE:WITH ;
private

: SPN ( n n -- IR-SOURCE:span )
   {: st:n ln:n :}
   BB SS st ln IR-BUILD:ADD-SPAN ;

: CELLT ( -- IR-ID:ir-type-id )
   CC BB IR--TYPE-WIDTH:W64 IR--TYPE-SIGN:SIGNED IR-BUILD:INTERN-INT ;

: HIR-MOD ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   IR-BUILD:PLAN-BEGIN
   IR-BUILD:PLAN-DEFAULT
   c HIR:NEW-BUILDER {: b:IR-BUILD:builder :}
   c b HIR:REGISTER
   c 0 W-CTX !
   b 0 W-BLD !
   c b TXT TXT-N IR-BUILD:ADD-SOURCE 0 W-SRC ! ;

\ ---- staging one source operation --------------------------------------------
: OPEN-OP ( HIR:opcode n n -- )
   {: o:HIR:opcode st:n ln:n :}
   CC BB  CC BB o HIR:OPCODE  IR-BUILD:BEGIN-OP
   CC BB  st ln SPN  IR-BUILD:SET-OP-SPAN ;

: CLOSE-VALUE ( -- IR-ID:ir-value-id )
   CC BB IR-BUILD:END-OP {: id:IR-ID:ir-op-id :}
   CC BB id 0 IR-BUILD:OP-RESULT@ ;

: BINOP ( HIR:opcode IR-ID:ir-value-id IR-ID:ir-value-id -- IR-ID:ir-value-id )
   {: o:HIR:opcode x:IR-ID:ir-value-id y:IR-ID:ir-value-id :}
   o BODY-ST BODY-LN OPEN-OP
   CC BB x IR-BUILD:ADD-OPERAND
   CC BB y IR-BUILD:ADD-OPERAND
   CC BB CELLT IR-BUILD:ADD-RESULT
   CLOSE-VALUE ;

: RET1 ( IR-ID:ir-value-id -- )
   {: v:IR-ID:ir-value-id :}
   HIR-OPCODE:RETURN CLOSE-ST CLOSE-LN OPEN-OP
   CC BB v IR-BUILD:ADD-OPERAND
   CC BB IR-BUILD:END-OP drop ;

\ ---- staging the function ----------------------------------------------------
: SIGN ( n n -- IR-ID:ir-type-id )
   {: in:n out:n :}
   CELLT {: t:IR-ID:ir-type-id :}
   IR-TYPE:FN-BEGIN
   in 0 ?do t IR-TYPE:FN-PARAM loop
   out 0 ?do t IR-TYPE:FN-RESULT loop
   CC BB IR-BUILD:INTERN-CODE-REF ;

\ A module's function table admits one function per symbol (E-IR-FUN-DUP), so a
\ module of two names them apart.
: OPEN-FUN-NAMED ( n n ptr u8 n -- )
   {: in:n out:n a:ptr u:n :}
   CC BB  CC BB a u IR-BUILD:INTERN-SYMBOL  IR-BUILD:BEGIN-FUN
   CC BB  in out SIGN  IR-BUILD:SET-SIGNATURE
   CC BB IR--FUN-LINKAGE:DEFINED IR-BUILD:SET-LINKAGE
   CC BB IR--FUN-VISIBILITY:EXPORTED IR-BUILD:SET-VISIBILITY
   CC BB IR--FUN-CONVENTION:HABU IR-BUILD:SET-CONVENTION
   CC BB  NAME-ST NAME-LN SPN  IR-BUILD:SET-FUN-SPAN
   CC BB IR-BUILD:BEGIN-BLOCK
   CC BB  OPEN-ST OPEN-LN SPN  IR-BUILD:SET-BLOCK-SPAN ;

: OPEN-FUN ( n n -- )
   s" LEAF" OPEN-FUN-NAMED ;

: ARG+ ( -- IR-ID:ir-value-id )
   CC BB CELLT IR-BUILD:ADD-BLOCK-ARG ;

: CLOSE-FUN ( -- )
   CC BB IR-BUILD:END-BLOCK drop
   CC BB IR-BUILD:END-FUN drop ;

\ ---- the shapes --------------------------------------------------------------
\ The values of the pressure shape, held while the body that reads them is
\ staged: a local binds once, and there are more of them than a definition
\ would want names for.
16 TYPED-BUFFER ARGV IR-ID:ir-value-id

\ `: LEAF ( a -- n ) a a + a a + ... twelve times ... and then sum the twelve`.
\ All twelve doublings are live at once where the last of them is made, and this
\ machine has nine allocatable registers, so the values read furthest away lose
\ theirs. The interface is ONE argument because the data-stack entry transfer
\ takes every argument's bytes in a single operation: a routine of twelve would
\ need twelve registers at that one instant and no spill can free one
\ (E-A64RA-POOL). Pressure here is over values the body makes one at a time.
\
\ The doubling is two-address and its operand is live after it, so selection
\ copies the argument before each one - the copies are short-lived and the
\ twelve results are not.
12 constant PRESSURE-N

: DOUBLINGS ( IR-ID:ir-value-id -- )
   {: a:IR-ID:ir-value-id :}
   PRESSURE-N 0 ?do
      HIR-OPCODE:ADD a a BINOP  i ARGV !
   loop ;

\ Their sum, `24 a *`, read in the order they were made.
: DOUBLINGS-SUM ( -- IR-ID:ir-value-id )
   HIR-OPCODE:ADD  0 ARGV @  1 ARGV @  BINOP
   PRESSURE-N 2 ?do
      HIR-OPCODE:ADD swap  i ARGV @  BINOP
   loop ;

: BUILD-PRESSURE ( -- )
   1 1 OPEN-FUN
   ARG+ DOUBLINGS
   DOUBLINGS-SUM RET1
   CLOSE-FUN ;

\ ---- the same pressure held across control ----------------------------------
: BLOCK-ID ( n -- IR-ID:ir-block-id )
   {: k:n :}
   BB IR-BUILD:MODULE-KEY k IR-ID:PACK-BLOCK ;

: BLOCK+ ( -- )
   CC BB IR-BUILD:END-BLOCK drop
   CC BB IR-BUILD:BEGIN-BLOCK
   CC BB  OPEN-ST OPEN-LN SPN  IR-BUILD:SET-BLOCK-SPAN ;

: BRZ2 ( IR-ID:ir-value-id n n -- )
   {: f:IR-ID:ir-value-id z:n o:n :}
   HIR-OPCODE:BRZ CLOSE-ST CLOSE-LN OPEN-OP
   CC BB f IR-BUILD:ADD-OPERAND
   CC BB z BLOCK-ID IR-BUILD:ADD-SUCCESSOR
   CC BB o BLOCK-ID IR-BUILD:ADD-SUCCESSOR
   CC BB IR-BUILD:END-OP drop ;

: BR1 ( IR-ID:ir-value-id n -- )
   {: v:IR-ID:ir-value-id t:n :}
   HIR-OPCODE:BR CLOSE-ST CLOSE-LN OPEN-OP
   CC BB v IR-BUILD:ADD-OPERAND
   CC BB t BLOCK-ID IR-BUILD:ADD-SUCCESSOR
   CC BB IR-BUILD:END-OP drop ;

: BR2 ( IR-ID:ir-value-id IR-ID:ir-value-id n -- )
   {: v:IR-ID:ir-value-id w:IR-ID:ir-value-id t:n :}
   HIR-OPCODE:BR CLOSE-ST CLOSE-LN OPEN-OP
   CC BB v IR-BUILD:ADD-OPERAND
   CC BB w IR-BUILD:ADD-OPERAND
   CC BB t BLOCK-ID IR-BUILD:ADD-SUCCESSOR
   CC BB IR-BUILD:END-OP drop ;

: CONSTOP ( n -- IR-ID:ir-value-id )
   {: v:n :}
   HIR-OPCODE:CONST BODY-ST BODY-LN OPEN-OP
   CC BB CELLT IR-BUILD:ADD-RESULT
   CC BB  CC BB HIR:KEY-VALUE  CC BB v IR-BUILD:INTERN-INT-ATTR
   IR-BUILD:ADD-ATTR
   CC BB  CC BB HIR:KEY-ADDR  CC BB HIR:ADDR-NONE HIR:ADDR-ATTR
   IR-BUILD:ADD-ATTR
   CLOSE-VALUE ;

\ `( a b -- n )`: the twelve doublings, then `b` branches, and each arm sums
\ them - so every one put away before the branch is brought back on both sides.
\ The arms join at the return: `24 a *` where `b` is zero, `24 a * b +` where it
\ is not.
: BUILD-PBRANCH ( -- )
   2 1 OPEN-FUN
   ARG+ {: a:IR-ID:ir-value-id :}
   ARG+ {: b:IR-ID:ir-value-id :}
   a DOUBLINGS
   b 1 2 BRZ2
   BLOCK+
   DOUBLINGS-SUM 3 BR1
   BLOCK+
   HIR-OPCODE:ADD DOUBLINGS-SUM b BINOP 3 BR1
   BLOCK+
   ARG+ RET1
   CLOSE-FUN ;

\ `( a n -- r )`: the twelve doublings, then a loop that turns `n` times, adding
\ their sum to an accumulator that starts at `a` - `a 24 a * n * +`. All twelve
\ are live around the backedge beside the count and the accumulator, so what
\ the frame holds is brought back on every turn. The loop leaves the way
\ src/compiler/native/elaborate.f DO-WHILE lays `begin ... while ... repeat` out:
\ `brz` to a stub that hands the accumulator to the exit block as its argument,
\ laid before the body.
: BUILD-PLOOP ( -- )
   2 1 OPEN-FUN
   ARG+ {: a:IR-ID:ir-value-id :}
   ARG+ {: n:IR-ID:ir-value-id :}
   a DOUBLINGS
   n a 1 BR2
   BLOCK+
   ARG+ {: c:IR-ID:ir-value-id :}
   ARG+ {: acc:IR-ID:ir-value-id :}
   c 2 3 BRZ2
   BLOCK+
   acc 4 BR1
   BLOCK+
   HIR-OPCODE:ADD acc DOUBLINGS-SUM BINOP {: acc2:IR-ID:ir-value-id :}
   HIR-OPCODE:SUB c 1 CONSTOP BINOP  acc2  1 BR2
   BLOCK+
   ARG+ RET1
   CLOSE-FUN ;

: MEMT ( -- IR-ID:ir-type-id )
   CC BB HIR:MEM-TYPE ;

: MEM0 ( -- IR-ID:ir-value-id )
   HIR-OPCODE:MEM BODY-ST BODY-LN OPEN-OP
   CC BB MEMT IR-BUILD:ADD-RESULT
   CLOSE-VALUE ;

: INT-ATTR+ ( IR-ID:ir-symbol-id n -- )
   {: k:IR-ID:ir-symbol-id v:n :}
   CC BB  k  CC BB v IR-BUILD:INTERN-INT-ATTR  IR-BUILD:ADD-ATTR ;

\ One argument in and one answer out, with nothing else on the data stack.
: WORDCALL1 ( IR-ID:ir-value-id IR-ID:ir-value-id n -- IR-ID:ir-value-id )
   {: tok:IR-ID:ir-value-id arg:IR-ID:ir-value-id e:n :}
   HIR-OPCODE:WORDCALL BODY-ST BODY-LN OPEN-OP
   CC BB tok IR-BUILD:ADD-OPERAND
   CC BB arg IR-BUILD:ADD-OPERAND
   CC BB MEMT IR-BUILD:ADD-RESULT
   CC BB CELLT IR-BUILD:ADD-RESULT
   CC BB HIR:KEY-ENTRY e INT-ATTR+
   CC BB HIR:KEY-IN 1 INT-ATTR+
   CC BB HIR:KEY-OUT 1 INT-ATTR+
   CC BB IR-BUILD:END-OP {: id:IR-ID:ir-op-id :}
   CC BB id 1 IR-BUILD:OP-RESULT@ ;

\ `( a -- n )` that calls: the twelve doublings and their sum, a call to another
\ word's entry with that sum, and the twelve doublings of its answer and their
\ sum. The frame is reserved at the entry and released before the return, so it
\ is held across the call - and nothing is live in a register there: the call
\ site hands its argument over on the data stack (select-x64.f CALL-SAVE).
: BUILD-PCALLER ( n -- )
   {: e:n :}
   1 1 OPEN-FUN
   ARG+ {: a:IR-ID:ir-value-id :}
   MEM0 {: tok:IR-ID:ir-value-id :}
   a DOUBLINGS
   tok DOUBLINGS-SUM e WORDCALL1 DOUBLINGS
   DOUBLINGS-SUM RET1
   CLOSE-FUN ;

\ `: LEAF ( a b -- n ) - ;` - two arguments that die at the subtraction. Nothing
\ is live that a register cannot hold, so the allocator seals an empty plan.
: BUILD-DIFF ( -- )
   2 1 OPEN-FUN
   ARG+ {: x:IR-ID:ir-value-id :}
   ARG+ {: y:IR-ID:ir-value-id :}
   HIR-OPCODE:SUB x y BINOP RET1
   CLOSE-FUN ;

\ `: LEAF ( -- xt ) [: 3000 ;] ;`: a module of two functions, the first
\ answering the address of the second, which `hir.quot` names by its ordinal.
\ Both answer one cell, because the contract is the whole module's.
: QUOT1 ( n -- IR-ID:ir-value-id )
   {: k:n :}
   HIR-OPCODE:QUOT BODY-ST BODY-LN OPEN-OP
   CC BB CELLT IR-BUILD:ADD-RESULT
   CC BB HIR:KEY-FUN k INT-ATTR+
   CLOSE-VALUE ;

: BUILD-QUOTER ( -- )
   0 1 OPEN-FUN
   1 QUOT1 RET1
   CLOSE-FUN
   0 1 s" SECOND" OPEN-FUN-NAMED
   3000 CONSTOP RET1
   CLOSE-FUN ;

\ ---- driving the chain -------------------------------------------------------
\ The driver's order before emission: declare what the definition takes and
\ leaves, select, prune, and lower to a fixpoint.
: CHAIN-LINKED ( n n NBACK:linkage -- IR-BUILD:module )
   {: in:n out:n l:NBACK:linkage :}
   NS in out l NBACK:DECLARE
   NS BB NBACK:FREEZE {: hm:IR-BUILD:module :}
   NS hm NBACK:SELECT {: m0:IR-BUILD:module :}
   hm IR-BUILD:RETIRE
   NS m0 NBACK:PRUNE {: m1:IR-BUILD:module :}
   NS m1 NBACK:FIXPOINT ;

: CHAIN ( n n -- IR-BUILD:module )
   NBACK:L-NONE CHAIN-LINKED ;

\ The frame the last allocation settled on, as the ABI's slot count. There is no
\ prologue slot to subtract on this machine: `call` pushes the return address on
\ the machine stack, so a frame holds the allocator's spills and nothing else.
: SPILL-SLOTS ( -- n )
   A64RA:FRAME X64IR:SLOT-WIDTH / ;

\ The contract the chain lowered under, rebuilt from the same three facts
\ X64PASS builds it from - so a routine the validator accepts here is the
\ routine the pass declared.
: ACCEPTED ( IR-BUILD:module n n -- bool )
   {: m:IR-BUILD:module in:n out:n :}
   m  X64ABI:SCRATCH in out SPILL-SLOTS X64ABI:LEAF-FRAMED  A64RAV:ACCEPT
   A64RAV:ACCEPTED? ;

\ ---- reading the lowered module ----------------------------------------------
\ A symbol is an ordinal of the module that interned it, so what identifies a
\ form across modules is its SPELLING - which is how the spill pass itself
\ crosses between the module it reads and the one it writes.
64 constant NAME-CAP
create NAMEBUF NAME-CAP allot

: SYM$ ( IR-BUILD:module IR-ID:ir-symbol-id -- ptr u8 n )
   {: m:IR-BUILD:module sym:IR-ID:ir-symbol-id :}
   m IR-BUILD:FSYM-POOL  m IR-BUILD:FSYM-ROWS  sym  NAMEBUF NAME-CAP
   IR-SYM:FCOPY {: u:n :}
   NAMEBUF u ;

\ The integer an operation carries under a named key. -1 where the operation
\ carries no such key, which no case below expects: each one pins the number the
\ key really holds.
: ATTR-N ( IR-BUILD:module IR-ID:ir-op-id ptr u8 n -- n )
   {: m:IR-BUILD:module op:IR-ID:ir-op-id a:ptr u:n :}
   op NFROZEN:ATTRS-OF {: n:n :}
   n 0 ?do
      m  op i NFROZEN:ATTR-KEY-AT  SYM$  a u STR= if
         op i NFROZEN:ATTR-INT-AT unloop exit
      then
   loop
   -1 ;

variable N-STORE                     \ frame stores the lowering wrote
variable N-LOAD                      \ and the loads that bring those values back
variable N-RESERVE
variable SLOT-A                      \ the frame slot the first store names
variable SLOT-TOP                    \ the highest any store names
variable FRAME-BYTES                 \ the size the reserve carries

: SCAN-CLEAR ( -- )
   0 N-STORE !  0 N-LOAD !  0 N-RESERVE !
   -1 SLOT-A !  -1 SLOT-TOP !  -1 FRAME-BYTES ! ;

: STORE-SEEN ( IR-BUILD:module IR-ID:ir-op-id -- )
   {: m:IR-BUILD:module op:IR-ID:ir-op-id :}
   m op s" x64.slot" ATTR-N {: slot:n :}
   N-STORE @ 0= if slot SLOT-A ! then
   slot SLOT-TOP @ > if slot SLOT-TOP ! then
   N-STORE @ 1+ N-STORE ! ;

: SCAN-OP ( IR-BUILD:module IR-ID:ir-op-id -- )
   {: m:IR-BUILD:module op:IR-ID:ir-op-id :}
   m op NFROZEN:OPCODE-AT SYM$ {: a:ptr u:n :}
   a u s" x64.store" STR= if m op STORE-SEEN exit then
   a u s" x64.load" STR= if N-LOAD @ 1+ N-LOAD ! exit then
   a u s" x64.reserve" STR= if
      N-RESERVE @ 1+ N-RESERVE !
      m op s" x64.frame" ATTR-N FRAME-BYTES !
   then ;

\ Every shape here is one function of one straight line, which the lowering
\ keeps: the reserve, the stores and the loads are threaded into the block the
\ values they carry are in.
: SCAN ( IR-BUILD:module -- n )
   {: m:IR-BUILD:module :}
   SCAN-CLEAR
   m NFROZEN:VIEWS!
   NFROZEN:MKEY 0 IR-ID:PACK-FUN {: f:IR-ID:ir-fun-id :}
   f NFROZEN:BLOCK-COUNT {: blocks:n :}
   f 0 NFROZEN:BLOCK-AT {: b:IR-ID:ir-block-id :}
   b NFROZEN:OP-COUNT {: n:n :}
   n 0 ?do  m  b i NFROZEN:OP-AT  SCAN-OP  loop
   blocks ;

\ ---- the cases ---------------------------------------------------------------
\ WHAT THE NUMBERS BELOW ARE. The fixpoint takes two turns over this shape. The
\ first spills four of the twelve doublings; the second spills the four copies
\ selection made for the doublings that are still live. Eight stores and eight
\ loads over FOUR slots - 0, 8, 16 and 24 - because a slot is reused once the
\ value in it is dead: each slot holds a copy up to its doubling and that
\ doubling afterwards. So the frame the reserve carries is the highest slot plus
\ one slot width, 32 bytes, and it is one reserve for the one function.
: PRESSURE-BODY ( IR-CTX:ctx -- n n n n n n n bool )
   HIR-MOD
   BUILD-PRESSURE
   1 1 CHAIN {: m:IR-BUILD:module :}
   m 1 1 ACCEPTED {: ok:bool :}
   m SCAN {: blocks:n :}
   N-STORE @  N-LOAD @  SLOT-A @  SLOT-TOP @  N-RESERVE @  FRAME-BYTES @
   blocks  ok ;

\ The prune row on the module selection wrote: the very same module comes back,
\ by identity, with nothing threaded into it.
: PRUNE-BODY ( IR-CTX:ctx -- n n n bool )
   HIR-MOD
   BUILD-DIFF
   NS 2 1 NBACK:L-NONE NBACK:DECLARE
   NS BB NBACK:FREEZE {: hm:IR-BUILD:module :}
   NS hm NBACK:SELECT {: m0:IR-BUILD:module :}
   hm IR-BUILD:RETIRE
   NS m0 NBACK:PRUNE {: m1:IR-BUILD:module :}
   m0 IR-BUILD:FMODULE  m1 IR-BUILD:FMODULE  IR-ID:MODULE-SAME? {: same:bool :}
   m1 SCAN drop
   N-STORE @  N-LOAD @  N-RESERVE @  same ;

: UNCHANGED-BODY ( IR-CTX:ctx -- n n n n bool bool )
   HIR-MOD
   BUILD-DIFF
   NS 2 1 NBACK:L-NONE NBACK:DECLARE
   NS BB NBACK:FREEZE {: hm:IR-BUILD:module :}
   NS hm NBACK:SELECT {: m0:IR-BUILD:module :}
   hm IR-BUILD:RETIRE
   NS m0 NBACK:PRUNE {: m1:IR-BUILD:module :}
   NS m1 NBACK:FIXPOINT {: m2:IR-BUILD:module :}
   m0 IR-BUILD:FMODULE  m2 IR-BUILD:FMODULE  IR-ID:MODULE-SAME? {: same:bool :}
   m2 2 1 ACCEPTED {: ok:bool :}
   m2 SCAN drop
   A64RA:PLAN-N {: plan:n :}
   N-STORE @  N-LOAD @  N-RESERVE @  plan  same  ok ;

\ ---- the two rows that finish a definition -----------------------------------
\ A SLOT THE PUBLISHER COULD REALLY NAME: whole multiples of X64IR:SP-ALIGN are
\ the unit a code region hands out, and this one is not zero, so the emission is
\ measured from a placement that is not the number an unplaced one would use.
X64IR:SP-ALIGN 4 * constant EMIT-SLOT

: HEX-DIGIT ( n -- n ) {: c:n :}
   c 48 >= c 57 <= and if c 48 - exit then
   c 97 >= c 102 <= and 0= if E-X64EMIT-FORM throw then
   c 97 - 10 + ;

: HEX-BYTE ( ptr u8 n -- n ) {: a:ptr i:n :}
   a i 2 * + c@ HEX-DIGIT 4 lshift
   a i 2 * 1 + + c@ HEX-DIGIT or ;

: SPAN=HEX? ( ptr u8 n ptr u8 n -- bool ) {: da:ptr dlen:n ea:ptr eu:n :}
   dlen eu 2 / <> if false exit then
   dlen 0 ?do
      ea i HEX-BYTE da i + c@ <> if false unloop exit then
   loop
   true ;

\ The same image test/compiler/x64-emit.f pins for this routine under this
\ contract, read back off the row instead of off the emitter: what the chain adds
\ is the placement and the driver's own order, not other bytes.
: DIFF-IMAGE? ( ptr u8 n -- bool )
   s" 4981ec10000000498b0424498b4c24084829c8498904244981c408000000c3" SPAN=HEX? ;

\ Retire is idempotent: this case calls it before the scoped cleanup does.
: PLACED-BODY ( IR-CTX:ctx -- n n bool bool )
   HIR-MOD
   BUILD-DIFF
   2 1 CHAIN {: m:IR-BUILD:module :}
   NS m EMIT-SLOT NBACK:EMIT
   m SCAN {: blocks:n :}
   NS NART:COPY {: e:NART:emission :}
   e NART:SIZE {: sz:n :}
   e NART:BYTES e NART:SIZE DIFF-IMAGE? {: same:bool :}
   NS NBACK:RETIRE
   X64EMIT:SEALED? 0= {: gone:bool :}
   sz blocks same gone ;

\ ---- the rows publication reads ------------------------------------------------
\ The owned artifact states the image the emitter sealed, no
\ trailing return because the span is exact, and the slot it was measured from.
\ Owned rows remain readable after backend retirement. This foreign artifact
\ still refuses publication into the host before moving CP or the dictionary.
: ROWS-BODY ( IR-CTX:ctx -- bool n n n n )
   HIR-MOD
   BUILD-DIFF
   2 1 CHAIN {: m:IR-BUILD:module :}
   NS m EMIT-SLOT NBACK:EMIT
   NS NART:COPY {: e:NART:emission :}
   NS NBACK:RETIRE
   e NART:BYTES e NART:SIZE DIFF-IMAGE? {: same:bool :}
   e NART:RET-BYTES {: ret:n :}
   e NART:PLACEMENT {: at:n :}
   cp@ {: cp0:n :}
   ndict@ {: nd0:n :}
   e W-ART !
   [: PUBLISH-FOREIGN ;] catch E-NPUB-TARGET T=
   same ret at  cp@ cp0 -  ndict@ nd0 - ;

\ ---- the spilling modules through the emit row -------------------------------
\ The frame is taken on rsp by the routine's first instruction and given back by
\ the one before its return: the spill pass opens the entry block with the
\ reserve and stands the release in front of the return (spill.f WALK-BLOCK),
\ and the emitter lays the entry first and the return block last. 32 is the
\ frame the first case reads off the reserve. What these bytes do when they run
\ is test/x86-64-peer-routines.f's question.
: HEAD=HEX? ( NART:emission ptr u8 n -- bool )
   {: e:NART:emission ea:ptr eu:n :}
   e NART:BYTES eu 2 / ea eu SPAN=HEX? ;

: TAIL=HEX? ( NART:emission ptr u8 n -- bool )
   {: e:NART:emission ea:ptr eu:n :}
   eu 2 / {: k:n :}
   e NART:BYTES e NART:SIZE k - + k ea eu SPAN=HEX? ;

: PRESSURE-EMIT-BODY ( IR-CTX:ctx -- bool bool )
   HIR-MOD
   BUILD-PRESSURE
   1 1 CHAIN {: m:IR-BUILD:module :}
   NS m EMIT-SLOT NBACK:EMIT
   NS NART:COPY {: e:NART:emission :}
   \ mc: subq $32, %rsp
   e s" 4881ec20000000" HEAD=HEX? {: head:bool :}
   \ mc: addq $32, %rsp
   \ mc: retq
   e s" 4881c420000000c3" TAIL=HEX? {: tail:bool :}
   head tail ;

\ A shape through every row, emit included: how many reserves the entry block of
\ the lowered module holds, and whether the emission was sealed. The emission
\ is copied before scoped cleanup so a case can read its rows first.
: FRAMED ( n n NBACK:linkage -- n NART:emission )
   CHAIN-LINKED {: m:IR-BUILD:module :}
   m SCAN drop
   NS m EMIT-SLOT NBACK:EMIT
   N-RESERVE @ NS NART:COPY ;

: FRAMED-EMIT ( n n NBACK:linkage -- n bool )
   FRAMED NART:PLACED? ;

$400 constant CALLEE-ENTRY           \ the entry the caller's site names

: PBRANCH-BODY ( IR-CTX:ctx -- n bool )
   HIR-MOD BUILD-PBRANCH 2 1 NBACK:L-NONE FRAMED-EMIT ;

: PLOOP-BODY ( IR-CTX:ctx -- n bool )
   HIR-MOD BUILD-PLOOP 2 1 NBACK:L-NONE FRAMED-EMIT ;

\ The call as publication reads it: one site, a call that comes back, to the
\ callee's entry, filed where the `call` itself starts - the e8 its rel32
\ follows.
: CALL-ROW ( NART:emission -- n n n n )
   {: e:NART:emission :}
   e NART:CALL-SITES e 0 NART:CALL-KIND@ e 0 NART:CALL-TARGET@
   e NART:BYTES e 0 NART:CALL-SITE@ + c@ ;

: PCALLER-BODY ( IR-CTX:ctx -- n n n n n bool )
   HIR-MOD CALLEE-ENTRY BUILD-PCALLER 1 1 NBACK:L-CALLED FRAMED
   {: r:n e:NART:emission :}
   e CALL-ROW r e NART:PLACED? ;

\ A function's address as publication reads it: a row for each of the two
\ functions and none past them, and one site of the code kind at the
\ `mov r64, imm64` that loads the second one's address. Nothing is called.
: QUOTER-BODY ( IR-CTX:ctx -- n n n n n n )
   HIR-MOD BUILD-QUOTER
   0 1 CHAIN {: m:IR-BUILD:module :}
   NS m EMIT-SLOT NBACK:EMIT
   NS NART:COPY {: e:NART:emission :}
   e W-ART !
   [: BAD-OFFSET ;] E-NEMIT-ROW TTHROWSQ
   e 0 NART:FUNCTION-OFFSET@ e 1 NART:FUNCTION-OFFSET@
   e NART:ADDR-SITES e 0 NART:ADDR-SITE@ e 0 NART:ADDR-SITE-KIND@
   e NART:CALL-SITES ;

\ ---- a callee no rel32 reaches ------------------------------------------------
\ Four gigabytes from the slot, so the writer refuses the call's field inside
\ the emit row, before the emission seals, and no row is stated: a publisher
\ asked now finds nothing to publish.
$100000000 constant FAR-ENTRY

\ The module the refusing quotation emits, which it cannot take as a local.
1 TYPED-BUFFER W-MOD IR-BUILD:module

: FAR-EMIT ( -- )
   NS 0 W-MOD @ EMIT-SLOT NBACK:EMIT ;

: FAR-BODY ( IR-CTX:ctx -- )
   HIR-MOD FAR-ENTRY BUILD-PCALLER
   1 1 NBACK:L-CALLED CHAIN-LINKED 0 W-MOD !
   [: FAR-EMIT ;] E-X64EMIT-REACH TTHROWSQ
   [: NS NART:COPY NART:RELEASE ;] E-NEMIT-STATE TTHROWSQ ;

public

: RUN ( -- )
   T-RESET

   s" twelve live values do not fit nine registers: the fixpoint lowers the plan into this dialect's own stores and loads over two turns, eight of each, and the validator accepts the module that comes out" T-LABEL
   [: PRESSURE-BODY ;] WITH-CASE
   TTRUE 1 T= 32 T= 1 T= 24 T= 8 T= 8 T= 8 T=

   s" prune is a pass-through on this machine: the row answers the very module selection wrote, by identity, and threads no store, load or reserve into it" T-LABEL
   [: PRUNE-BODY ;] WITH-CASE
   TTRUE 0 T= 0 T= 0 T=

   s" a routine that fits comes back from the fixpoint as the very module selection wrote: nothing was lowered because the allocator sealed an empty plan" T-LABEL
   [: UNCHANGED-BODY ;] WITH-CASE
   TTRUE TTRUE 0 T= 0 T= 0 T= 0 T=

   s" the last two rows: the chain's module is placed at the slot the driver names and comes back sealed as the bytes x64-emit.f pins for it, and retiring gives the emission back twice over" T-LABEL
   [: PLACED-BODY ;] WITH-CASE
   TTRUE TTRUE 1 T= 31 T=

   s" the emit row hands publication the sealed emission - the same image, no trailing return because the span is exact, the slot it was measured from - and retiring clears it, so the publisher asked next refuses and moves neither CP nor NDICT" T-LABEL
   [: ROWS-BODY ;] WITH-CASE
   0 T= 0 T= EMIT-SLOT T= 0 T= TTRUE

   s" the spilling module through the emit row: its first instruction takes the 32-byte frame on rsp and the one before its return gives it back" T-LABEL
   [: PRESSURE-EMIT-BODY ;] WITH-CASE
   TTRUE TTRUE

   s" the pressure held across a branch lowers into one frame, reserved where the routine enters, and is emitted" T-LABEL
   [: PBRANCH-BODY ;] WITH-CASE
   TTRUE 1 T=

   s" the pressure held around a loop lowers into one frame, reserved where the routine enters, and is emitted" T-LABEL
   [: PLOOP-BODY ;] WITH-CASE
   TTRUE 1 T=

   s" the pressure on both sides of a call lowers into one frame held across it, and is emitted" T-LABEL
   [: PCALLER-BODY ;] WITH-CASE
   TTRUE 1 T=
   s" its call is the row publication reads: one site, a call that comes back, to the callee's entry, at the e8 its rel32 follows" T-LABEL
   $E8 T= CALLEE-ENTRY T= NEMIT:CALL T= 1 T=

   s" a function's address as publication reads it: a row for each of the two functions and none past them, one site of the code kind at the literal that loads the second's, and no call" T-LABEL
   [: QUOTER-BODY ;] WITH-CASE
   0 T=                                  \ no call row
   X64IR:ADDR-CODE T=                    \ the site is a code address
   0 T=                                  \ at byte zero, the `mov r64, imm64` itself
   1 T=                                  \ one site: the literal 3000 is no address
   22 T=                                 \ the second function starts after the first's 22 bytes
   0 T=                                  \ and the first where the emission does

   s" a callee four gigabytes from the slot is refused inside the emit row, before any row publication reads is stated" T-LABEL
   [: FAR-BODY ;] WITH-CASE

   T-REPORT ;

;package

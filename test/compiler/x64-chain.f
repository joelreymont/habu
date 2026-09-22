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
\ NOW FILLED: the last case runs the driver's whole order - declare, select,
\ prune, fixpoint, emit at a named slot, retire - and reads the sealed image back
\ off the emitter, which is what says the x86-64 chain is reachable end to end
\ from src/compiler/native/compiler.f. What the driver does with those bytes is
\ another stage's: this host's publisher takes ARM64 words only, so an x86-64
\ emission is a cross-build's input and is published nowhere here.
\
\ ONE FIXTURE PER CONTEXT, and each case gives the passes it bound back through
\ the release row before its context leaves: a context that dies holding them
\ gives its registry slots back only when a live enclosing one leaves normally
\ (src/compiler/ir/context.f, the note on stale handles).

require lib/test.f
require lib/string.f
require src/compiler/ir/id.f
require src/compiler/ir/symbol.f
require src/compiler/ir/build.f
require src/compiler/native/backend.f
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

: CC ( -- IR-CTX:ctx )               0 W-CTX @ ;
: BB ( -- IR-BUILD:builder )         0 W-BLD @ ;
: SS ( -- IR-ID:ir-source-id )       0 W-SRC @ ;

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

: OPEN-FUN ( n n -- )
   {: in:n out:n :}
   CC BB  CC BB s" LEAF" IR-BUILD:INTERN-SYMBOL  IR-BUILD:BEGIN-FUN
   CC BB  in out SIGN  IR-BUILD:SET-SIGNATURE
   CC BB IR--FUN-LINKAGE:DEFINED IR-BUILD:SET-LINKAGE
   CC BB IR--FUN-VISIBILITY:EXPORTED IR-BUILD:SET-VISIBILITY
   CC BB IR--FUN-CONVENTION:HABU IR-BUILD:SET-CONVENTION
   CC BB  NAME-ST NAME-LN SPN  IR-BUILD:SET-FUN-SPAN
   CC BB IR-BUILD:BEGIN-BLOCK
   CC BB  OPEN-ST OPEN-LN SPN  IR-BUILD:SET-BLOCK-SPAN ;

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

: BUILD-PRESSURE ( -- )
   1 1 OPEN-FUN
   ARG+ {: a:IR-ID:ir-value-id :}
   PRESSURE-N 0 ?do
      HIR-OPCODE:ADD a a BINOP  i ARGV !
   loop
   HIR-OPCODE:ADD  0 ARGV @  1 ARGV @  BINOP
   PRESSURE-N 2 ?do
      HIR-OPCODE:ADD swap  i ARGV @  BINOP
   loop
   RET1
   CLOSE-FUN ;

\ `: LEAF ( a b -- n ) - ;` - two arguments that die at the subtraction. Nothing
\ is live that a register cannot hold, so the allocator seals an empty plan.
: BUILD-DIFF ( -- )
   2 1 OPEN-FUN
   ARG+ {: x:IR-ID:ir-value-id :}
   ARG+ {: y:IR-ID:ir-value-id :}
   HIR-OPCODE:SUB x y BINOP RET1
   CLOSE-FUN ;

\ ---- driving the chain -------------------------------------------------------
\ The driver's own order up to the emit row this backend leaves unfilled:
\ declare what the definition takes and leaves, select, prune, and lower to a
\ fixpoint. src/compiler/native/compiler.f runs exactly these words.
: CHAIN ( n n -- IR-BUILD:module )
   {: in:n out:n :}
   CC in out NBACK:L-NONE NBACK:DECLARE
   CC BB NBACK:SELECT {: m0:IR-BUILD:module :}
   CC m0 NBACK:PRUNE {: m1:IR-BUILD:module :}
   CC m1 NBACK:FIXPOINT ;

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
   CC NBACK:RELEASE
   N-STORE @  N-LOAD @  SLOT-A @  SLOT-TOP @  N-RESERVE @  FRAME-BYTES @
   blocks  ok ;

\ The prune row on the module selection wrote: the very same module comes back,
\ by identity, with nothing threaded into it.
: PRUNE-BODY ( IR-CTX:ctx -- n n n bool )
   HIR-MOD
   BUILD-DIFF
   CC 2 1 NBACK:L-NONE NBACK:DECLARE
   CC BB NBACK:SELECT {: m0:IR-BUILD:module :}
   CC m0 NBACK:PRUNE {: m1:IR-BUILD:module :}
   m0 IR-BUILD:FMODULE  m1 IR-BUILD:FMODULE  IR-ID:MODULE-SAME? {: same:bool :}
   m1 SCAN drop
   CC NBACK:RELEASE
   N-STORE @  N-LOAD @  N-RESERVE @  same ;

: UNCHANGED-BODY ( IR-CTX:ctx -- n n n n bool bool )
   HIR-MOD
   BUILD-DIFF
   CC 2 1 NBACK:L-NONE NBACK:DECLARE
   CC BB NBACK:SELECT {: m0:IR-BUILD:module :}
   CC m0 NBACK:PRUNE {: m1:IR-BUILD:module :}
   CC m1 NBACK:FIXPOINT {: m2:IR-BUILD:module :}
   m0 IR-BUILD:FMODULE  m2 IR-BUILD:FMODULE  IR-ID:MODULE-SAME? {: same:bool :}
   m2 2 1 ACCEPTED {: ok:bool :}
   m2 SCAN drop
   A64RA:PLAN-N {: plan:n :}
   CC NBACK:RELEASE
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
: DIFF-IMAGE? ( -- bool )
   X64EMIT:BYTES X64EMIT:SIZE
   s" 4981ec10000000498b0424498b4c24084829c8498904244981c408000000c3" SPAN=HEX? ;

\ The driver's last two calls in the order src/compiler/native/compiler.f makes
\ them: emit at the slot the publisher named, then retire inside the dying
\ context. Retire runs there on the accepting path AND on the refusing one, so
\ it is called twice here: the second has nothing left to give back and is
\ nonetheless harmless.
: PLACED-BODY ( IR-CTX:ctx -- n n bool bool )
   HIR-MOD
   BUILD-DIFF
   2 1 CHAIN {: m:IR-BUILD:module :}
   CC m EMIT-SLOT NBACK:EMIT
   X64EMIT:SIZE {: sz:n :}
   X64EMIT:BLOCKS {: blocks:n :}
   DIFF-IMAGE? {: same:bool :}
   CC NBACK:RETIRE
   CC NBACK:RETIRE
   X64EMIT:SEALED? 0= {: gone:bool :}
   CC NBACK:RELEASE
   sz blocks same gone ;

public

: RUN ( -- )
   T-RESET

   s" twelve live values do not fit nine registers: the fixpoint lowers the plan into this dialect's own stores and loads over two turns, eight of each, and the validator accepts the module that comes out" T-LABEL
   WBND [: PRESSURE-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE 1 T= 32 T= 1 T= 24 T= 8 T= 8 T= 8 T=

   s" prune is a pass-through on this machine: the row answers the very module selection wrote, by identity, and threads no store, load or reserve into it" T-LABEL
   WBND [: PRUNE-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE 0 T= 0 T= 0 T=

   s" a routine that fits comes back from the fixpoint as the very module selection wrote: nothing was lowered because the allocator sealed an empty plan" T-LABEL
   WBND [: UNCHANGED-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE 0 T= 0 T= 0 T= 0 T=

   s" the last two rows: the chain's module is placed at the slot the driver names and comes back sealed as the bytes x64-emit.f pins for it, and retiring gives the emission back twice over" T-LABEL
   WBND [: PLACED-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE 1 T= 31 T=

   T-REPORT ;

;package

X64CHAIN-TEST:RUN

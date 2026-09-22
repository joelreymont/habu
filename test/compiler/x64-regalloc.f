\ x64-regalloc.f - the first x86-64 register allocation.
\
\ src/compiler/native/regalloc.f (A64RA) and regalloc-verify.f (A64RAV) are one
\ allocator and one validator for the compiler rather than one per machine. What
\ they used to spell as ARM64's - the three value types, the attribute keys, the
\ copy and the re-emittable opcode, how many instructions an address literal is,
\ the bytes one frame access moves and the architecture the allocation is for -
\ now arrives in the vocabulary the dialect builds
\ (src/compiler/native/dialect.f). This suite is what proves the vocabulary is
\ ENOUGH: the same two passes run here over a module in which no ARM64 name
\ appears, and the ARM64 suite next door runs unchanged over the other.
\
\ WHAT THESE CASES MEASURE THAT test/compiler/native-regalloc.f CANNOT:
\
\ - THE ARCHITECTURE. The allocator refused any contract whose arch was not
\   AARCH64. It now refuses any contract that is not the bound dialect's own
\   machine, and an x86-64 contract allocates - which is this whole suite.
\ - THE MISSING KEY. x86-64 has no write-back addressing, so its vocabulary
\   declares `x64.dwb` ABSENT: no transfer moves the data-stack pointer inside
\   its own encoding and no symbol names one. Every data-stack case below runs
\   through the reader that has to answer "no such transfer" without a symbol.
\ - THE COPY OPCODE. A copy is `x64.mov` here and `a64.mov` there, and the
\   allocator coalesces by the vocabulary's opcode. The two-address case pins
\   registers on both sides of a copy the selector inserted.
\ - ONE-LANE ADDRESSES. `mov r64, imm64` is one instruction, so this dialect
\   declares one address-carrier lane and the allocator does no lane arithmetic;
\   ARM64's four move-wides must stay contiguous through spill insertion.
\
\ A REGISTER CONVENTION, which is the shape the ARM64 cases allocate under too:
\ the contract names no place, so the module is the body the source selected to
\ and every value in it is one the allocator is free to place.
\
\ AND X64ABI:LEAF, WHICH IS THE DATA-STACK CONVENTION. Where the data-stack
\ pointer STANDS over a body is the SELECTOR's policy and not something either
\ later pass can derive from the module: A64SEL surveys the boundary transfers
\ and stands where the fewest need an adjustment, X64SEL takes every argument's
\ bytes at the entry and publishes every result's at the exit. The dialect is
\ what states which of the two it is - the vocabulary's `stand` field, `survey`
\ for a64ir and `entry-base` for x64ir (dialect.f NDIALECT:dstand) - and
\ regalloc-verify.f VDPLACE-CK measures a module against the policy its dialect
\ states rather than re-deriving one of them. The leaf case below is that fact
\ measured.
\
\ ONE FIXTURE PER CONTEXT, and the refusing case runs inside an enclosing one:
\ an abandoned context gives its registry slots back only when a live enclosing
\ context leaves normally (src/compiler/ir/context.f, the note on stale handles).

require lib/test.f
require src/compiler/native/select-x64.f
require src/compiler/native/regalloc.f
require src/compiler/native/regalloc-verify.f
require src/compiler/native/a64ir.f
require src/arch/x86-64/abi.f
require src/arch/x86-64/machine.f

package X64RA-TEST
private

\ ---- bindings ----------------------------------------------------------------
\ A linux x86-64 contract whose integer overflow wraps: the machine's own
\ behaviour, and the one this dialect selects arithmetic under.
: WBND ( -- CBIND:binding )
   CTARGET-ARCH:X86-64 CTARGET-ABI:SYSV-AMD64 CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64
   CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH CTARGET:CONTRACT
   CNUM-OVERFLOW:WRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY
   CBIND:BIND ;

\ The other machine, for the one case that builds a module of the other dialect:
\ a dialect may only make a builder under a contract its backend lowers for.
: ABND ( -- CBIND:binding )
   CTARGET-ARCH:AARCH64 CTARGET-ABI:AAPCS64-DARWIN CTARGET-ENDIAN:LITTLE
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

$400 constant CALLEE-ENTRY           \ the address the tail case leaves through

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

: MEMT ( -- IR-ID:ir-type-id )
   CC BB HIR:MEM-TYPE ;

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

\ ---- the shapes --------------------------------------------------------------
\ `: LEAF ( a b -- n ) - ;` - two arguments that die at the subtraction, which is
\ two-address and destroys the first of them. Nothing is copied.
: BUILD-DIFF ( -- )
   2 1 OPEN-FUN
   ARG+ {: x:IR-ID:ir-value-id :}
   ARG+ {: y:IR-ID:ir-value-id :}
   HIR-OPCODE:SUB x y BINOP RET1
   CLOSE-FUN ;

\ `: LEAF ( a -- n ) dup + ;` - one argument read twice by a form that destroys
\ its first operand, so selection copies it with `x64.mov` first. The copy and
\ its source are live at the same instant and cannot share a register.
: BUILD-SQUARE ( -- )
   1 1 OPEN-FUN
   ARG+ {: a:IR-ID:ir-value-id :}
   HIR-OPCODE:ADD a a BINOP RET1
   CLOSE-FUN ;

\ `: LEAF ( a b -- n ) lshift ;` - a count that is not a literal, which the
\ selector copies with `x64.mov` because the form fixes the copy to rcx. Nothing
\ else reads the count, so the copy is coalesced back into it and one class
\ carries the register.
: BUILD-VAR-SHIFT ( -- )
   2 1 OPEN-FUN
   ARG+ {: x:IR-ID:ir-value-id :}
   ARG+ {: y:IR-ID:ir-value-id :}
   HIR-OPCODE:LSHIFT x y BINOP RET1
   CLOSE-FUN ;

\ `: LEAF ( a b c -- n ) lshift lshift ;` - two variable shifts whose counts are
\ BOTH live at the first of them. One register holds one count at one instant, so
\ the second count's copy may not be coalesced back into it: that would put two
\ demands for rcx over one interval and no placement satisfies both.
: BUILD-TWO-SHIFTS ( -- )
   3 1 OPEN-FUN
   ARG+ {: x:IR-ID:ir-value-id :}
   ARG+ {: c1:IR-ID:ir-value-id :}
   ARG+ {: c2:IR-ID:ir-value-id :}
   HIR-OPCODE:LSHIFT x c1 BINOP {: s1:IR-ID:ir-value-id :}
   HIR-OPCODE:LSHIFT s1 c2 BINOP RET1
   CLOSE-FUN ;

\ `: LEAF ( a b -- n ) / ;` - the dividend is copied into the register the divide
\ takes it in, the divisor is the free operand, and the remainder is a result
\ nothing reads.
: BUILD-DIV ( -- )
   2 1 OPEN-FUN
   ARG+ {: x:IR-ID:ir-value-id :}
   ARG+ {: y:IR-ID:ir-value-id :}
   HIR-OPCODE:DIV x y BINOP RET1
   CLOSE-FUN ;

\ `B0(x, c0): br B1(c0); B1(c): t = add x c; brz t -> B2 / B3; B2: ret t; B3: br
\ B1(t)` - the loop whose header adds a value defined above it. `x` lives across
\ the backedge and the add is two-address, so the two ends of the tie overlap
\ unless selection copied `x` first: where it does not, this allocation is
\ refused with E-A64RA-TIE over a module nothing is wrong with.
: BUILD-LOOP ( -- )
   2 1 OPEN-FUN
   ARG+ {: x:IR-ID:ir-value-id :}
   ARG+ {: c0:IR-ID:ir-value-id :}
   c0 1 BR1
   BLOCK+
   ARG+ {: c:IR-ID:ir-value-id :}
   HIR-OPCODE:ADD x c BINOP {: t:IR-ID:ir-value-id :}
   t 2 3 BRZ2
   BLOCK+
   t RET1
   BLOCK+
   t 1 BR1
   CLOSE-FUN ;

: MEM0 ( -- IR-ID:ir-value-id )
   HIR-OPCODE:MEM BODY-ST BODY-LN OPEN-OP
   CC BB MEMT IR-BUILD:ADD-RESULT
   CLOSE-VALUE ;

: WCALL-ATTRS ( n n n -- )
   {: e:n in:n out:n :}
   CC BB  CC BB HIR:KEY-ENTRY  CC BB e IR-BUILD:INTERN-INT-ATTR IR-BUILD:ADD-ATTR
   CC BB  CC BB HIR:KEY-IN     CC BB in IR-BUILD:INTERN-INT-ATTR IR-BUILD:ADD-ATTR
   CC BB  CC BB HIR:KEY-OUT    CC BB out IR-BUILD:INTERN-INT-ATTR IR-BUILD:ADD-ATTR ;

\ One call to a one-in one-out callee, carrying one value besides its argument.
\ Its operands are the memory order, the carried value and the argument, and its
\ results are the order, the carried value again and the callee's answer.
: WCALL1 ( IR-ID:ir-value-id IR-ID:ir-value-id IR-ID:ir-value-id -- IR-ID:ir-op-id )
   {: tok:IR-ID:ir-value-id live:IR-ID:ir-value-id arg:IR-ID:ir-value-id :}
   HIR-OPCODE:WORDCALL BODY-ST BODY-LN OPEN-OP
   CC BB tok IR-BUILD:ADD-OPERAND
   CC BB live IR-BUILD:ADD-OPERAND
   CC BB arg IR-BUILD:ADD-OPERAND
   CC BB MEMT IR-BUILD:ADD-RESULT
   CC BB CELLT IR-BUILD:ADD-RESULT
   CC BB CELLT IR-BUILD:ADD-RESULT
   CALLEE-ENTRY 1 1 WCALL-ATTRS
   CC BB IR-BUILD:END-OP ;

\ `: LEAF ( n -- n ) CALLEE ;` - the routine that leaves through its callee.
\ Nothing the site carries is read again, which is what a tail branch needs.
: BUILD-CALLER ( -- )
   1 1 OPEN-FUN
   ARG+ {: a:IR-ID:ir-value-id :}
   MEM0 {: tok:IR-ID:ir-value-id :}
   tok a a WCALL1 {: id:IR-ID:ir-op-id :}
   CC BB id 2 IR-BUILD:OP-RESULT@ RET1
   CLOSE-FUN ;

\ ---- running selection, allocation and validation ----------------------------
: X64-BUILDER ( -- IR-BUILD:builder )
   IR-BUILD:PLAN-BEGIN
   IR-BUILD:PLAN-DEFAULT
   CC X64IR:NEW-BUILDER ;

\ The contract every case here allocates under: a leaf that computes in this
\ machine's nine allocatable general registers (X64ABI:SCRATCH), returns to its
\ caller, reserves no frame and calls nothing. It names no place, so the
\ selector adds no entry and no exit and the module is the body alone. The
\ flags are declared CLOBBERED because every arithmetic form of this machine
\ writes them - there is no add here that does not.
: LEAF ( -- NEFF:routine )
   NEFF-CONV:REGISTER NEFF:SEQ-NONE NEFF:SEQ-NONE X64ABI:SCRATCH
   NEFF:FPR-NONE NEFF:FPR-NONE NEFF:FPR-NONE
   NEFF-NZCV:CLOBBERED NEFF-LINK:ABSENT NEFF-CONTROL:RETURNS
   NEFF:TRAITS-NONE 0 0 X64M:MACHINE NEFF:ROUTINE ;

\ Bind the source dialect to the module being read and the machine dialect to
\ the module about to be written, then select. Both the allocator and the
\ validator are bound with THIS dialect's vocabulary and THIS machine, which is
\ the whole point: neither pass names x86-64 anywhere in its own text.
: SELECTED ( -- IR-BUILD:module )
   CC BB X64SEL:BIND-SOURCE
   CC BB IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   X64-BUILDER {: xb:IR-BUILD:builder :}
   CC xb X64M:MACHINE  CC xb X64IR:VOCABULARY  A64RA:BIND-DIALECT
   CC xb  CC xb X64IR:VOCABULARY  A64RAV:BIND-DIALECT
   CC m xb LEAF X64SEL:SELECT ;

\ Allocate the selected module for the same contract it was selected under and
\ have the validator accept it. Every positive case goes through both, so no
\ case reads a claim the validator has not agreed with.
: ALLOCATED ( -- IR-BUILD:module )
   SELECTED {: m:IR-BUILD:module :}
   CC m LEAF A64RA:ALLOCATE
   m LEAF A64RAV:ACCEPT
   m ;

\ ---- and the same body under the data-stack convention -----------------------
\ The other contract of this machine: the interface is two caller cells in and
\ one out, so the selector writes the boundary and the validator has a stand to
\ measure. It is x86-64's own policy that is measured, because the vocabulary
\ x64ir builds states `entry-base` and the validator reads it there.
: DLEAF ( -- NEFF:routine )
   X64ABI:SCRATCH 2 1 X64ABI:LEAF ;

: DSTACK-SELECTED ( -- IR-BUILD:module )
   CC BB X64SEL:BIND-SOURCE
   CC BB IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   X64-BUILDER {: xb:IR-BUILD:builder :}
   CC xb X64M:MACHINE  CC xb X64IR:VOCABULARY  A64RA:BIND-DIALECT
   CC xb  CC xb X64IR:VOCABULARY  A64RAV:BIND-DIALECT
   CC m xb DLEAF X64SEL:SELECT ;

: DSTACK-ALLOCATED ( -- IR-BUILD:module )
   DSTACK-SELECTED {: m:IR-BUILD:module :}
   CC m DLEAF A64RA:ALLOCATE
   m DLEAF A64RAV:ACCEPT
   m ;

\ ---- and the contract a routine leaves through its callee under --------------
\ One cell in and one out, control leaving through the callee: the pointer never
\ moves, so every cell the callee reads is a cell this routine was entered with.
: DTAIL ( -- NEFF:routine )
   X64ABI:SCRATCH 1 1 X64ABI:TAIL ;

: TAIL-SELECTED ( -- IR-BUILD:module )
   CC BB X64SEL:BIND-SOURCE
   CC BB IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   X64-BUILDER {: xb:IR-BUILD:builder :}
   CC xb X64M:MACHINE  CC xb X64IR:VOCABULARY  A64RA:BIND-DIALECT
   CC xb  CC xb X64IR:VOCABULARY  A64RAV:BIND-DIALECT
   CC m xb DTAIL X64SEL:SELECT ;

: TAIL-ALLOCATED ( -- IR-BUILD:module )
   TAIL-SELECTED {: m:IR-BUILD:module :}
   CC m DTAIL A64RA:ALLOCATE
   m DTAIL A64RAV:ACCEPT
   m ;

\ ---- the forms whose registers the MACHINE names -----------------------------
\ `shl r64, cl` reads its count from rcx and `idiv r64` divides rdx:rax into rax
\ and rdx, which x64ir declares in the schema as fixed registers beside the
\ operand types. The selector lowers both shapes, and it makes every constraint
\ satisfiable by COPYING the count and the dividend first. These modules are
\ built straight into the machine dialect - the way native-regalloc.f builds the
\ shapes its own selector never produces - so that what they measure is the
\ allocator alone: the constraint read out of the schema, and the refusal of a
\ pair no copy has been inserted for. The selected shapes are cases of their own
\ further down.
: X64-MOD ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   IR-BUILD:PLAN-BEGIN
   IR-BUILD:PLAN-DEFAULT
   c X64IR:NEW-BUILDER {: b:IR-BUILD:builder :}
   c 0 W-CTX !
   b 0 W-BLD !
   c b X64M:MACHINE  c b X64IR:VOCABULARY  A64RA:BIND-DIALECT
   c b  c b X64IR:VOCABULARY  A64RAV:BIND-DIALECT
   c b X64IR:REGISTER
   c b TXT TXT-N IR-BUILD:ADD-SOURCE 0 W-SRC ! ;

: M-OPEN ( X64IR:opcode -- )
   {: o:X64IR:opcode :}
   CC BB  CC BB o X64IR:OPCODE  IR-BUILD:BEGIN-OP
   CC BB  BODY-ST BODY-LN SPN  IR-BUILD:SET-OP-SPAN ;

: M-RESULT+ ( -- )
   CC BB  CC BB X64IR:GPR-TYPE  IR-BUILD:ADD-RESULT ;

: M-MOVI ( n -- IR-ID:ir-value-id )
   {: imm:n :}
   X64IR-OPCODE:MOVI M-OPEN
   M-RESULT+
   CC BB  CC BB X64IR:KEY-IMM   CC BB imm X64IR:IMM-ATTR  IR-BUILD:ADD-ATTR
   CC BB  CC BB X64IR:KEY-ADDR
      CC BB HIR:ADDR-NONE X64IR:ADDR-ATTR  IR-BUILD:ADD-ATTR
   CLOSE-VALUE ;

: M-BIN ( X64IR:opcode IR-ID:ir-value-id IR-ID:ir-value-id -- IR-ID:ir-value-id )
   {: o:X64IR:opcode x:IR-ID:ir-value-id y:IR-ID:ir-value-id :}
   o M-OPEN
   CC BB x IR-BUILD:ADD-OPERAND
   CC BB y IR-BUILD:ADD-OPERAND
   M-RESULT+
   CLOSE-VALUE ;

: M-SHL ( IR-ID:ir-value-id IR-ID:ir-value-id -- IR-ID:ir-value-id )
   X64IR-OPCODE:SHL -rot M-BIN ;

: M-ADD ( IR-ID:ir-value-id IR-ID:ir-value-id -- IR-ID:ir-value-id )
   X64IR-OPCODE:ADD -rot M-BIN ;

\ The form carries the routine its cold side branches to as a required attribute.
\ The allocator never reads the entry, so a stand-in address satisfies the schema.
$1000 constant THROW-STAND

: M-IDIV ( IR-ID:ir-value-id IR-ID:ir-value-id -- IR-ID:ir-value-id IR-ID:ir-value-id )
   {: x:IR-ID:ir-value-id y:IR-ID:ir-value-id :}
   X64IR-OPCODE:IDIV M-OPEN
   CC BB x IR-BUILD:ADD-OPERAND
   CC BB y IR-BUILD:ADD-OPERAND
   M-RESULT+
   M-RESULT+
   CC BB  CC BB X64IR:KEY-THROW-ENTRY
      CC BB THROW-STAND X64IR:ENTRY-ATTR  IR-BUILD:ADD-ATTR
   CC BB IR-BUILD:END-OP {: id:IR-ID:ir-op-id :}
   CC BB id 0 IR-BUILD:OP-RESULT@
   CC BB id 1 IR-BUILD:OP-RESULT@ ;

: M-RET ( IR-ID:ir-value-id -- )
   {: v:IR-ID:ir-value-id :}
   X64IR-OPCODE:RET M-OPEN
   CC BB v IR-BUILD:ADD-OPERAND
   CC BB IR-BUILD:END-OP drop ;

: M-ALLOCATED ( -- IR-BUILD:module )
   CC BB IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   CC m LEAF A64RA:ALLOCATE
   m LEAF A64RAV:ACCEPT
   m ;

: M-ALLOCATE ( -- )
   CC BB IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   CC m LEAF A64RA:ALLOCATE ;

\ A variable shift over a count nothing else reads: the count is placed in rcx
\ because the form fixes it there, and the value shifted takes the lowest free
\ register as anything else would.
: SHL-BODY ( IR-CTX:ctx -- n n bool )
   X64-MOD
   0 1 OPEN-FUN
   $40 M-MOVI {: v:IR-ID:ir-value-id :}
   3 M-MOVI {: c:IR-ID:ir-value-id :}
   v c M-SHL M-RET
   CLOSE-FUN
   M-ALLOCATED drop
   0 A64RAV:REG@
   1 A64RAV:REG@
   A64RAV:ACCEPTED? ;

\ A division with a THIRD value live across it. The dividend is fixed to rax and
\ the two results to rax and rdx, so the divisor and the value that has to
\ survive the instruction may be in neither: the divisor takes rcx because rax
\ is already the dividend's, and the value live across is kept out of rax and
\ rdx because the operation it crosses writes both - a fixed result destroys its
\ register exactly as a call destroys the ones it may.
: IDIV-BODY ( IR-CTX:ctx -- n n n n n bool )
   X64-MOD
   0 1 OPEN-FUN
   100 M-MOVI {: d:IR-ID:ir-value-id :}
   7 M-MOVI {: v:IR-ID:ir-value-id :}
   9 M-MOVI {: k:IR-ID:ir-value-id :}
   d v M-IDIV {: q:IR-ID:ir-value-id r:IR-ID:ir-value-id :}
   q r M-ADD {: s1:IR-ID:ir-value-id :}
   s1 k M-ADD M-RET
   CLOSE-FUN
   M-ALLOCATED drop
   0 A64RAV:REG@
   1 A64RAV:REG@
   2 A64RAV:REG@
   3 A64RAV:REG@
   4 A64RAV:REG@
   A64RAV:ACCEPTED? ;

\ THE DIVISOR IS EARLY-CLOBBERED. The same division with the value live across it
\ defined FIRST, so it takes rcx and rdx is the lowest register still free where
\ the divisor is placed. The divisor dies AT the divide and crosses nothing, and
\ yet rdx is not its to have: the form renders `cqo; idiv r64` and the cqo has
\ written rdx before the divide reads its divisor. It is placed in rsi, the next
\ register of the pool, and the only operand a fixed-result register is left to is
\ the dividend, which the form declares INTO rax.
: EARLY-CLOBBER-BODY ( IR-CTX:ctx -- n n n n n bool )
   X64-MOD
   0 1 OPEN-FUN
   9 M-MOVI {: k:IR-ID:ir-value-id :}
   100 M-MOVI {: d:IR-ID:ir-value-id :}
   7 M-MOVI {: v:IR-ID:ir-value-id :}
   d v M-IDIV {: q:IR-ID:ir-value-id r:IR-ID:ir-value-id :}
   q r M-ADD {: s1:IR-ID:ir-value-id :}
   s1 k M-ADD M-RET
   CLOSE-FUN
   M-ALLOCATED drop
   0 A64RAV:REG@
   1 A64RAV:REG@
   2 A64RAV:REG@
   3 A64RAV:REG@
   4 A64RAV:REG@
   A64RAV:ACCEPTED? ;

\ Two counts, both fixed to rcx, live over one interval. One register cannot
\ hold both, and this lane inserts no copy: the allocation is refused by name.
\ The copy that makes such a pair satisfiable is the selector's.
: TWO-COUNTS-BODY ( IR-CTX:ctx -- )
   X64-MOD
   0 1 OPEN-FUN
   $40 M-MOVI {: v:IR-ID:ir-value-id :}
   3 M-MOVI {: c1:IR-ID:ir-value-id :}
   5 M-MOVI {: c2:IR-ID:ir-value-id :}
   v c1 M-SHL {: s1:IR-ID:ir-value-id :}
   s1 c2 M-SHL M-RET
   CLOSE-FUN
   M-ALLOCATE ;

\ A form of this module's own table that fixes its operand to rbx, the register
\ the running engine keeps its interpreter in and the routine's pool therefore
\ does not hold. The schema stores any register a file could number; WHICH file
\ and which of its registers this routine may write is known here, and the
\ allocator refuses.
: RESERVED-SCHEMA ( -- IR-ID:ir-symbol-id )
   CC BB s" x64.keepbx" IR-BUILD:INTERN-SYMBOL {: op:IR-ID:ir-symbol-id :}
   op IR-SCHEMA:BEGIN-OP
   CC BB X64IR:GPR-TYPE IR-SCHEMA:ADD-OPERAND
   CC BB X64IR:GPR-TYPE IR-SCHEMA:ADD-RESULT
   IR--SCHEMA-SIDE:OPERAND 0 X64ASM:RBX X64ASM:R64>N IR-SCHEMA:ADD-FIXED
   false 0 0 IR-SCHEMA:SET-CONTROL
   IR-SCHEMA:SET-PURE
   false IR-SCHEMA:SET-TRAP
   CTARGET-ARCH:X86-64 CTARGET:F-BASE IR-SCHEMA:SET-TARGET
   CC BB s" x64.rule.keepbx" IR-BUILD:INTERN-SYMBOL IR-SCHEMA:SET-RULE
   CC BB s" x64.render.keepbx" IR-BUILD:INTERN-SYMBOL IR-SCHEMA:SET-RENDERER
   CC BB IR-BUILD:DEFINE-OP
   op ;

: M-UNARY ( IR-ID:ir-symbol-id IR-ID:ir-value-id -- IR-ID:ir-value-id )
   {: op:IR-ID:ir-symbol-id v:IR-ID:ir-value-id :}
   CC BB op IR-BUILD:BEGIN-OP
   CC BB  BODY-ST BODY-LN SPN  IR-BUILD:SET-OP-SPAN
   CC BB v IR-BUILD:ADD-OPERAND
   M-RESULT+
   CLOSE-VALUE ;

: RESERVED-BODY ( IR-CTX:ctx -- )
   X64-MOD
   RESERVED-SCHEMA {: op:IR-ID:ir-symbol-id :}
   0 1 OPEN-FUN
   7 M-MOVI {: a:IR-ID:ir-value-id :}
   op a M-UNARY M-RET
   CLOSE-FUN
   M-ALLOCATE ;

: TWO-COUNTS ( -- )
   WBND [: TWO-COUNTS-BODY ;] IR-CTX:WITH-CONTEXT ;

: RESERVED-FIX ( -- )
   WBND [: RESERVED-BODY ;] IR-CTX:WITH-CONTEXT ;

: FIXED-REFUSE-CASES ( -- )
   s" two values that both demand the count register over one interval are refused: one register holds one value and this pass inserts no copy" T-LABEL
   [: TWO-COUNTS ;] E-A64RA-FIXED TTHROWSQ

   s" a form fixing an operand to a register the routine may not write is refused where the file is known, which is not the schema" T-LABEL
   [: RESERVED-FIX ;] E-A64RA-FIXED TTHROWSQ ;

: GROUP-FIXED ( IR-CTX:ctx -- )       drop FIXED-REFUSE-CASES ;

\ ---- the cases ---------------------------------------------------------------
: DIFF-BODY ( IR-CTX:ctx -- n n n n n bool )
   HIR-MOD
   BUILD-DIFF
   ALLOCATED drop
   A64RA:VALUES
   0 A64RAV:REG@
   1 A64RAV:REG@
   2 A64RAV:REG@
   A64RA:PLAN-N
   A64RAV:ACCEPTED? ;

\ The boundary is three memory tokens and a fourth the publish leaves, so the
\ values the allocator places are the two loaded arguments and the difference:
\ 1, 3 and 5. The other four are tokens and REG@ refuses them by class.
: DSTACK-BODY ( IR-CTX:ctx -- n n n n n bool )
   HIR-MOD
   BUILD-DIFF
   DSTACK-ALLOCATED drop
   A64RA:VALUES
   1 A64RAV:REG@
   3 A64RAV:REG@
   5 A64RAV:REG@
   A64RA:PLAN-N
   A64RAV:ACCEPTED? ;

\ The routine that leaves through its callee, through the same two passes. The
\ entry loads the argument out of cell 0 and the call site passes it in cell 0
\ again, so a selector that re-stores it writes a value the cell already holds
\ and the validator refuses the module by name: this case threw
\ E-A64RAV-DKEEP (-8611) at that store until X64SEL:CALL-SAVE read the residency
\ map. With the store elided the load has no reader either, so the whole body is
\ the take and the branch.
: TAIL-BODY ( IR-CTX:ctx -- bool )
   HIR-MOD
   BUILD-CALLER
   TAIL-ALLOCATED drop
   A64RAV:ACCEPTED? ;

\ The loop under the data-stack convention, where the residency of a cell is a
\ MEET over the block's predecessors and not a block-local memory: the backedge
\ carries the header's argument round, so what cell 1 holds at the header is
\ what both edges say it holds. The validator keeps the same map over the same
\ graph and refuses an emission that disagrees with it, which is what this case
\ holds the selector's own fixpoint to.
: DLOOP-BODY ( IR-CTX:ctx -- bool )
   HIR-MOD
   BUILD-LOOP
   DSTACK-ALLOCATED drop
   A64RAV:ACCEPTED? ;

: SQUARE-BODY ( IR-CTX:ctx -- n n n n n bool )
   HIR-MOD
   BUILD-SQUARE
   ALLOCATED drop
   A64RA:VALUES
   0 A64RAV:REG@
   1 A64RAV:REG@
   2 A64RAV:REG@
   A64RA:PLAN-N
   A64RAV:ACCEPTED? ;

\ ---- and the same two forms as the SELECTOR leaves them ----------------------
\ The count is copied and the COPY is what the form fixes to rcx, so the count
\ itself is placed like any other value - and because nothing else reads it, the
\ copy is coalesced back into it and one class holds rcx over both. The value
\ shifted is tied to the result and keeps its own register.
: VAR-SHIFT-BODY ( IR-CTX:ctx -- n n n n bool )
   HIR-MOD
   BUILD-VAR-SHIFT
   ALLOCATED drop
   0 A64RAV:REG@
   1 A64RAV:REG@
   2 A64RAV:REG@
   3 A64RAV:REG@
   A64RAV:ACCEPTED? ;

\ Two variable shifts whose counts are both live at the first of them, which the
\ direct-dialect case above refuses when no copy stands between them. Here the
\ selector's copies make the pair placeable: the first count's copy is coalesced
\ back into it and holds rcx to the first shift, and the second count waits in a
\ register of its own until its own copy takes rcx after the first is dead.
: TWO-SHIFTS-BODY ( IR-CTX:ctx -- n n n n n n n bool )
   HIR-MOD
   BUILD-TWO-SHIFTS
   ALLOCATED drop
   0 A64RAV:REG@
   1 A64RAV:REG@
   2 A64RAV:REG@
   3 A64RAV:REG@
   4 A64RAV:REG@
   5 A64RAV:REG@
   6 A64RAV:REG@
   A64RAV:ACCEPTED? ;

\ The division the selector lowers: the dividend's copy is coalesced into the
\ argument and holds rax, the divisor is placed out of rax and rdx, the quotient
\ is rax again and the remainder is the rdx result NOTHING reads - a result with
\ no reader is placed and validated like any other.
: DIVIDE-BODY ( IR-CTX:ctx -- n n n n n bool )
   HIR-MOD
   BUILD-DIV
   ALLOCATED drop
   0 A64RAV:REG@
   1 A64RAV:REG@
   2 A64RAV:REG@
   3 A64RAV:REG@
   4 A64RAV:REG@
   A64RAV:ACCEPTED? ;

\ The loop above through the same two passes: the copy selection inserts for a
\ value live around the backedge is what makes the tie satisfiable, and the
\ allocation the module gets is one the validator accepts.
: LOOP-BODY ( IR-CTX:ctx -- bool )
   HIR-MOD
   BUILD-LOOP
   ALLOCATED drop
   A64RAV:ACCEPTED? ;

\ ---- the vocabulary is one module's, and only that module's ------------------
\ Every name in a vocabulary is an ordinal of the module it was interned in, so
\ a vocabulary handed to another module's binding names nothing there. The
\ binding refuses before it copies a single name out.
: WRONG-VOCAB-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   IR-BUILD:PLAN-BEGIN
   IR-BUILD:PLAN-DEFAULT
   c A64IR:NEW-BUILDER {: b:IR-BUILD:builder :}
   c b A64IR:MACHINE  c b X64IR:VOCABULARY  A64RA:BIND-DIALECT ;

: WRONG-VOCAB ( -- )
   ABND [: WRONG-VOCAB-BODY ;] IR-CTX:WITH-CONTEXT ;

\ The key this dialect does not have, asked for anyway. x64ir declares the
\ write-back key ABSENT because x86-64 has no write-back addressing and interns
\ no symbol for one, and the reader refuses rather than answering with a number
\ that would read as a symbol here.
: ABSENT-KEY ( -- )
   NDIALECT-OPTSYM:ABSENT NDIALECT:SYM drop ;

: VOCAB-REFUSE-CASES ( -- )
   s" one dialect's vocabulary bound to another dialect's module is refused, because its names are that module's ordinals and name nothing here" T-LABEL
   [: WRONG-VOCAB ;] E-A64RA-MODULE TTHROWSQ

   s" a key the dialect declared absent is refused rather than answered: x64ir says `absent` for the write-back key every data-stack case above reads through" T-LABEL
   [: ABSENT-KEY ;] E-NDIALECT TTHROWSQ ;

\ A refusing case runs INSIDE an enclosing context: an abandoned context gives
\ its registry slots back only when a live enclosing context leaves normally.
: GROUP-VOCAB ( IR-CTX:ctx -- )       drop VOCAB-REFUSE-CASES ;

public

: RUN ( -- )
   T-RESET

   s" two arguments take two registers and the difference reuses the first: the allocator reads this module through the x86-64 vocabulary alone" T-LABEL
   WBND [: DIFF-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE 0 T= 0 T= 1 T= 0 T= 3 T=

   s" the copy a two-address form needs takes a register of its own: the allocator found it by the vocabulary's own copy opcode, x64.mov" T-LABEL
   WBND [: SQUARE-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE 0 T= 1 T= 1 T= 0 T= 3 T=

   s" the same leaf under the data-stack convention allocates and is accepted: the validator measures the stand against the `entry-base` policy x64ir states, where re-deriving A64SEL's survey refused this module with E-A64RAV-DSTACK" T-LABEL
   WBND [: DSTACK-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE 0 T= 0 T= 1 T= 0 T= 7 T=

   s" a routine that leaves through its callee allocates and is accepted: the argument stays in the cell it was entered in, and a store of a value the cell already holds is what the validator refuses" T-LABEL
   WBND [: TAIL-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE

   s" a loop under the data-stack convention allocates and is accepted: which value a cell holds at the header is the meet of both edges, and the validator keeps that same map over the same graph" T-LABEL
   WBND [: DLOOP-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE

   s" the count of a variable shift is placed in rcx because the form fixes it there, and the value shifted takes the lowest free register" T-LABEL
   WBND [: SHL-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE 1 T= 0 T=

   s" a division puts its dividend and quotient in rax and its remainder in rdx, and a value live across it is placed in neither" T-LABEL
   WBND [: IDIV-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE 2 T= 0 T= 6 T= 1 T= 0 T=

   s" the divisor is kept out of rdx as well, because the form writes rdx before it reads its divisor: it takes rsi where rdx was free" T-LABEL
   WBND [: EARLY-CLOBBER-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE 2 T= 0 T= 6 T= 0 T= 1 T=

   s" a variable shift the selector lowered places the count's copy in rcx and coalesces the copy into the count nothing else reads" T-LABEL
   WBND [: VAR-SHIFT-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE 0 T= 1 T= 1 T= 0 T=

   s" two variable shifts whose counts are both live at the first allocate, because the second count's copy is NOT coalesced into a count another class already wants the register for" T-LABEL
   WBND [: TWO-SHIFTS-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE 0 T= 1 T= 0 T= 1 T= 2 T= 1 T= 0 T=

   s" a division the selector lowered places the dividend's copy in rax, the divisor out of rax and rdx, and the remainder nothing reads in rdx" T-LABEL
   WBND [: DIVIDE-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE 2 T= 0 T= 0 T= 1 T= 0 T=

   s" a value read once inside a loop and live around its backedge allocates: selection copied the operand the add destroys, and a tie whose ends overlap is what this allocator refuses rather than repairs" T-LABEL
   WBND [: LOOP-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE

   WBND [: GROUP-VOCAB ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-FIXED ;] IR-CTX:WITH-CONTEXT

   T-REPORT ;

;package

X64RA-TEST:RUN

\ x64-select.f - checked x86-64 instruction selection tests.
\
\ Proves the contract of src/compiler/native/select-x64.f: a frozen HIR module
\ becomes a frozen X64IR module in which every source operation has become the
\ machine operations that compute it, every source value has become the value
\ the last of those operations defines, and every operand names that value
\ rather than a position; and a float, a trapping unit, an opcode with no rule,
\ a module the pass was not told about and a contract of another machine are each
\ refused by name.
\
\ WHAT THESE FIXTURES MEASURE THAT THE ARM64 ONES CANNOT. Three answers are this
\ machine's alone and a wrong table would pass every ARM64 case:
\
\ - THE IMMEDIATE FORMS. `8 +` is ONE x86-64 instruction and the literal never
\   becomes a `x64.movi` at all, where ARM64 needs a move-wide chain unless the
\   constant fits its own narrower field. The case asserts the operation COUNT
\   as well as the opcode, because a literal that was materialised and then also
\   folded would carry the right opcode and one instruction too many.
\ - THE UNFUSED COMPARE AND BRANCH. This slice selects `x64.cmpset` and then
\   `x64.brz` where the ARM64 pass fuses the pair into one `a64.cmpbr`. The case
\   asserts both operations and both successors, so the fusing slice that comes
\   next changes this case deliberately rather than silently.
\ - THE COPY A TWO-ADDRESS FORM NEEDS. `add rd, rs` destroys rd, so an operand
\   that is still LIVE after the form is copied with `x64.mov` before it, and an
\   operand the form is the last reader of is not. Liveness and not a use count:
\   the loop case below reads its operand once and still needs the copy, because
\   the backedge brings control round to read it again. ARM64's three-register
\   forms destroy nothing, so no ARM64 fixture measures it.
\ - THE COPY A FIXED REGISTER NEEDS. `shl r64, cl` reads its count from rcx and
\   `idiv r64` its dividend from rax, so the count and the dividend are copied
\   with `x64.mov` ALWAYS - the copy is the value the allocator places in the
\   named register, and a count the rest of the function reads is then repaired
\   rather than refused. The cases assert that the form's operand is the copy and
\   not the source value. ARM64's sdiv and lslv name no register at all.
\ - THE NEGATIVE DATA-STACK OFFSET. In a routine that leaves through its callee
\   the pointer never moves, so every argument is read BEHIND it. x86-64
\   displacements are signed and ARM64's are not, which is why the tail case
\   asserts the offset and not only the opcode.
\
\ WHY THE POSITIVE CASES BIND A WRAPPING UNIT. Whether a Habu `+` traps is the
\ compilation unit's overflow policy, and the source dialect records the answer
\ in its arithmetic schemas. x86-64's add wraps and this dialect has no trapping
\ form, so a trapping unit is refused - which is a case of its own below.
\
\ ONE FIXTURE PER CONTEXT, and a refusing case runs inside an enclosing one: an
\ abandoned context gives its registry slots back only when a live enclosing
\ context leaves normally (src/compiler/ir/context.f, the note on stale handles).

require lib/test.f
require src/compiler/native/select-x64.f
require src/arch/x86-64/abi.f
require src/arch/arm64/machine.f

package X64SEL-TEST
private

\ ---- bindings ----------------------------------------------------------------
\ A linux x86-64 contract whose integer overflow wraps: the machine's own
\ behaviour, and the one this dialect can select arithmetic under.
: WBND ( -- CBIND:binding )
   CTARGET-ARCH:X86-64 CTARGET-ABI:SYSV-AMD64 CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64
   CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH CTARGET:CONTRACT
   CNUM-OVERFLOW:WRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY
   CBIND:BIND ;

\ The same machine with a trapping overflow policy.
: TBND ( -- CBIND:binding )
   CTARGET-ARCH:X86-64 CTARGET-ABI:SYSV-AMD64 CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64
   CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH CTARGET:CONTRACT
   CNUM-OVERFLOW:TRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY
   CBIND:BIND ;

\ ---- the fixture's source text -----------------------------------------------
\ One text stands behind every fixture, so each span a fixture attaches is a real
\ byte range in bytes the module has really registered.
create TXT
   58 c, 32 c, 83 c, 81 c, 85 c, 65 c, 82 c, 69 c,            \ ": SQUARE"
   32 c, 100 c, 117 c, 112 c, 32 c, 42 c, 32 c, 59 c,         \ " dup * ;"
16 constant TXT-N

2 constant NAME-ST                   \ the defined name inside TXT
6 constant NAME-LN
0 constant OPEN-ST                   \ the opening `:`
1 constant OPEN-LN
9 constant BODY-ST                   \ the body word
3 constant BODY-LN
15 constant CLOSE-ST                 \ the closing `;`
1 constant CLOSE-LN

$1000 constant BUMP-ADDR             \ a fixed address the memory fixture uses
$400 constant CALLEE-ENTRY           \ an address; nothing here branches to it

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

: REALT ( -- IR-ID:ir-type-id )
   CC BB HIR:REAL-TYPE ;

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

: UNOP ( HIR:opcode IR-ID:ir-value-id -- IR-ID:ir-value-id )
   {: o:HIR:opcode x:IR-ID:ir-value-id :}
   o BODY-ST BODY-LN OPEN-OP
   CC BB x IR-BUILD:ADD-OPERAND
   CC BB CELLT IR-BUILD:ADD-RESULT
   CLOSE-VALUE ;

: CONSTOP ( n -- IR-ID:ir-value-id )
   {: v:n :}
   HIR-OPCODE:CONST BODY-ST BODY-LN OPEN-OP
   CC BB CELLT IR-BUILD:ADD-RESULT
   CC BB  CC BB HIR:KEY-VALUE  CC BB v IR-BUILD:INTERN-INT-ATTR
   IR-BUILD:ADD-ATTR
   CC BB  CC BB HIR:KEY-ADDR  CC BB HIR:ADDR-NONE HIR:ADDR-ATTR
   IR-BUILD:ADD-ATTR
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
   CC BB  CC BB s" SQUARE" IR-BUILD:INTERN-SYMBOL  IR-BUILD:BEGIN-FUN
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

\ ---- the fixtures ------------------------------------------------------------
\ `: K ( -- n ) 7 ;` - one literal, which on this machine is one instruction
\ whatever the cell holds.
: BUILD-LIT ( -- )
   0 1 OPEN-FUN
   7 CONSTOP RET1
   CLOSE-FUN ;

\ `: SQUARE ( n -- n ) dup + ;` - one addition over the same value twice.
: BUILD-SQUARE ( -- )
   1 1 OPEN-FUN
   ARG+ {: a:IR-ID:ir-value-id :}
   HIR-OPCODE:ADD a a BINOP RET1
   CLOSE-FUN ;

\ `: DIFF ( n n -- n ) - ;` - two different arguments, so the order the operands
\ are wired in is visible. The subtraction is two-address and the result is tied
\ to the FIRST operand, which is the operand this case pins.
: BUILD-DIFF ( -- )
   2 1 OPEN-FUN
   ARG+ {: x:IR-ID:ir-value-id :}
   ARG+ {: y:IR-ID:ir-value-id :}
   HIR-OPCODE:SUB x y BINOP RET1
   CLOSE-FUN ;

: BUILD-ADD ( -- )
   2 1 OPEN-FUN
   ARG+ {: x:IR-ID:ir-value-id :}
   ARG+ {: y:IR-ID:ir-value-id :}
   HIR-OPCODE:ADD x y BINOP RET1
   CLOSE-FUN ;

\ `x y + x -`: the sum, and then the first argument taken off it again. The add
\ may not destroy `x`, because the subtraction below reads it; the subtraction
\ may destroy the sum, whose only reader it is. One function holds both answers.
: BUILD-REUSE ( -- )
   2 1 OPEN-FUN
   ARG+ {: x:IR-ID:ir-value-id :}
   ARG+ {: y:IR-ID:ir-value-id :}
   HIR-OPCODE:ADD x y BINOP {: sum:IR-ID:ir-value-id :}
   HIR-OPCODE:SUB sum x BINOP RET1
   CLOSE-FUN ;

\ `8 + x -`: the same reuse under the form that carries its second operand as an
\ immediate. The literal still folds into the instruction and the copy is the
\ operand the instruction destroys, which is the only thing the imm form changes.
: BUILD-IMM-REUSE ( -- )
   1 1 OPEN-FUN
   ARG+ {: x:IR-ID:ir-value-id :}
   8 CONSTOP {: k:IR-ID:ir-value-id :}
   HIR-OPCODE:ADD x k BINOP {: sum:IR-ID:ir-value-id :}
   HIR-OPCODE:SUB sum x BINOP RET1
   CLOSE-FUN ;

\ `dup invert and`: the unary's operand is read again below it, and a unary form
\ is tied exactly as a binary one is, so it takes the copy too.
: BUILD-MASK ( -- )
   1 1 OPEN-FUN
   ARG+ {: x:IR-ID:ir-value-id :}
   HIR-OPCODE:INVERT x UNOP {: m:IR-ID:ir-value-id :}
   HIR-OPCODE:AND m x BINOP RET1
   CLOSE-FUN ;

\ The three logical forms and the one unary, in one function: each is the same
\ shape and a table that swapped two of them fails on the opcode.
: BUILD-LOGIC ( -- )
   2 1 OPEN-FUN
   ARG+ {: x:IR-ID:ir-value-id :}
   ARG+ {: y:IR-ID:ir-value-id :}
   HIR-OPCODE:AND x y BINOP {: a1:IR-ID:ir-value-id :}
   HIR-OPCODE:OR a1 y BINOP {: a2:IR-ID:ir-value-id :}
   HIR-OPCODE:XOR a2 y BINOP {: a3:IR-ID:ir-value-id :}
   HIR-OPCODE:INVERT a3 UNOP RET1
   CLOSE-FUN ;

\ `: BUMP ( n -- n ) 8 + ;` - the literal the instruction carries.
: BUILD-IMM-ADD ( -- )
   1 1 OPEN-FUN
   ARG+ {: x:IR-ID:ir-value-id :}
   8 CONSTOP {: k:IR-ID:ir-value-id :}
   HIR-OPCODE:ADD x k BINOP RET1
   CLOSE-FUN ;

\ `: OCT ( n -- n ) 3 lshift ;` - the shift the machine takes an immediate for.
: BUILD-SHIFT ( -- )
   1 1 OPEN-FUN
   ARG+ {: x:IR-ID:ir-value-id :}
   3 CONSTOP {: k:IR-ID:ir-value-id :}
   HIR-OPCODE:LSHIFT x k BINOP RET1
   CLOSE-FUN ;

\ `: VSH ( n n -- n ) lshift ;` - the count in a register, which is rcx and
\ nowhere else.
: BUILD-VAR-SHIFT ( -- )
   2 1 OPEN-FUN
   ARG+ {: x:IR-ID:ir-value-id :}
   ARG+ {: y:IR-ID:ir-value-id :}
   HIR-OPCODE:LSHIFT x y BINOP RET1
   CLOSE-FUN ;

\ `: QUOT ( n n -- n ) / ;` - rdx:rax, for the same reason.
: BUILD-DIV ( -- )
   2 1 OPEN-FUN
   ARG+ {: x:IR-ID:ir-value-id :}
   ARG+ {: y:IR-ID:ir-value-id :}
   HIR-OPCODE:DIV x y BINOP RET1
   CLOSE-FUN ;

\ `: PICK ( a b -- n ) < if a else b then ;` as the elaborator leaves the
\ branch: the entry compares and branches and each arm returns. There is no join
\ block, because a two-way branch carries no values and each arm leaves.
: BUILD-BRANCH ( -- )
   2 1 OPEN-FUN
   ARG+ {: x:IR-ID:ir-value-id :}
   ARG+ {: y:IR-ID:ir-value-id :}
   HIR-OPCODE:LT x y BINOP {: f:IR-ID:ir-value-id :}
   f 1 2 BRZ2
   BLOCK+
   x RET1
   BLOCK+
   y RET1
   CLOSE-FUN ;

\ The same two arguments added and handed on over an edge: the second block
\ takes the sum as its argument and returns it. This is the only shape the
\ machine branch has an operand for, and the value reaches the argument through
\ two copies - every source on the edge is read before any destination is
\ written, so a branch that permuted its block's own arguments could not destroy
\ one of them (select-x64.f, "splitting the edges that carry values").
: BUILD-CARRY ( -- )
   2 1 OPEN-FUN
   ARG+ {: x:IR-ID:ir-value-id :}
   ARG+ {: y:IR-ID:ir-value-id :}
   HIR-OPCODE:ADD x y BINOP {: s:IR-ID:ir-value-id :}
   s 1 BR1
   BLOCK+
   ARG+ RET1
   CLOSE-FUN ;

\ A loop whose header reads a value defined ABOVE it: `B0(x, c0): br B1(c0);
\ B1(c): t = add x c; brz t -> B2 / B3; B2: ret t; B3: br B1(t)`. The add is the
\ only operand in the function that names `x`, and the backedge carries `t` and
\ not `x`, so no count of textual uses can see that the next pass through the
\ header reads `x` again. The add may not destroy it.
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

\ The same two blocks with nothing re-entering the second: `B0(x, c0): br
\ B1(c0); B1(c): t = add x c; ret t`. The add IS the last reader of `x` here and
\ takes it as operand 0 with no copy at all, which the operation count pins.
: BUILD-STRAIGHT ( -- )
   2 1 OPEN-FUN
   ARG+ {: x:IR-ID:ir-value-id :}
   ARG+ {: c0:IR-ID:ir-value-id :}
   c0 1 BR1
   BLOCK+
   ARG+ {: c:IR-ID:ir-value-id :}
   HIR-OPCODE:ADD x c BINOP RET1
   CLOSE-FUN ;

\ `: ZERO? ( n -- bool ) 0= ;` - the comparison against a literal, which is one
\ instruction with the zero inside it.
: BUILD-ZEROP ( -- )
   1 1 OPEN-FUN
   ARG+ {: x:IR-ID:ir-value-id :}
   0 CONSTOP {: k:IR-ID:ir-value-id :}
   HIR-OPCODE:EQUAL x k BINOP RET1
   CLOSE-FUN ;

\ `: PASS ( n -- n ) ;` - a routine that hands its argument straight back, which
\ is the whole data-stack boundary and nothing else.
: BUILD-PASS ( -- )
   1 1 OPEN-FUN
   ARG+ RET1
   CLOSE-FUN ;

: MEM0 ( -- IR-ID:ir-value-id )
   HIR-OPCODE:MEM BODY-ST BODY-LN OPEN-OP
   CC BB MEMT IR-BUILD:ADD-RESULT
   CLOSE-VALUE ;

: STORE1 ( HIR:opcode IR-ID:ir-value-id IR-ID:ir-value-id IR-ID:ir-value-id -- IR-ID:ir-value-id )
   {: o:HIR:opcode v:IR-ID:ir-value-id a:IR-ID:ir-value-id k:IR-ID:ir-value-id :}
   o BODY-ST BODY-LN OPEN-OP
   CC BB v IR-BUILD:ADD-OPERAND
   CC BB a IR-BUILD:ADD-OPERAND
   CC BB k IR-BUILD:ADD-OPERAND
   CC BB MEMT IR-BUILD:ADD-RESULT
   CLOSE-VALUE ;

: LOAD1 ( HIR:opcode IR-ID:ir-value-id IR-ID:ir-value-id -- IR-ID:ir-value-id IR-ID:ir-value-id )
   {: o:HIR:opcode a:IR-ID:ir-value-id k:IR-ID:ir-value-id :}
   o BODY-ST BODY-LN OPEN-OP
   CC BB a IR-BUILD:ADD-OPERAND
   CC BB k IR-BUILD:ADD-OPERAND
   CC BB CELLT IR-BUILD:ADD-RESULT
   CC BB MEMT IR-BUILD:ADD-RESULT
   CC BB IR-BUILD:END-OP {: id:IR-ID:ir-op-id :}
   CC BB id 0 IR-BUILD:OP-RESULT@
   CC BB id 1 IR-BUILD:OP-RESULT@ ;

\ `: POKE ( n -- n ) A ! A @ A c! A c@ ;` with A a fixed address: the cell store
\ and load and then the byte pair, each taking the order the one before it
\ answered. The address is ONE literal with four readers, so it is materialised
\ once and folded into nothing: no addressed form of this dialect carries one.
: BUILD-MEMOPS ( -- )
   1 1 OPEN-FUN
   ARG+ {: x:IR-ID:ir-value-id :}
   MEM0 {: k0:IR-ID:ir-value-id :}
   BUMP-ADDR CONSTOP {: a0:IR-ID:ir-value-id :}
   HIR-OPCODE:STORE x a0 k0 STORE1 {: k1:IR-ID:ir-value-id :}
   HIR-OPCODE:LOAD a0 k1 LOAD1 {: got:IR-ID:ir-value-id k2:IR-ID:ir-value-id :}
   HIR-OPCODE:BSTORE got a0 k2 STORE1 {: k3:IR-ID:ir-value-id :}
   HIR-OPCODE:BLOAD a0 k3 LOAD1 {: b:IR-ID:ir-value-id k4:IR-ID:ir-value-id :}
   b RET1
   CLOSE-FUN ;

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

\ The same module twice over: under a contract that returns it is a call site,
\ and under one that leaves through its callee it is a tail branch. Nothing the
\ site carries is read again, which is what a tail branch needs.
: BUILD-CALLER ( -- )
   1 1 OPEN-FUN
   ARG+ {: a:IR-ID:ir-value-id :}
   MEM0 {: tok:IR-ID:ir-value-id :}
   tok a a WCALL1 {: id:IR-ID:ir-op-id :}
   CC BB id 2 IR-BUILD:OP-RESULT@ RET1
   CLOSE-FUN ;

\ `: F ( n -- n ) intreal realbits ;` - a floating operation, which this dialect
\ has no form for at all.
: BUILD-FLOAT ( -- )
   1 1 OPEN-FUN
   ARG+ {: x:IR-ID:ir-value-id :}
   HIR-OPCODE:INTREAL BODY-ST BODY-LN OPEN-OP
   CC BB x IR-BUILD:ADD-OPERAND
   CC BB REALT IR-BUILD:ADD-RESULT
   CLOSE-VALUE {: r:IR-ID:ir-value-id :}
   HIR-OPCODE:REALBITS BODY-ST BODY-LN OPEN-OP
   CC BB r IR-BUILD:ADD-OPERAND
   CC BB CELLT IR-BUILD:ADD-RESULT
   CLOSE-VALUE RET1
   CLOSE-FUN ;

\ An operation of an opcode the source dialect does not have, defined into the
\ same dialect's table. Nothing in the substrate forbids it and the module
\ verifies, so the pass has to refuse it by name rather than by never meeting it.
: EXTRA-SCHEMA ( -- IR-ID:ir-symbol-id )
   CC BB s" hir.negate" IR-BUILD:INTERN-SYMBOL {: op:IR-ID:ir-symbol-id :}
   op IR-SCHEMA:BEGIN-OP
   CELLT IR-SCHEMA:ADD-OPERAND
   CELLT IR-SCHEMA:ADD-OPERAND
   CELLT IR-SCHEMA:ADD-RESULT
   false 0 0 IR-SCHEMA:SET-CONTROL
   IR-SCHEMA:SET-PURE
   false IR-SCHEMA:SET-TRAP
   CTARGET-ARCH:X86-64 CTARGET:F-BASE IR-SCHEMA:SET-TARGET
   CC BB s" hir.rule.negate" IR-BUILD:INTERN-SYMBOL IR-SCHEMA:SET-RULE
   CC BB s" hir.render.negate" IR-BUILD:INTERN-SYMBOL IR-SCHEMA:SET-RENDERER
   CC BB IR-BUILD:DEFINE-OP
   op ;

: BUILD-EXTRA ( -- )
   EXTRA-SCHEMA {: op:IR-ID:ir-symbol-id :}
   2 1 OPEN-FUN
   ARG+ {: x:IR-ID:ir-value-id :}
   ARG+ {: y:IR-ID:ir-value-id :}
   CC BB op IR-BUILD:BEGIN-OP
   CC BB  BODY-ST BODY-LN SPN  IR-BUILD:SET-OP-SPAN
   CC BB x IR-BUILD:ADD-OPERAND
   CC BB y IR-BUILD:ADD-OPERAND
   CC BB CELLT IR-BUILD:ADD-RESULT
   CLOSE-VALUE RET1
   CLOSE-FUN ;

\ ---- running the pass --------------------------------------------------------
: X64-BUILDER ( -- IR-BUILD:builder )
   IR-BUILD:PLAN-BEGIN
   IR-BUILD:PLAN-DEFAULT
   CC X64IR:NEW-BUILDER ;

\ The contract the body cases select under: the convention names no place at
\ all, so the selector adds no entry and no exit and what the case reads back is
\ exactly what the source module's operations selected to. The pool is the
\ machine's own nine, because a routine that may write no register cannot hold
\ the values a body computes.
: NO-PLACES ( -- NEFF:routine )
   NEFF-CONV:REGISTER NEFF:SEQ-NONE NEFF:SEQ-NONE X64M:MACHINE NEFF:GPR-ALL
   NEFF:FPR-NONE NEFF:FPR-NONE NEFF:FPR-NONE
   NEFF-NZCV:UNTOUCHED NEFF-LINK:ABSENT NEFF-CONTROL:RETURNS
   NEFF:TRAITS-NONE 0 0 X64M:MACHINE NEFF:ROUTINE ;

\ A contract of ANOTHER machine, which this backend may not lower for: every
\ number in it - the register file, the frame bound, the displacement - means
\ something else.
: A64-CONV ( -- NEFF:routine )
   NEFF-CONV:REGISTER NEFF:SEQ-NONE NEFF:SEQ-NONE NEFF:GPR-NONE
   NEFF:FPR-NONE NEFF:FPR-NONE NEFF:FPR-NONE
   NEFF-NZCV:UNTOUCHED NEFF-LINK:PRESERVED NEFF-CONTROL:RETURNS
   NEFF:TRAITS-NONE 0 0 A64M:MACHINE NEFF:ROUTINE ;

: SELECTED ( -- IR-BUILD:module )
   CC BB X64SEL:BIND-SOURCE
   CC BB IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   CC m X64-BUILDER NO-PLACES X64SEL:SELECT ;

: SELECTED-LEAF ( n n -- IR-BUILD:module )
   {: in:n out:n :}
   CC BB X64SEL:BIND-SOURCE
   CC BB IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   CC m X64-BUILDER  X64ABI:SCRATCH in out X64ABI:LEAF  X64SEL:SELECT ;

: SELECTED-FRAMED ( n n n -- IR-BUILD:module )
   {: in:n out:n spills:n :}
   CC BB X64SEL:BIND-SOURCE
   CC BB IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   CC m X64-BUILDER  X64ABI:SCRATCH in out spills X64ABI:LEAF-FRAMED
   X64SEL:SELECT ;

: SELECTED-CALL ( n n -- IR-BUILD:module )
   {: in:n out:n :}
   CC BB X64SEL:BIND-SOURCE
   CC BB IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   CC m X64-BUILDER  X64ABI:SCRATCH in out X64ABI:CALL  X64SEL:SELECT ;

: SELECTED-TAIL ( n n -- IR-BUILD:module )
   {: in:n out:n :}
   CC BB X64SEL:BIND-SOURCE
   CC BB IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   CC m X64-BUILDER  X64ABI:SCRATCH in out X64ABI:TAIL  X64SEL:SELECT ;

: SELECTED-A64 ( -- IR-BUILD:module )
   CC BB X64SEL:BIND-SOURCE
   CC BB IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   CC m X64-BUILDER A64-CONV X64SEL:SELECT ;

\ ---- reading the selected module ---------------------------------------------
1 TYPED-BUFFER R-KEY IR-ID:ir-module-key
7 constant R-VIEWS
0 constant R-SYMP
1 constant R-SYMR
2 constant R-ATTR
3 constant R-OPP
4 constant R-OPR
5 constant R-VALR
6 constant R-BLKR
R-VIEWS TYPED-BUFFER R-VIEW IR-ARENA:view
1 TYPED-BUFFER R-FUNR IR-ARENA:view

: RV ( n -- IR-ARENA:view )          R-VIEW @ ;
: RK ( -- IR-ID:ir-module-key )      0 R-KEY @ ;
: RF ( -- IR-ARENA:view )            0 R-FUNR @ ;

: READ! ( IR-BUILD:module -- )
   {: m:IR-BUILD:module :}
   m IR-BUILD:FKEY 0 R-KEY !
   m IR-BUILD:FSYM-POOL   R-SYMP R-VIEW !
   m IR-BUILD:FSYM-ROWS   R-SYMR R-VIEW !
   m IR-BUILD:FATTR-ROWS  R-ATTR R-VIEW !
   m IR-BUILD:FOP-POOL    R-OPP  R-VIEW !
   m IR-BUILD:FOP-ROWS    R-OPR  R-VIEW !
   m IR-BUILD:FVALUE-ROWS R-VALR R-VIEW !
   m IR-BUILD:FBLOCK-ROWS R-BLKR R-VIEW !
   m IR-BUILD:FFUN-ROWS 0 R-FUNR ! ;

: BLKN ( n -- IR-ID:ir-block-id )
   {: b:n :}
   RF R-BLKR RV RK  RK 0 IR-ID:PACK-FUN  b IR-FUN:FBLOCK@ ;

: BLK0 ( -- IR-ID:ir-block-id )
   0 BLKN ;

: BOP@ ( n n -- IR-ID:ir-op-id )
   {: b:n i:n :}
   R-BLKR RV R-OPR RV RK  b BLKN  i IR-FUN:FOP@ ;

: BOPS ( n -- n )
   BLKN {: bk:IR-ID:ir-block-id :}
   R-BLKR RV bk IR-FUN:FOP-COUNT ;

: BOPCODE-IS? ( n n ptr u8 n -- bool )
   {: b:n i:n p u:n :}
   R-SYMP RV R-SYMR RV  R-OPR RV RK b i BOP@ IR-OP:FOPCODE@  p u IR-SYM:FEQ? ;

: BOPERAND@ ( n n n -- IR-ID:ir-value-id )
   {: b:n i:n k:n :}
   R-OPP RV R-OPR RV RK b i BOP@ k IR-OP:FOPERAND@ ;

: BRESULT@ ( n n n -- IR-ID:ir-value-id )
   {: b:n i:n k:n :}
   R-OPP RV R-OPR RV RK b i BOP@ k IR-OP:FRESULT@ ;

: BARG@ ( n n -- IR-ID:ir-value-id )
   {: b:n i:n :}
   R-BLKR RV R-VALR RV RK  b BLKN  i IR-FUN:FARG@ ;

: BARGS ( n -- n )
   BLKN {: bk:IR-ID:ir-block-id :}
   R-BLKR RV bk IR-FUN:FARG-COUNT ;

: OP@ ( n -- IR-ID:ir-op-id )
   {: i:n :}
   0 i BOP@ ;

: ARG@ ( n -- IR-ID:ir-value-id )
   {: i:n :}
   0 i BARG@ ;

: OPCODE-IS? ( n ptr u8 n -- bool )
   {: i:n p u:n :}
   R-SYMP RV R-SYMR RV  R-OPR RV RK i OP@ IR-OP:FOPCODE@  p u IR-SYM:FEQ? ;

: OPERAND@ ( n n -- IR-ID:ir-value-id )
   {: i:n k:n :}
   R-OPP RV R-OPR RV RK i OP@ k IR-OP:FOPERAND@ ;

: RESULT@ ( n n -- IR-ID:ir-value-id )
   {: i:n k:n :}
   R-OPP RV R-OPR RV RK i OP@ k IR-OP:FRESULT@ ;

: SAME-VALUE? ( IR-ID:ir-value-id IR-ID:ir-value-id -- bool )
   IR-ID:VALUE-LOCAL swap IR-ID:VALUE-LOCAL = ;

: OPS ( -- n )
   0 BOPS ;

: ARGS-N ( -- n )
   0 BARGS ;

: BLOCKS ( -- n )
   RF  RK 0 IR-ID:PACK-FUN  IR-FUN:FBLOCK-COUNT ;

: ATTR-INT ( n n -- n )
   {: i:n k:n :}
   R-ATTR RV  R-OPP RV R-OPR RV RK i OP@ k IR-OP:FATTR@  IR-ATTR:FINT@ ;

: ATTR-KEY-IS? ( n n ptr u8 n -- bool )
   {: i:n k:n p u:n :}
   R-SYMP RV R-SYMR RV
   R-OPP RV R-OPR RV RK i OP@ k IR-OP:FATTR-KEY@
   p u IR-SYM:FEQ? ;

: OPERANDS ( n -- n )
   {: i:n :}
   R-OPR RV i OP@ IR-OP:FOPERANDS ;

: SUCC@ ( n n -- n )
   {: i:n k:n :}
   R-OPP RV R-OPR RV RK i OP@ k IR-OP:FSUCCESSOR@ IR-ID:BLOCK-LOCAL ;

\ ---- the literal ------------------------------------------------------------
: LIT-BODY ( IR-CTX:ctx -- n bool bool n bool n bool bool )
   HIR-MOD
   BUILD-LIT
   SELECTED READ!
   OPS
   0 s" x64.movi" OPCODE-IS?
   0 0 s" x64.imm" ATTR-KEY-IS?
   0 0 ATTR-INT
   0 1 s" x64.addr" ATTR-KEY-IS?
   0 1 ATTR-INT
   1 s" x64.ret" OPCODE-IS?
   1 0 OPERAND@ 0 0 RESULT@ SAME-VALUE? ;

: LIT-CASE ( -- )
   s" a literal selects to the one instruction that holds a whole cell" T-LABEL
   WBND [: LIT-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE 0 T= TTRUE 7 T= TTRUE TTRUE 2 T= ;

\ ---- the arithmetic ---------------------------------------------------------
\ The tie names the operand the add destroys, so one value in both operands is
\ the case the copy exists for: the machine add reads the copy and the original.
\ The COUNT is asserted because a selection that dropped the copy would carry
\ these same opcodes with one instruction fewer.
: SQUARE-BODY ( IR-CTX:ctx -- n bool bool bool bool bool )
   HIR-MOD
   BUILD-SQUARE
   SELECTED READ!
   OPS
   0 s" x64.mov" OPCODE-IS?
   0 0 OPERAND@ 0 ARG@ SAME-VALUE?
   1 s" x64.add" OPCODE-IS?
   1 0 OPERAND@ 0 0 RESULT@ SAME-VALUE?
   1 1 OPERAND@ 0 ARG@ SAME-VALUE? ;

: SQUARE-CASE ( -- )
   s" an addition over one value twice copies it: the tie destroys operand 0" T-LABEL
   WBND [: SQUARE-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE TTRUE TTRUE 3 T= ;

\ Two different operands, and the rule still turns on the one being destroyed:
\ the add's `x` is read again by the subtraction below and is copied, while the
\ subtraction's own first operand is the sum it is the only reader of and stays.
: REUSE-BODY ( IR-CTX:ctx -- n bool bool bool bool bool )
   HIR-MOD
   BUILD-REUSE
   SELECTED READ!
   OPS
   0 s" x64.mov" OPCODE-IS?
   1 s" x64.add" OPCODE-IS?
   1 0 OPERAND@ 0 0 RESULT@ SAME-VALUE?
   2 s" x64.sub" OPCODE-IS?
   2 0 OPERAND@ 1 0 RESULT@ SAME-VALUE? ;

: REUSE-CASE ( -- )
   s" an operand read below the form is copied; a last-use operand is not" T-LABEL
   WBND [: REUSE-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE TTRUE TTRUE 4 T= ;

: IMM-REUSE-BODY ( IR-CTX:ctx -- n bool bool bool n bool bool )
   HIR-MOD
   BUILD-IMM-REUSE
   SELECTED READ!
   OPS
   0 s" x64.mov" OPCODE-IS?
   1 s" x64.addi" OPCODE-IS?
   1 0 OPERAND@ 0 0 RESULT@ SAME-VALUE?
   1 0 ATTR-INT
   2 s" x64.sub" OPCODE-IS?
   2 1 OPERAND@ 0 ARG@ SAME-VALUE? ;

: IMM-REUSE-CASE ( -- )
   s" the immediate form copies its destroyed operand and still folds the literal" T-LABEL
   WBND [: IMM-REUSE-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE 8 T= TTRUE TTRUE TTRUE 4 T= ;

: MASK-BODY ( IR-CTX:ctx -- n bool bool bool bool bool )
   HIR-MOD
   BUILD-MASK
   SELECTED READ!
   OPS
   0 s" x64.mov" OPCODE-IS?
   1 s" x64.not" OPCODE-IS?
   1 0 OPERAND@ 0 0 RESULT@ SAME-VALUE?
   2 s" x64.and" OPCODE-IS?
   2 1 OPERAND@ 0 ARG@ SAME-VALUE? ;

: MASK-CASE ( -- )
   s" a unary over a value read again copies it too: its form is tied as well" T-LABEL
   WBND [: MASK-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE TTRUE TTRUE 4 T= ;

: DIFF-BODY ( IR-CTX:ctx -- n bool bool bool bool bool )
   HIR-MOD
   BUILD-DIFF
   SELECTED READ!
   OPS
   0 s" x64.sub" OPCODE-IS?
   0 s" x64.add" OPCODE-IS?
   0 0 OPERAND@ 0 ARG@ SAME-VALUE?
   0 1 OPERAND@ 1 ARG@ SAME-VALUE?
   0 0 OPERAND@ 1 ARG@ SAME-VALUE? ;

: DIFF-CASE ( -- )
   s" a subtraction takes its operands in order and copies neither: both die here" T-LABEL
   WBND [: DIFF-BODY ;] IR-CTX:WITH-CONTEXT
   TFALSE TTRUE TTRUE TFALSE TTRUE 2 T= ;

: LOGIC-BODY ( IR-CTX:ctx -- n bool bool bool bool bool )
   HIR-MOD
   BUILD-LOGIC
   SELECTED READ!
   OPS
   0 s" x64.and" OPCODE-IS?
   1 s" x64.or" OPCODE-IS?
   2 s" x64.xor" OPCODE-IS?
   3 s" x64.not" OPCODE-IS?
   3 0 OPERAND@ 2 0 RESULT@ SAME-VALUE? ;

: LOGIC-CASE ( -- )
   s" each logical form is its own, and a chain of single readers copies none" T-LABEL
   WBND [: LOGIC-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE TTRUE TTRUE 5 T= ;

\ ---- the literal the instruction carries instead of a register ---------------
: IMM-ADD-BODY ( IR-CTX:ctx -- n bool bool bool bool n bool )
   HIR-MOD
   BUILD-IMM-ADD
   SELECTED READ!
   OPS
   0 s" x64.addi" OPCODE-IS?
   0 s" x64.movi" OPCODE-IS?
   0 s" x64.add" OPCODE-IS?
   0 0 s" x64.imm" ATTR-KEY-IS?
   0 0 ATTR-INT
   0 0 OPERAND@ 0 ARG@ SAME-VALUE? ;

: IMM-ADD-CASE ( -- )
   s" a literal second operand is the instruction's own and costs no move" T-LABEL
   WBND [: IMM-ADD-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE 8 T= TTRUE TFALSE TFALSE TTRUE 2 T= ;

: SHIFT-BODY ( IR-CTX:ctx -- n bool bool n )
   HIR-MOD
   BUILD-SHIFT
   SELECTED READ!
   OPS
   0 s" x64.shli" OPCODE-IS?
   0 0 s" x64.shift" ATTR-KEY-IS?
   0 0 ATTR-INT ;

: SHIFT-CASE ( -- )
   s" a shift by a literal is the immediate form and carries the count" T-LABEL
   WBND [: SHIFT-BODY ;] IR-CTX:WITH-CONTEXT
   3 T= TTRUE TTRUE 2 T= ;

\ ---- the two forms that name a register --------------------------------------
\ The count is copied and the copy is the shift's second operand, which is the
\ whole of how this pass asks for rcx: the count itself stays where it was and
\ what the allocator has to place in the named register is a value nothing else
\ reads.
: VAR-SHIFT-BODY ( IR-CTX:ctx -- n bool bool bool bool bool bool )
   HIR-MOD
   BUILD-VAR-SHIFT
   SELECTED READ!
   OPS
   0 s" x64.mov" OPCODE-IS?
   1 s" x64.shl" OPCODE-IS?
   0 0 OPERAND@ 1 ARG@ SAME-VALUE?
   1 0 OPERAND@ 0 ARG@ SAME-VALUE?
   1 1 OPERAND@ 0 0 RESULT@ SAME-VALUE?
   1 1 OPERAND@ 1 ARG@ SAME-VALUE? ;

: VAR-SHIFT-CASE ( -- )
   s" a shift by a computed count is the register form and its operand is the count's copy" T-LABEL
   WBND [: VAR-SHIFT-BODY ;] IR-CTX:WITH-CONTEXT
   TFALSE TTRUE TTRUE TTRUE TTRUE TTRUE 3 T= ;

\ The dividend is copied for the same reason and the quotient is result 0, which
\ is the value the source division named. The remainder is result 1 and nothing
\ reads it. The runtime routine the cold side hands a zero divisor to is the
\ operation's own attribute, and it is the dictionary's `throw`.
: DIVIDE-BODY ( IR-CTX:ctx -- n bool bool bool bool bool bool bool bool bool )
   HIR-MOD
   BUILD-DIV
   SELECTED READ!
   OPS
   0 s" x64.mov" OPCODE-IS?
   1 s" x64.idiv" OPCODE-IS?
   0 0 OPERAND@ 0 ARG@ SAME-VALUE?
   1 0 OPERAND@ 0 0 RESULT@ SAME-VALUE?
   1 1 OPERAND@ 1 ARG@ SAME-VALUE?
   1 0 s" x64.throw-entry" ATTR-KEY-IS?
   1 0 ATTR-INT  s" throw" NDICT:CALL-TARGET =
   2 0 OPERAND@ 1 0 RESULT@ SAME-VALUE?
   2 0 OPERAND@ 1 1 RESULT@ SAME-VALUE? ;

: DIVIDE-CASE ( -- )
   s" a division is the divide form: the dividend's copy, the divisor, the quotient and the throw entry" T-LABEL
   WBND [: DIVIDE-BODY ;] IR-CTX:WITH-CONTEXT
   TFALSE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE 3 T= ;

\ ---- the comparison and the branch below it ---------------------------------
: BRANCH-BODY ( IR-CTX:ctx -- n n bool bool bool n n bool )
   HIR-MOD
   BUILD-BRANCH
   SELECTED READ!
   BLOCKS
   OPS
   0 s" x64.cmpset" OPCODE-IS?
   1 s" x64.brz" OPCODE-IS?
   1 s" x64.cmpbr" OPCODE-IS?
   1 0 SUCC@
   1 1 SUCC@
   1 0 OPERAND@ 0 0 RESULT@ SAME-VALUE? ;

: BRANCH-CASE ( -- )
   s" a comparison feeding a branch is a compare-and-set and then a test" T-LABEL
   WBND [: BRANCH-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE 2 T= 1 T= TFALSE TTRUE TTRUE 2 T= 3 T= ;

: COND-BODY ( IR-CTX:ctx -- n bool bool bool n )
   HIR-MOD
   BUILD-BRANCH
   SELECTED READ!
   OPS
   0 0 s" x64.cond" ATTR-KEY-IS?
   0 0 ATTR-INT X64IR-COND:LT X64IR:COND-CODE =
   0 0 ATTR-INT X64IR-COND:GE X64IR:COND-CODE =
   0 OPERANDS ;

: COND-CASE ( -- )
   s" the condition is the signed less-than the assembler spells" T-LABEL
   WBND [: COND-BODY ;] IR-CTX:WITH-CONTEXT
   2 T= TFALSE TTRUE TTRUE 2 T= ;

: ZEROP-BODY ( IR-CTX:ctx -- n bool bool n n bool )
   HIR-MOD
   BUILD-ZEROP
   SELECTED READ!
   OPS
   0 s" x64.cmpseti" OPCODE-IS?
   0 s" x64.cmpset" OPCODE-IS?
   0 OPERANDS
   0 1 ATTR-INT
   0 0 ATTR-INT X64IR-COND:EQUAL X64IR:COND-CODE = ;

: ZEROP-CASE ( -- )
   s" a comparison against a literal is one instruction against the immediate" T-LABEL
   WBND [: ZEROP-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE 0 T= 1 T= TFALSE TTRUE 2 T= ;

\ ---- the block a branch carries a value into --------------------------------
\ Every block of the source function is selected, not the entry alone, and the
\ value an edge carries arrives in the destination argument's own register.
: CARRY-BODY ( IR-CTX:ctx -- n n bool bool bool bool bool n n bool bool )
   HIR-MOD
   BUILD-CARRY
   SELECTED READ!
   BLOCKS
   0 BOPS
   0 0 s" x64.add" BOPCODE-IS?
   0 1 s" x64.mov" BOPCODE-IS?
   0 2 s" x64.mov" BOPCODE-IS?
   0 3 s" x64.br" BOPCODE-IS?
   0 3 0 BOPERAND@  0 2 0 BRESULT@ SAME-VALUE?
   1 BARGS
   1 BOPS
   1 0 s" x64.ret" BOPCODE-IS?
   1 0 0 BOPERAND@  1 0 BARG@ SAME-VALUE? ;

: CARRY-CASE ( -- )
   s" a branch hands its value over through the copies the edge is split with" T-LABEL
   WBND [: CARRY-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE 1 T= 1 T= TTRUE TTRUE TTRUE TTRUE TTRUE 4 T= 2 T= ;

\ ---- the operand a loop reads again -----------------------------------------
\ The header's own block, which is where the answer differs: the add is preceded
\ by the copy of `x`, its operand 0 IS that copy and is NOT `x`, and the count
\ says the copy is really there.
: LOOP-BODY ( IR-CTX:ctx -- n bool bool bool bool bool )
   HIR-MOD
   BUILD-LOOP
   SELECTED READ!
   1 BOPS
   1 0 s" x64.mov" BOPCODE-IS?
   1 0 0 BOPERAND@  0 0 BARG@ SAME-VALUE?
   1 1 s" x64.add" BOPCODE-IS?
   1 1 0 BOPERAND@  1 0 0 BRESULT@ SAME-VALUE?
   1 1 0 BOPERAND@  0 0 BARG@ SAME-VALUE? ;

: LOOP-CASE ( -- )
   s" a value the backedge brings the header round to read again is copied, though one operand of the function names it" T-LABEL
   WBND [: LOOP-BODY ;] IR-CTX:WITH-CONTEXT
   TFALSE TTRUE TTRUE TTRUE TTRUE 3 T= ;

: STRAIGHT-BODY ( IR-CTX:ctx -- n bool bool bool )
   HIR-MOD
   BUILD-STRAIGHT
   SELECTED READ!
   1 BOPS
   1 0 s" x64.add" BOPCODE-IS?
   1 0 0 BOPERAND@  0 0 BARG@ SAME-VALUE?
   1 0 1 BOPERAND@  1 0 BARG@ SAME-VALUE? ;

: STRAIGHT-CASE ( -- )
   s" the same value read once with no path back to the reader is the form's own operand and costs no copy" T-LABEL
   WBND [: STRAIGHT-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE 2 T= ;

\ ---- the data-stack boundary ------------------------------------------------
\ A routine that hands its argument straight back leaves it in the cell it was
\ entered in: the result's cell already holds the value the exit would store, so
\ the store is left out - the validator refuses that store by name - and with no
\ store nothing reads the argument out of a register, so the entry load goes
\ too. The take and the publish are the whole boundary, and they stay: the
\ pointer still moves over the arguments and still publishes the results.
: PASS-BODY ( IR-CTX:ctx -- n n bool n bool n bool n )
   HIR-MOD
   BUILD-PASS
   1 1 SELECTED-LEAF READ!
   OPS
   ARGS-N
   0 s" x64.dtake" OPCODE-IS?
   0 0 ATTR-INT
   1 s" x64.dpublish" OPCODE-IS?
   1 0 ATTR-INT
   2 s" x64.ret" OPCODE-IS?
   2 OPERANDS ;

: PASS-CASE ( -- )
   s" the boundary of a routine that hands its argument back is the take and the publish alone" T-LABEL
   WBND [: PASS-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= TTRUE 8 T= TTRUE 8 T= TTRUE 0 T= 3 T= ;

: FRAME-BODY ( IR-CTX:ctx -- n bool n bool n bool )
   HIR-MOD
   BUILD-PASS
   1 1 2 SELECTED-FRAMED READ!
   OPS
   0 s" x64.reserve" OPCODE-IS?
   0 0 ATTR-INT
   3 s" x64.release" OPCODE-IS?
   3 0 ATTR-INT
   4 s" x64.ret" OPCODE-IS? ;

: FRAME-CASE ( -- )
   s" a routine with spills reserves at the entry and releases before the return" T-LABEL
   WBND [: FRAME-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE 16 T= TTRUE 16 T= TTRUE 5 T= ;

\ The boundary where the value really moves: the argument is read out of its
\ cell into a register, and what the body computed is a different value from the
\ one the result's cell holds, so the exit stores it and publishes the pointer.
: MEMOPS-BODY ( IR-CTX:ctx -- n bool n bool bool bool bool bool bool n bool n bool )
   HIR-MOD
   BUILD-MEMOPS
   1 1 SELECTED-LEAF READ!
   OPS
   1 s" x64.dload" OPCODE-IS?
   1 0 ATTR-INT
   2 s" x64.movi" OPCODE-IS?
   3 s" x64.astore" OPCODE-IS?
   4 s" x64.aload" OPCODE-IS?
   5 s" x64.abstore" OPCODE-IS?
   6 s" x64.abload" OPCODE-IS?
   7 s" x64.dstore" OPCODE-IS?
   7 0 ATTR-INT
   8 s" x64.dpublish" OPCODE-IS?
   8 0 ATTR-INT
   9 s" x64.ret" OPCODE-IS? ;

: MEMOPS-CASE ( -- )
   s" the addressed loads and stores select to the forms of their width, between a load per argument and a store per result" T-LABEL
   WBND [: MEMOPS-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE 8 T= TTRUE 0 T= TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE 0 T= TTRUE 10 T= ;

\ ---- the call ---------------------------------------------------------------
\ WHAT THE RESIDENCY MAP TAKES OUT OF A CALL SITE. The entry loaded the argument
\ out of cell 0 and this site passes that same value in cell 0, so the store is
\ left out: a store into a cell that still holds the value stored is what
\ regalloc-verify.f refuses by name (E-A64RAV-DKEEP), not a saving. Cell 1 is
\ the carried value and is written. On the way back, the carried value's result
\ has no reader, so only the answer is loaded. The site was four transfers
\ around the call and is two, and the count is asserted because a selection that
\ stopped eliding would carry these same opcodes at other indices.
: CALL-BODY ( IR-CTX:ctx -- n bool n bool n bool bool n bool n bool n bool n bool n bool bool )
   HIR-MOD
   BUILD-CALLER
   1 1 SELECTED-CALL READ!
   OPS
   1 s" x64.dload" OPCODE-IS?
   1 0 ATTR-INT
   2 s" x64.dstore" OPCODE-IS?
   2 0 ATTR-INT
   3 s" x64.wordcall" OPCODE-IS?
   3 0 s" x64.dbytes" ATTR-KEY-IS?
   3 0 ATTR-INT
   3 1 s" x64.dback" ATTR-KEY-IS?
   3 1 ATTR-INT
   3 2 s" x64.entry" ATTR-KEY-IS?
   3 2 ATTR-INT
   4 s" x64.dload" OPCODE-IS?
   4 0 ATTR-INT
   5 s" x64.dstore" OPCODE-IS?
   5 0 ATTR-INT
   6 s" x64.dpublish" OPCODE-IS?
   7 s" x64.ret" OPCODE-IS? ;

: CALL-CASE ( -- )
   s" a call writes only the cells that do not already hold the value, calls, and takes back only what is read" T-LABEL
   WBND [: CALL-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE 0 T= TTRUE 8 T= TTRUE
   CALLEE-ENTRY T= TTRUE 16 T= TTRUE 16 T= TTRUE
   TTRUE 8 T= TTRUE 0 T= TTRUE 8 T= ;

\ The same body where control leaves through the callee: the pointer never
\ moves, so the argument is already in the cell the callee reads it from and the
\ site stores nothing at all. With no store, nothing reads the argument out of a
\ register either, so the entry load goes too and the whole routine is the take
\ and the branch. It was four operations - take, load, store, branch - and the
\ store was the one the validator refused (test/compiler/x64-regalloc.f).
: TAIL-BODY ( IR-CTX:ctx -- n bool n bool n )
   HIR-MOD
   BUILD-CALLER
   1 1 SELECTED-TAIL READ!
   OPS
   0 s" x64.dtake" OPCODE-IS?
   0 0 ATTR-INT
   1 s" x64.tailcall" OPCODE-IS?
   1 0 ATTR-INT ;

: TAIL-CASE ( -- )
   s" a routine that leaves through its callee never moves the pointer and re-stores nothing" T-LABEL
   WBND [: TAIL-BODY ;] IR-CTX:WITH-CONTEXT
   CALLEE-ENTRY T= TTRUE 0 T= TTRUE 2 T= ;

\ ---- the contracts a compiled word is written under -------------------------
\ src/arch/x86-64/abi.f, read back off the routines it builds. The three fields
\ no ARM64 contract can share are the subject: the absent link, the nine-register
\ pool and a frame that holds spills alone.
: R-MASK ( -- n )
   1 3 lshift  1 4 lshift or  1 5 lshift or
   1 12 lshift or  1 13 lshift or  1 14 lshift or  1 15 lshift or ;

: A-MASK ( -- n )   $FFFF R-MASK xor ;

: ABI-CASE ( -- )
   s" every contract of this machine says the link register is absent" T-LABEL
   X64ABI:SCRATCH 1 1 X64ABI:LEAF NEFF:LINK@
      NEFF-LINK:ABSENT NEFF-LINK:EQ TTRUE
   X64ABI:SCRATCH 1 1 X64ABI:CALL NEFF:LINK@
      NEFF-LINK:ABSENT NEFF-LINK:EQ TTRUE
   s" the pool is the machine's nine allocatable registers" T-LABEL
   X64ABI:SCRATCH NEFF:GPRS-N A-MASK T=
   X64ABI:SCRATCH 1 1 X64ABI:LEAF NEFF:GPR-CLOBBER@ NEFF:GPRS-N A-MASK T=
   s" a frame holds spills alone and is rounded to the stack alignment" T-LABEL
   X64ABI:SCRATCH 1 1 X64ABI:LEAF NEFF:FRAME@ 0 T=
   X64ABI:SCRATCH 1 1 X64ABI:CALL NEFF:FRAME@ 0 T=
   X64ABI:SCRATCH 1 1 1 X64ABI:LEAF-FRAMED NEFF:FRAME@ 16 T=
   X64ABI:SCRATCH 1 1 2 X64ABI:LEAF-FRAMED NEFF:FRAME@ 16 T=
   X64ABI:SCRATCH 1 1 3 X64ABI:LEAF-FRAMED NEFF:FRAME@ 32 T=
   s" each convention declares what it is" T-LABEL
   X64ABI:SCRATCH 1 1 X64ABI:CALL NEFF:TRAITS@ NEFF:T-CALL NEFF:TRAITS-HAS? TTRUE
   X64ABI:SCRATCH 1 1 X64ABI:LEAF NEFF:TRAITS@ NEFF:T-CALL NEFF:TRAITS-HAS? TFALSE
   X64ABI:SCRATCH 1 1 X64ABI:TAIL NEFF:CONTROL@
      NEFF-CONTROL:TAIL-CALL NEFF-CONTROL:EQ TTRUE
   X64ABI:SCRATCH 1 1 0 X64ABI:TAIL-CALLING-FRAMED NEFF:TRAITS@
      NEFF:T-CALL NEFF:TRAITS-HAS? TTRUE
   X64ABI:SCRATCH 1 1 2 X64ABI:NORET-FRAMED NEFF:CONTROL@
      NEFF-CONTROL:NO-RETURN NEFF-CONTROL:EQ TTRUE
   X64ABI:SCRATCH 1 1 2 X64ABI:NORET-FRAMED NEFF:DELTA@ -16 T=
   X64ABI:SCRATCH 1 1 2 X64ABI:NORET-LEAF-FRAMED NEFF:TRAITS@
      NEFF:T-CALL NEFF:TRAITS-HAS? TFALSE ;

\ ---- refusals ---------------------------------------------------------------
: NO-BIND-BODY ( IR-CTX:ctx -- )
   HIR-MOD
   BUILD-SQUARE
   CC BB IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   CC m X64-BUILDER NO-PLACES X64SEL:SELECT drop ;

: TWICE-BIND-BODY ( IR-CTX:ctx -- )
   HIR-MOD
   CC BB X64SEL:BIND-SOURCE
   CC BB X64SEL:BIND-SOURCE ;

: WRONG-DIALECT-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   IR-BUILD:PLAN-BEGIN
   IR-BUILD:PLAN-DEFAULT
   c X64IR:NEW-BUILDER {: b:IR-BUILD:builder :}
   c b X64SEL:BIND-SOURCE ;

\ The pass is told ONE module and every ordinal it reads - opcodes, attribute
\ keys, the memory type - is that module's own. A frozen module it was never
\ bound to is refused rather than read against another module's numbering.
: OTHER-MODULE-BODY ( IR-CTX:ctx -- )
   HIR-MOD
   BUILD-SQUARE
   CC BB X64SEL:BIND-SOURCE
   CC BB IR-BUILD:FREEZE drop
   X64-BUILDER {: b:IR-BUILD:builder :}
   CC b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   CC m X64-BUILDER NO-PLACES X64SEL:SELECT drop ;

: EXTRA-OPCODE-BODY ( IR-CTX:ctx -- )
   HIR-MOD
   BUILD-EXTRA
   SELECTED drop ;

: TRAP-BODY ( IR-CTX:ctx -- )
   HIR-MOD
   BUILD-ADD
   SELECTED drop ;

: FLOAT-BODY ( IR-CTX:ctx -- )
   HIR-MOD
   BUILD-FLOAT
   SELECTED drop ;

: WRONG-MACHINE-BODY ( IR-CTX:ctx -- )
   HIR-MOD
   BUILD-SQUARE
   SELECTED-A64 drop ;

: MEM-REG-BODY ( IR-CTX:ctx -- )
   HIR-MOD
   BUILD-MEMOPS
   SELECTED drop ;

: NO-BIND ( -- )
   WBND [: NO-BIND-BODY ;] IR-CTX:WITH-CONTEXT ;

: TWICE-BIND ( -- )
   WBND [: TWICE-BIND-BODY ;] IR-CTX:WITH-CONTEXT ;

: WRONG-DIALECT ( -- )
   WBND [: WRONG-DIALECT-BODY ;] IR-CTX:WITH-CONTEXT ;

: OTHER-MODULE ( -- )
   WBND [: OTHER-MODULE-BODY ;] IR-CTX:WITH-CONTEXT ;

: EXTRA-OPCODE ( -- )
   WBND [: EXTRA-OPCODE-BODY ;] IR-CTX:WITH-CONTEXT ;

: TRAPPING ( -- )
   TBND [: TRAP-BODY ;] IR-CTX:WITH-CONTEXT ;

: FLOATING ( -- )
   WBND [: FLOAT-BODY ;] IR-CTX:WITH-CONTEXT ;

: WRONG-MACHINE ( -- )
   WBND [: WRONG-MACHINE-BODY ;] IR-CTX:WITH-CONTEXT ;

: MEM-REG ( -- )
   WBND [: MEM-REG-BODY ;] IR-CTX:WITH-CONTEXT ;

\ Each refusal leaves a binding behind or takes one, so the binding is released
\ between cases and a case never selects against a neighbour's.
: DROP-BINDING ( -- )
   X64SEL:RELEASE ;

: BIND-REFUSE-CASES ( -- )
   s" selecting without a binding is refused" T-LABEL
   [: NO-BIND ;] E-X64SEL-BIND TTHROWSQ
   s" a second binding over a live one is refused" T-LABEL
   [: TWICE-BIND ;] E-X64SEL-BIND TTHROWSQ
   DROP-BINDING ;

: SOURCE-REFUSE-CASES ( -- )
   s" binding a module of another dialect is refused" T-LABEL
   [: WRONG-DIALECT ;] E-X64SEL-SOURCE TTHROWSQ
   s" selecting a module the pass was not told about is refused" T-LABEL
   [: OTHER-MODULE ;] E-X64SEL-SOURCE TTHROWSQ ;

: OPCODE-REFUSE-CASES ( -- )
   s" an operation of an opcode with no selection rule is refused" T-LABEL
   [: EXTRA-OPCODE ;] E-X64SEL-OPCODE TTHROWSQ ;

: TRAP-REFUSE-CASES ( -- )
   s" arithmetic that may trap has no x86-64 lowering and is refused" T-LABEL
   [: TRAPPING ;] E-X64SEL-TRAP TTHROWSQ ;

: FLOAT-REFUSE-CASES ( -- )
   s" a floating operation has no form in this dialect and is refused" T-LABEL
   [: FLOATING ;] E-X64SEL-FLOAT TTHROWSQ ;

: MACHINE-REFUSE-CASES ( -- )
   s" a contract of another machine is not one this backend lowers for" T-LABEL
   [: WRONG-MACHINE ;] E-X64SEL-MACHINE TTHROWSQ
   DROP-BINDING ;

: MEM-REFUSE-CASES ( -- )
   s" a memory operation needs the data-stack order this convention has none of" T-LABEL
   [: MEM-REG ;] E-X64SEL-MEM TTHROWSQ ;

\ A refusing case runs INSIDE an enclosing context: an abandoned context gives
\ its registry slots back only when a live enclosing context leaves normally.
: GROUP-BIND ( IR-CTX:ctx -- )        drop BIND-REFUSE-CASES ;
: GROUP-SOURCE ( IR-CTX:ctx -- )      drop SOURCE-REFUSE-CASES ;
: GROUP-OPCODE ( IR-CTX:ctx -- )      drop OPCODE-REFUSE-CASES ;
: GROUP-TRAP ( IR-CTX:ctx -- )        drop TRAP-REFUSE-CASES ;
: GROUP-FLOAT ( IR-CTX:ctx -- )       drop FLOAT-REFUSE-CASES ;
: GROUP-MACHINE ( IR-CTX:ctx -- )     drop MACHINE-REFUSE-CASES ;
: GROUP-MEM ( IR-CTX:ctx -- )         drop MEM-REFUSE-CASES ;

public

: RUN ( -- )
   T-RESET
   LIT-CASE
   SQUARE-CASE
   REUSE-CASE
   IMM-REUSE-CASE
   MASK-CASE
   DIFF-CASE
   LOGIC-CASE
   IMM-ADD-CASE
   SHIFT-CASE
   VAR-SHIFT-CASE
   DIVIDE-CASE
   BRANCH-CASE
   COND-CASE
   CARRY-CASE
   LOOP-CASE
   STRAIGHT-CASE
   ZEROP-CASE
   PASS-CASE
   FRAME-CASE
   MEMOPS-CASE
   CALL-CASE
   TAIL-CASE
   ABI-CASE
   WBND [: GROUP-BIND ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-SOURCE ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-OPCODE ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-TRAP ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-FLOAT ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-MACHINE ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-MEM ;] IR-CTX:WITH-CONTEXT
   T-REPORT ;

;package

X64SEL-TEST:RUN

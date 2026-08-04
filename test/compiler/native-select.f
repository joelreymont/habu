\ native-select.f - checked instruction selection tests.
\
\ Proves the contract of src/compiler/native/select.f: a frozen straight-line HIR
\ module becomes a frozen A64IR module in which every source operation has become
\ the machine operations that compute it, every source value has become the value
\ the last of those operations defines, and every operand names that value rather
\ than a position; a literal becomes the move-wide chain that materialises it;
\ and a module the pass was not told about, a text that is not the one the module
\ was compiled from, an operation the pass has no rule for, an operation that may
\ trap, and a module shape this leaf cannot select are each refused by name.
\
\ WHAT THE FIXTURES MEASURE, AND WHY THEY WOULD CATCH A WRONG TABLE. Every
\ positive case asserts the selected OPCODE per source operation and the selected
\ VALUE per operand. Swapping two arms of the selection table - selecting a
\ subtraction as an addition - fails the opcode assertion; wiring an operand to
\ the wrong value - taking the second source operand where the first belongs -
\ fails the operand-identity assertion, which is why `- ` and not `*` is the
\ fixture that pins operand order: both operands of `dup *` are the same value,
\ so a swapped pair there would be invisible.
\
\ WHY THE SOURCE MODULES ARE BUILT HERE RATHER THAN ELABORATED. The subject is
\ the pass, and its input is a frozen module; the fixtures are therefore built
\ through the real staged builder, the real dialect and the real freeze - which
\ runs the whole structural verifier - and not through a stand-in. Building them
\ directly is also what makes the hostile cases possible at all: a module holding
\ an operation of a sixth opcode, or two registered sources, is something the
\ elaborator will never produce and the pass must still refuse.
\
\ WHY THE POSITIVE CASES BIND A WRAPPING UNIT. Whether a Habu `+` traps is the
\ compilation unit's overflow policy, and the source dialect records the answer
\ in its arithmetic schemas. ARM64's Add wraps and this dialect has no lowering
\ for a trapping one, so a trapping unit is refused - which is a case of its own
\ below, and the reason the arithmetic fixtures are compiled with wrapping.
\
\ ONE FIXTURE PER CONTEXT. A module holds about seventeen arenas and the live
\ arena registry holds sixty-four, so a context that built two modules is close
\ to full and one that built four is over. Every case therefore runs in its own
\ context, and a refusing case runs inside an enclosing one because an abandoned
\ context gives its registry slots back only when a live enclosing context leaves
\ normally (src/compiler/ir/context.f, the note on stale handles).

require lib/test.f
require src/compiler/native/select.f

package A64SEL-TEST
private

\ ---- bindings ----------------------------------------------------------------
\ An AArch64 Darwin contract whose integer overflow wraps: the machine's own
\ behaviour, and the one this dialect can select arithmetic under.
: WBND ( -- CBIND:binding )
   CTARGET-ARCH:AARCH64 CTARGET-ABI:AAPCS64-DARWIN CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64
   CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH CTARGET:CONTRACT
   CNUM-OVERFLOW:WRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY
   CBIND:BIND ;

\ The same machine with a trapping overflow policy.
: TBND ( -- CBIND:binding )
   CTARGET-ARCH:AARCH64 CTARGET-ABI:AAPCS64-DARWIN CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64
   CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH CTARGET:CONTRACT
   CNUM-OVERFLOW:TRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY
   CBIND:BIND ;

\ ---- the fixture's source text -----------------------------------------------
\ One text stands behind every fixture, so each span a fixture attaches is a real
\ byte range in bytes the module has really registered, and IR-SOURCE refuses one
\ that is not.
create TXT
   58 c, 32 c, 83 c, 81 c, 85 c, 65 c, 82 c, 69 c,            \ ": SQUARE"
   32 c, 100 c, 117 c, 112 c, 32 c, 42 c, 32 c, 59 c,         \ " dup * ;"
16 constant TXT-N

: OTHER-TXT ( -- ptr u8 n )
   s" : OTHER dup * ;" ;

2 constant NAME-ST                   \ the defined name inside TXT
6 constant NAME-LN
0 constant OPEN-ST                   \ the opening `:`
1 constant OPEN-LN
9 constant BODY-ST                   \ the body word
3 constant BODY-LN
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

: CONSTOP ( n -- IR-ID:ir-value-id )
   {: v:n :}
   HIR-OPCODE:CONST BODY-ST BODY-LN OPEN-OP
   CC BB CELLT IR-BUILD:ADD-RESULT
   CC BB  CC BB HIR:KEY-VALUE  CC BB v IR-BUILD:INTERN-INT-ATTR
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

\ ---- the fixtures ------------------------------------------------------------
\ `: SQUARE ( n -- n ) dup * ;` as the elaborator leaves it: one multiply whose
\ two operands are the same block argument, and one return.
: BUILD-SQUARE ( -- )
   1 1 OPEN-FUN
   ARG+ {: a:IR-ID:ir-value-id :}
   HIR-OPCODE:MUL a a BINOP RET1
   CLOSE-FUN ;

\ `: DIFF ( n n -- n ) - ;`: two different arguments, so the order the operands
\ are wired in is visible.
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

\ `: QUOT ( n n -- n ) / ;`: two different arguments again, so the divide's
\ operand order is visible, and the one source operation whose schema says it
\ may trap that this pass still selects.
: BUILD-DIV ( -- )
   2 1 OPEN-FUN
   ARG+ {: x:IR-ID:ir-value-id :}
   ARG+ {: y:IR-ID:ir-value-id :}
   HIR-OPCODE:DIV x y BINOP RET1
   CLOSE-FUN ;

\ `: BUMP ( n -- n ) A ! A @ 1+ ;` with A a fixed address, as the elaborator
\ leaves it: the memory the definition is entered with, then a store, then a
\ load, each taking the order the one before it answered.
$1000 constant BUMP-ADDR

: MEMT ( -- IR-ID:ir-type-id )
   CC BB HIR:MEM-TYPE ;

: MEM0 ( -- IR-ID:ir-value-id )
   HIR-OPCODE:MEM BODY-ST BODY-LN OPEN-OP
   CC BB MEMT IR-BUILD:ADD-RESULT
   CLOSE-VALUE ;

: STORE1 ( IR-ID:ir-value-id IR-ID:ir-value-id IR-ID:ir-value-id -- IR-ID:ir-value-id )
   {: v:IR-ID:ir-value-id a:IR-ID:ir-value-id k:IR-ID:ir-value-id :}
   HIR-OPCODE:STORE BODY-ST BODY-LN OPEN-OP
   CC BB v IR-BUILD:ADD-OPERAND
   CC BB a IR-BUILD:ADD-OPERAND
   CC BB k IR-BUILD:ADD-OPERAND
   CC BB MEMT IR-BUILD:ADD-RESULT
   CLOSE-VALUE ;

: LOAD1 ( IR-ID:ir-value-id IR-ID:ir-value-id -- IR-ID:ir-value-id IR-ID:ir-value-id )
   {: a:IR-ID:ir-value-id k:IR-ID:ir-value-id :}
   HIR-OPCODE:LOAD BODY-ST BODY-LN OPEN-OP
   CC BB a IR-BUILD:ADD-OPERAND
   CC BB k IR-BUILD:ADD-OPERAND
   CC BB CELLT IR-BUILD:ADD-RESULT
   CC BB MEMT IR-BUILD:ADD-RESULT
   CC BB IR-BUILD:END-OP {: id:IR-ID:ir-op-id :}
   CC BB id 0 IR-BUILD:OP-RESULT@
   CC BB id 1 IR-BUILD:OP-RESULT@ ;

: BUILD-BUMP ( -- )
   1 1 OPEN-FUN
   ARG+ {: x:IR-ID:ir-value-id :}
   MEM0 {: k0:IR-ID:ir-value-id :}
   BUMP-ADDR CONSTOP {: a0:IR-ID:ir-value-id :}
   x a0 k0 STORE1 {: k1:IR-ID:ir-value-id :}
   BUMP-ADDR CONSTOP {: a1:IR-ID:ir-value-id :}
   a1 k1 LOAD1 {: got:IR-ID:ir-value-id k2:IR-ID:ir-value-id :}
   1 CONSTOP {: one:IR-ID:ir-value-id :}
   HIR-OPCODE:ADD got one BINOP RET1
   CLOSE-FUN ;

\ `: PASS ( n -- n ) ;` - a routine that hands its argument straight back. There
\ is nothing between the two, so under the data-stack convention the cell the
\ caller wrote is already the cell the caller will read.
: BUILD-PASS ( -- )
   1 1 OPEN-FUN
   ARG+ RET1
   CLOSE-FUN ;

\ `: EXCH ( a b -- b a ) swap ;` - the same two cells the other way round. Every
\ result is in a cell, and it is the WRONG cell, so nothing here is droppable and
\ the whole entry and exit sequence has to be built.
: BUILD-EXCH ( -- )
   2 2 OPEN-FUN
   ARG+ {: x:IR-ID:ir-value-id :}
   ARG+ {: y:IR-ID:ir-value-id :}
   HIR-OPCODE:RETURN CLOSE-ST CLOSE-LN OPEN-OP
   CC BB y IR-BUILD:ADD-OPERAND
   CC BB x IR-BUILD:ADD-OPERAND
   CC BB IR-BUILD:END-OP drop
   CLOSE-FUN ;

\ A literal that fits one move-wide half, and one that needs two.
: BUILD-SMALL ( -- )
   0 1 OPEN-FUN
   7 CONSTOP RET1
   CLOSE-FUN ;

: BUILD-WIDE ( -- )
   0 1 OPEN-FUN
   $1234000000005678 CONSTOP RET1
   CLOSE-FUN ;

\ ---- a comparison and the branch that tests it -------------------------------
\ The four blocks a two-armed `if` leaves: the entry that compares and branches,
\ one stub per arm because a two-way branch carries no values, and the join that
\ takes the chosen value as its argument. It is built here rather than
\ elaborated for the reason every other fixture in this file is, and it is built
\ ONCE and used three ways: the comparison it opens with is a parameter, and the
\ value its two arms hand on is a parameter, so the fused case, the three
\ conditions and the multi-use fall-back are the same shape with one thing
\ changed.
\
\ ONE ARM DIVIDES, AND THAT IS WHAT KEEPS THIS FIXTURE ABOUT A BRANCH. The
\ if-conversion further down turns a selection whose arms are speculable into a
\ machine select and no branch at all, and the shape without the division - a
\ plain `a b < if a else b then` - is exactly what it converts; that shape has
\ its own cases below. A division may TRAP, so its arm cannot be run on a path
\ the program would not have taken, the region is refused, and the two-way
\ branch survives to be fused. It is the smallest source-level fact that keeps a
\ branch: an arm that divides, calls, loads or stores is a branch a real body
\ still emits.
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

\ `carry` says which value the two arms hand the join. Handing the ARGUMENTS on
\ leaves the comparison's answer with one reader, the branch, which is what the
\ fusion requires; handing the COMPARISON's answer on gives it three, which is
\ what the fall-back requires. Nothing else about the two modules differs, so a
\ fusion that ignored the use count would produce the same module for both.
: BUILD-BRANCH ( HIR:opcode bool -- )
   {: o:HIR:opcode carry:bool :}
   2 1 OPEN-FUN
   ARG+ {: x:IR-ID:ir-value-id :}
   ARG+ {: y:IR-ID:ir-value-id :}
   o x y BINOP {: f:IR-ID:ir-value-id :}
   f 1 2 BRZ2
   BLOCK+
   HIR-OPCODE:DIV x y BINOP {: q:IR-ID:ir-value-id :}
   q 3 BR1
   BLOCK+
   carry if f 3 BR1 else y 3 BR1 then
   BLOCK+
   ARG+ RET1
   CLOSE-FUN ;

\ ---- the same shape over doubles ---------------------------------------------
\ The float comparisons reach the fusion by exactly the route the integer ones
\ do - a comparison whose one reader is the branch below it - so the fixture is
\ the same fixture with two things changed: the values compared are DOUBLES, and
\ the comparison is one of the five float opcodes.
\
\ WHY THE DOUBLES ARE CROSSED FROM CELL ARGUMENTS RATHER THAN BEING BLOCK
\ ARGUMENTS. That is what a real body does. A word's arguments arrive in
\ data-stack cells, and `hir.bits>real` is the operation that reads one as the
\ double it holds; the elaborator stages exactly that in front of any float
\ operation whose operand is a cell. Making them float-typed block arguments
\ instead would build a module this pass refuses anyway, because a double may
\ not cross a block edge yet.
\
\ AND WHY THE ARMS HAND ON THE CELLS. For the same reason: the join's arguments
\ are cells, so what crosses the edge is `xc` and `yc` and not the doubles - the
\ shape MAX-F has, and the reason MAX-F compiles today while RELU-F does not.
: REALT ( -- IR-ID:ir-type-id )
   CC BB HIR:REAL-TYPE ;

\ A block argument in the floating file. Only the if-conversion cases need one:
\ a join that carries a double is the shape this leaf refuses to select, because
\ choosing between two doubles is an instruction the shipped assembler has no
\ encoder for.
: FARG+ ( -- IR-ID:ir-value-id )
   CC BB REALT IR-BUILD:ADD-BLOCK-ARG ;

: CROSS ( IR-ID:ir-value-id -- IR-ID:ir-value-id )
   {: v:IR-ID:ir-value-id :}
   HIR-OPCODE:BITSREAL BODY-ST BODY-LN OPEN-OP
   CC BB v IR-BUILD:ADD-OPERAND
   CC BB REALT IR-BUILD:ADD-RESULT
   CLOSE-VALUE ;

\ A float comparison of two doubles, answering a cell - which is what a Habu
\ flag is.
: FCMP2 ( HIR:opcode IR-ID:ir-value-id IR-ID:ir-value-id -- IR-ID:ir-value-id )
   {: o:HIR:opcode x:IR-ID:ir-value-id y:IR-ID:ir-value-id :}
   o BODY-ST BODY-LN OPEN-OP
   CC BB x IR-BUILD:ADD-OPERAND
   CC BB y IR-BUILD:ADD-OPERAND
   CC BB CELLT IR-BUILD:ADD-RESULT
   CLOSE-VALUE ;

\ And one against the instruction's own zero, which takes one operand.
: FCMP1 ( HIR:opcode IR-ID:ir-value-id -- IR-ID:ir-value-id )
   {: o:HIR:opcode x:IR-ID:ir-value-id :}
   o BODY-ST BODY-LN OPEN-OP
   CC BB x IR-BUILD:ADD-OPERAND
   CC BB CELLT IR-BUILD:ADD-RESULT
   CLOSE-VALUE ;

\ `: MAX-F ( r r -- r ) {: x:r y:r :} x y f< if y else x then ;` in the shape the
\ elaborator leaves it, with the comparison a parameter and the value the arms
\ hand on a parameter - the same two knobs the integer fixture has, so the fused
\ case and the multi-use fall-back are one shape with one thing changed.
: BUILD-FBRANCH ( HIR:opcode bool -- )
   {: o:HIR:opcode carry:bool :}
   2 1 OPEN-FUN
   ARG+ {: xc:IR-ID:ir-value-id :}
   ARG+ {: yc:IR-ID:ir-value-id :}
   xc CROSS {: x:IR-ID:ir-value-id :}
   yc CROSS {: y:IR-ID:ir-value-id :}
   o x y FCMP2 {: f:IR-ID:ir-value-id :}
   f 1 2 BRZ2
   BLOCK+
   HIR-OPCODE:DIV xc yc BINOP {: q:IR-ID:ir-value-id :}
   q 3 BR1
   BLOCK+
   carry if f 3 BR1 else yc 3 BR1 then
   BLOCK+
   ARG+ RET1
   CLOSE-FUN ;

\ `: RELU-ISH ( r -- n ) {: x:r :} x f0< if 1 else 2 then ;` reduced to what this
\ pass can see: one argument, one crossing, one comparison against zero, and the
\ branch that reads it.
: BUILD-FZBRANCH ( HIR:opcode -- )
   {: o:HIR:opcode :}
   1 1 OPEN-FUN
   ARG+ {: xc:IR-ID:ir-value-id :}
   xc CROSS {: x:IR-ID:ir-value-id :}
   o x FCMP1 {: f:IR-ID:ir-value-id :}
   f 1 2 BRZ2
   BLOCK+
   HIR-OPCODE:DIV xc xc BINOP {: q:IR-ID:ir-value-id :}
   q 3 BR1
   BLOCK+
   xc 3 BR1
   BLOCK+
   ARG+ RET1
   CLOSE-FUN ;

\ And the flag-materialising shape: the comparison IS what the word leaves, so
\ there is no branch to fuse into.
: BUILD-FFLAG-VALUE ( HIR:opcode -- )
   {: o:HIR:opcode :}
   2 1 OPEN-FUN
   ARG+ {: xc:IR-ID:ir-value-id :}
   ARG+ {: yc:IR-ID:ir-value-id :}
   o  xc CROSS  yc CROSS  FCMP2 RET1
   CLOSE-FUN ;

: BUILD-FZFLAG-VALUE ( HIR:opcode -- )
   {: o:HIR:opcode :}
   1 1 OPEN-FUN
   ARG+ {: xc:IR-ID:ir-value-id :}
   o  xc CROSS  FCMP1 RET1
   CLOSE-FUN ;

\ `: ISLT ( n n -- bool ) < ;` - a comparison whose answer IS what the word
\ leaves. There is no branch below it at all, so there is nothing to fuse into
\ and the flag has to be materialised.
: BUILD-FLAG-VALUE ( -- )
   2 1 OPEN-FUN
   ARG+ {: x:IR-ID:ir-value-id :}
   ARG+ {: y:IR-ID:ir-value-id :}
   HIR-OPCODE:LT x y BINOP RET1
   CLOSE-FUN ;

\ An operation of an opcode the source dialect does not have, defined into the
\ same dialect's table. Nothing in the substrate forbids it and the module
\ verifies, so the pass has to refuse it by name rather than by never meeting
\ it. `hir.negate` is chosen because Habu really has a `negate` and this dialect
\ really has no opcode for it, so the row is the shape a capability that has not
\ landed yet would arrive in.
: EXTRA-SCHEMA ( -- IR-ID:ir-symbol-id )
   CC BB s" hir.negate" IR-BUILD:INTERN-SYMBOL {: op:IR-ID:ir-symbol-id :}
   op IR-SCHEMA:BEGIN-OP
   CELLT IR-SCHEMA:ADD-OPERAND
   CELLT IR-SCHEMA:ADD-OPERAND
   CELLT IR-SCHEMA:ADD-RESULT
   false 0 0 IR-SCHEMA:SET-CONTROL
   IR-SCHEMA:SET-PURE
   false IR-SCHEMA:SET-TRAP
   CTARGET-ARCH:AARCH64 CTARGET:F-BASE IR-SCHEMA:SET-TARGET
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

\ ---- the selections that become a select ------------------------------------
\ The same four blocks the branching fixture has, with nothing in the arms that
\ could trap or touch memory - which is the whole of what the if-conversion
\ admits. `carry` again says what the arms hand the join: two DIFFERENT values,
\ which is what needs a select at all, or the comparison's own answer twice,
\ which needs none because there is nothing to choose between.
: BUILD-SELECT ( HIR:opcode bool -- )
   {: o:HIR:opcode carry:bool :}
   2 1 OPEN-FUN
   ARG+ {: x:IR-ID:ir-value-id :}
   ARG+ {: y:IR-ID:ir-value-id :}
   o x y BINOP {: f:IR-ID:ir-value-id :}
   f 1 2 BRZ2
   BLOCK+
   carry if f 3 BR1 else x 3 BR1 then
   BLOCK+
   carry if f 3 BR1 else y 3 BR1 then
   BLOCK+
   ARG+ RET1
   CLOSE-FUN ;

\ `: PICK ( n n n -- n ) {: c:n x:n y:n :} c if x else y then ;` - a selection
\ on a value that no comparison next to it computed. There is nothing to fuse,
\ so the tested value is compared against zero and the select is made on that.
: BUILD-SELECT-VALUE ( -- )
   3 1 OPEN-FUN
   ARG+ {: c:IR-ID:ir-value-id :}
   ARG+ {: x:IR-ID:ir-value-id :}
   ARG+ {: y:IR-ID:ir-value-id :}
   c 1 2 BRZ2
   BLOCK+
   x 3 BR1
   BLOCK+
   y 3 BR1
   BLOCK+
   ARG+ RET1
   CLOSE-FUN ;

\ The same selection with a join that carries a DOUBLE. Everything else about it
\ is admissible, so it is the boundary itself: one type on one argument decides
\ which file the select answers in - and, under a contract with no floating
\ registers to hand out, whether the branch survives at all.
: BUILD-SELECT-REAL ( -- )
   2 1 OPEN-FUN
   ARG+ {: xc:IR-ID:ir-value-id :}
   ARG+ {: yc:IR-ID:ir-value-id :}
   xc CROSS {: x:IR-ID:ir-value-id :}
   yc CROSS {: y:IR-ID:ir-value-id :}
   HIR-OPCODE:LT xc yc BINOP {: f:IR-ID:ir-value-id :}
   f 1 2 BRZ2
   BLOCK+
   x 3 BR1
   BLOCK+
   y 3 BR1
   BLOCK+
   FARG+ drop
   xc RET1
   CLOSE-FUN ;

\ The same again with the arms handed over the other way round. A select whose
\ two sources were swapped is the other arm on every unequal pair, and the two
\ fixtures together are what makes the operand assertions below a statement
\ about polarity rather than a restatement of the order they were written in.
: BUILD-SELECT-REAL-SWAPPED ( -- )
   2 1 OPEN-FUN
   ARG+ {: xc:IR-ID:ir-value-id :}
   ARG+ {: yc:IR-ID:ir-value-id :}
   xc CROSS {: x:IR-ID:ir-value-id :}
   yc CROSS {: y:IR-ID:ir-value-id :}
   HIR-OPCODE:LT xc yc BINOP {: f:IR-ID:ir-value-id :}
   f 1 2 BRZ2
   BLOCK+
   y 3 BR1
   BLOCK+
   x 3 BR1
   BLOCK+
   FARG+ drop
   xc RET1
   CLOSE-FUN ;

\ ---- a FLOAT comparison feeding a selection ----------------------------------
\ The four fixtures below are the second ROW of the select square: the flags the
\ select reads were written by an Fcmp rather than by a Cmp. The four corners are
\ the two comparison shapes - two doubles, or one double against the
\ instruction's own zero - times the two files the chosen answers may live in,
\ and two of them are bodies this system runs:
\
\   BUILD-FSEL-REAL   `x y f< if y else x then` with doubles at the join
\   BUILD-FSEL-CELL   the same handing the argument CELLS on - MAX-F's shape
\   BUILD-FSELZ-REAL  `x f0< if y else x then` with doubles at the join -
\                     RELU-F's shape, with a second argument standing in for the
\                     float literal that leaf has no fixture for
\   BUILD-FSELZ-CELL  `x f0= if xc else yc then`, the zero compare choosing cells
\
\ WHAT EACH ONE HAS TO COME OUT AS, and why reading the opcode BY NAME is the
\ assertion. A conversion that chose a Cmp-flagged form would ask the machine to
\ compare two D registers with an integer Cmp, which is not an instruction; one
\ that chose the wrong ANSWER file would move eight bytes out of a register that
\ does not hold them. The names are the only thing that separates the four.
\
\ AND THE CONDITION IS THE OTHER HALF, which is why every case below reads the
\ condition attribute against the dialect's own code for `mi` or `equal`. A fused
\ float select reads the flags an Fcmp left, so what decides the arm for a NaN is
\ that condition and nothing else: `mi`, `gt` and `equal` are false on unordered
\ and `lt` is TRUE on it. The two `f<`-shaped fixtures are the negative control
\ for the whole condition table - their naive condition, read off the relation's
\ name, is `lt`, and asserting `mi` is what makes the table's derivation a check.
: BUILD-FSEL-REAL ( -- )
   2 1 OPEN-FUN
   ARG+ {: xc:IR-ID:ir-value-id :}
   ARG+ {: yc:IR-ID:ir-value-id :}
   xc CROSS {: x:IR-ID:ir-value-id :}
   yc CROSS {: y:IR-ID:ir-value-id :}
   HIR-OPCODE:FLT x y FCMP2 {: f:IR-ID:ir-value-id :}
   f 1 2 BRZ2
   BLOCK+
   x 3 BR1
   BLOCK+
   y 3 BR1
   BLOCK+
   FARG+ drop
   xc RET1
   CLOSE-FUN ;

\ The same again with the arms handed over the other way round, so the operand
\ assertions below are a statement about polarity rather than a restatement of
\ the order the fixture was written in.
: BUILD-FSEL-REAL-SWAPPED ( -- )
   2 1 OPEN-FUN
   ARG+ {: xc:IR-ID:ir-value-id :}
   ARG+ {: yc:IR-ID:ir-value-id :}
   xc CROSS {: x:IR-ID:ir-value-id :}
   yc CROSS {: y:IR-ID:ir-value-id :}
   HIR-OPCODE:FLT x y FCMP2 {: f:IR-ID:ir-value-id :}
   f 1 2 BRZ2
   BLOCK+
   y 3 BR1
   BLOCK+
   x 3 BR1
   BLOCK+
   FARG+ drop
   xc RET1
   CLOSE-FUN ;

: BUILD-FSEL-CELL ( -- )
   2 1 OPEN-FUN
   ARG+ {: xc:IR-ID:ir-value-id :}
   ARG+ {: yc:IR-ID:ir-value-id :}
   xc CROSS {: x:IR-ID:ir-value-id :}
   yc CROSS {: y:IR-ID:ir-value-id :}
   HIR-OPCODE:FLT x y FCMP2 {: f:IR-ID:ir-value-id :}
   f 1 2 BRZ2
   BLOCK+
   xc 3 BR1
   BLOCK+
   yc 3 BR1
   BLOCK+
   ARG+ RET1
   CLOSE-FUN ;

: BUILD-FSELZ-REAL ( -- )
   2 1 OPEN-FUN
   ARG+ {: xc:IR-ID:ir-value-id :}
   ARG+ {: yc:IR-ID:ir-value-id :}
   xc CROSS {: x:IR-ID:ir-value-id :}
   yc CROSS {: y:IR-ID:ir-value-id :}
   HIR-OPCODE:FLTZ x FCMP1 {: f:IR-ID:ir-value-id :}
   f 1 2 BRZ2
   BLOCK+
   x 3 BR1
   BLOCK+
   y 3 BR1
   BLOCK+
   FARG+ drop
   xc RET1
   CLOSE-FUN ;

: BUILD-FSELZ-CELL ( -- )
   2 1 OPEN-FUN
   ARG+ {: xc:IR-ID:ir-value-id :}
   ARG+ {: yc:IR-ID:ir-value-id :}
   xc CROSS {: x:IR-ID:ir-value-id :}
   HIR-OPCODE:FEQZ x FCMP1 {: f:IR-ID:ir-value-id :}
   f 1 2 BRZ2
   BLOCK+
   xc 3 BR1
   BLOCK+
   yc 3 BR1
   BLOCK+
   ARG+ RET1
   CLOSE-FUN ;

\ ---- running the pass --------------------------------------------------------
: A64-BUILDER ( -- IR-BUILD:builder )
   IR-BUILD:PLAN-BEGIN
   IR-BUILD:PLAN-DEFAULT
   CC A64IR:NEW-BUILDER ;

\ The contract these fixtures select under. Every one of them is about the BODY
\ of a selection, so the convention names no place at all - the selector then
\ adds no entry and no exit, and what the case reads back is exactly what the
\ source module's operations selected to. The data-stack convention has its own
\ cases further down, which is where an entry and an exit are the subject.
: NO-PLACES ( -- A64EFF:routine )
   A64EFF:SEQ-NONE A64EFF:SEQ-NONE A64EFF:GPR-NONE
   A64EFF:FPR-NONE A64EFF:FPR-NONE A64EFF:FPR-NONE
   A64EFF-NZCV:UNTOUCHED A64EFF-LINK:PRESERVED A64EFF-CONTROL:RETURNS
   A64EFF:TRAITS-NONE 0 0 A64EFF:ROUTINE ;

\ The same contract with the whole general file to hand out. The cases about a
\ branching or selecting body run under this one, because whether a selection
\ may become a select at all is partly a question about the ROUTINE: a machine
\ select reads its sources at one instant, so a routine with no registers to
\ hand out cannot hold one and the conversion is refused for that reason alone.
\ Under the contract above every such case would be refused by the pool and none
\ of them would be asking about the rule they are written for.
: POOLED ( -- A64EFF:routine )
   A64EFF:SEQ-NONE A64EFF:SEQ-NONE A64EFF:GPR-ALL
   A64EFF:FPR-NONE A64EFF:FPR-NONE A64EFF:FPR-NONE
   A64EFF-NZCV:UNTOUCHED A64EFF-LINK:PRESERVED A64EFF-CONTROL:RETURNS
   A64EFF:TRAITS-NONE 0 0 A64EFF:ROUTINE ;

\ And the same with the whole FLOATING file to hand out as well, which is what
\ src/compiler/native/abi.f declares for every routine the chain really
\ compiles: both of its conventions name the whole D file as destroyed, so a
\ compiled word's floating pool is all thirty-two registers. A selection whose
\ join carries a DOUBLE is held against that pool exactly as a cell selection is
\ held against the general one, so the cases about one run under this contract
\ and the contract above is what the refusal case uses - a routine that may
\ write no floating register at all cannot hold an Fcsel, whatever else is true
\ of it.
: POOLED-FP ( -- A64EFF:routine )
   A64EFF:SEQ-NONE A64EFF:SEQ-NONE A64EFF:GPR-ALL
   A64EFF:FPR-NONE A64EFF:FPR-NONE A64EFF:FPR-ALL
   A64EFF-NZCV:UNTOUCHED A64EFF-LINK:PRESERVED A64EFF-CONTROL:RETURNS
   A64EFF:TRAITS-NONE 0 0 A64EFF:ROUTINE ;

\ And a contract that hands out EXACTLY n floating registers, which is what turns
\ each form's floating floor from a number in a comment into a measured boundary:
\ the same module is selected once with one register too few and once with
\ enough, and only the second one converts. The registers are the lowest n
\ because which ones they are decides nothing here - a floor is a count.
: FPR-N ( n -- A64EFF:fprs )
   {: n:n :}
   A64EFF:FPR-NONE
   n 0 ?do  i A64EFF:FPR-REG A64EFF:FPR-WITH  loop ;

: POOLED-FN ( n -- A64EFF:routine )
   {: n:n :}
   A64EFF:SEQ-NONE A64EFF:SEQ-NONE A64EFF:GPR-ALL
   A64EFF:FPR-NONE A64EFF:FPR-NONE n FPR-N
   A64EFF-NZCV:UNTOUCHED A64EFF-LINK:PRESERVED A64EFF-CONTROL:RETURNS
   A64EFF:TRAITS-NONE 0 0 A64EFF:ROUTINE ;

\ Bind the source dialect while the module is still live, freeze it, and select.
: SELECTED ( -- IR-BUILD:module )
   CC BB A64SEL:BIND-SOURCE
   CC BB IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   CC m A64-BUILDER TXT TXT-N NO-PLACES A64SEL:SELECT ;

: SELECTED-POOL ( -- IR-BUILD:module )
   CC BB A64SEL:BIND-SOURCE
   CC BB IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   CC m A64-BUILDER TXT TXT-N POOLED A64SEL:SELECT ;

: SELECTED-POOL-FP ( -- IR-BUILD:module )
   CC BB A64SEL:BIND-SOURCE
   CC BB IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   CC m A64-BUILDER TXT TXT-N POOLED-FP A64SEL:SELECT ;

: SELECTED-POOL-FN ( n -- IR-BUILD:module )
   {: n:n :}
   CC BB A64SEL:BIND-SOURCE
   CC BB IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   CC m A64-BUILDER TXT TXT-N  n POOLED-FN  A64SEL:SELECT ;

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

: BLK0 ( -- IR-ID:ir-block-id )
   RF R-BLKR RV RK  RK 0 IR-ID:PACK-FUN  0 IR-FUN:FBLOCK@ ;

: OP@ ( n -- IR-ID:ir-op-id )
   {: i:n :}
   R-BLKR RV R-OPR RV RK BLK0 i IR-FUN:FOP@ ;

: ARG@ ( n -- IR-ID:ir-value-id )
   {: i:n :}
   R-BLKR RV R-VALR RV RK BLK0 i IR-FUN:FARG@ ;

: OPCODE-IS? ( n ptr u8 n -- bool )
   {: i:n p u:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
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
   R-BLKR RV BLK0 IR-FUN:FOP-COUNT ;

\ How many blocks the selected function has. The if-conversion is the one thing
\ in this pass that changes the number, so a case that asserts the branch is
\ gone asserts this too: a select in a module that still had four blocks would
\ be a select with the branch still beside it.
: BLOCKS ( -- n )
   RF  RK 0 IR-ID:PACK-FUN  IR-FUN:FBLOCK-COUNT ;

: ATTR-INT ( n n -- n )
   {: i:n k:n :}
   R-ATTR RV  R-OPP RV R-OPR RV RK i OP@ k IR-OP:FATTR@  IR-ATTR:FINT@ ;

: ATTR-KEY-IS? ( n n ptr u8 n -- bool )
   {: i:n k:n p u:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   R-SYMP RV R-SYMR RV
   R-OPP RV R-OPR RV RK i OP@ k IR-OP:FATTR-KEY@
   p u IR-SYM:FEQ? ;

\ How many values one operation of the entry block reads. It is what separates a
\ comparison against the instruction's own zero from one against a register: the
\ zero forms take one operand and the register forms take two.
: OPERANDS ( n -- n )
   {: i:n :}
   R-OPR RV i OP@ IR-OP:FOPERANDS ;

\ Which block a terminator of the entry block hands control to, as its ordinal.
: SUCC@ ( n n -- n )
   {: i:n k:n :}
   R-OPP RV R-OPR RV RK i OP@ k IR-OP:FSUCCESSOR@ IR-ID:BLOCK-LOCAL ;

\ ---- the multiply of `dup *` -------------------------------------------------
: SQUARE-BODY ( IR-CTX:ctx -- n n n n bool bool bool bool bool )
   HIR-MOD
   BUILD-SQUARE
   SELECTED READ!
   RF IR-FUN:FFUNS
   RF  RK 0 IR-ID:PACK-FUN  IR-FUN:FBLOCK-COUNT
   R-BLKR RV BLK0 IR-FUN:FARG-COUNT
   OPS
   0 s" a64.mul" OPCODE-IS?
   0 0 OPERAND@ 0 ARG@ SAME-VALUE?
   0 1 OPERAND@ 0 ARG@ SAME-VALUE?
   1 s" a64.ret" OPCODE-IS?
   1 0 OPERAND@ 0 0 RESULT@ SAME-VALUE? ;

: SQUARE-CASE ( -- )
   s" a multiply selects to one machine multiply over the same value twice" T-LABEL
   WBND [: SQUARE-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE TTRUE TTRUE 2 T= 1 T= 1 T= 1 T= ;

\ ---- the subtraction, where operand order is visible -------------------------
: DIFF-BODY ( IR-CTX:ctx -- bool bool bool bool bool )
   HIR-MOD
   BUILD-DIFF
   SELECTED READ!
   0 s" a64.sub" OPCODE-IS?
   0 s" a64.add" OPCODE-IS?
   0 0 OPERAND@ 0 ARG@ SAME-VALUE?
   0 1 OPERAND@ 1 ARG@ SAME-VALUE?
   0 0 OPERAND@ 1 ARG@ SAME-VALUE? ;

: DIFF-CASE ( -- )
   s" a subtraction selects to a subtraction with its operands in order" T-LABEL
   WBND [: DIFF-BODY ;] IR-CTX:WITH-CONTEXT
   TFALSE TTRUE TTRUE TFALSE TTRUE ;

\ A division selects even though its schema declares that it may trap, because
\ the machine form it selects to KEEPS the trap: a64.sdiv is the zero-divisor
\ guard and the divide together. That is the whole difference between this case
\ and the trapping-addition refusal further down, and it is why the trap gate
\ asks which opcode it is looking at rather than only whether the flag is set.
: DIV-BODY ( IR-CTX:ctx -- bool bool bool bool )
   HIR-MOD
   BUILD-DIV
   SELECTED READ!
   0 s" a64.sdiv" OPCODE-IS?
   0 s" a64.mul" OPCODE-IS?
   0 0 OPERAND@ 0 ARG@ SAME-VALUE?
   0 1 OPERAND@ 1 ARG@ SAME-VALUE? ;

: DIV-CASE ( -- )
   s" a division selects to the machine divide with its operands in order" T-LABEL
   WBND [: DIV-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TFALSE TTRUE ;

: ADD-BODY ( IR-CTX:ctx -- bool bool )
   HIR-MOD
   BUILD-ADD
   SELECTED READ!
   0 s" a64.add" OPCODE-IS?
   0 s" a64.sub" OPCODE-IS? ;

: ADD-CASE ( -- )
   s" an addition selects to an addition and not to its neighbour" T-LABEL
   WBND [: ADD-BODY ;] IR-CTX:WITH-CONTEXT
   TFALSE TTRUE ;


\ ---- the selection that becomes a select ------------------------------------
\ WHAT THE CONVERSION IS, MEASURED. The four-block shape becomes TWO blocks: the
\ entry, which now holds the machine select, the copy that carries its answer
\ across the one edge left, and that edge's branch; and the join, unchanged. The
\ two arms are gone, and with them the two-way branch - which is the whole point
\ of the transform and the thing the placement measurement asked for
\ (docs/codegen-placement.md).
\
\ THE OPERANDS SAY THE POLARITY IS RIGHT, and they are why this case reads four
\ of them rather than counting instructions. A source `<` answers a flag that is
\ true when the relation holds, and the source branch takes its FIRST successor
\ when that flag is ZERO - the arm the relation did NOT choose. A Csel writes its
\ first source when the condition holds. So the fused select compares x against
\ y under `lt` and takes the value the SECOND arm handed over, y, as its first
\ source: `csel d, y, x, lt` is `x < y ? y : x`, which is what the source
\ computes. A select whose two sources were the other way round is the other
\ arm on every unequal pair, and these two operand assertions are what catches
\ it.
: SELECT-BODY ( IR-CTX:ctx -- n n bool bool bool bool bool bool bool n )
   HIR-MOD
   HIR-OPCODE:LT false BUILD-SELECT
   SELECTED-POOL READ!
   BLOCKS
   OPS
   0 s" a64.cmpsel" OPCODE-IS?
   0 s" a64.cmpbr" OPCODE-IS?
   0 0 OPERAND@ 0 ARG@ SAME-VALUE?
   0 1 OPERAND@ 1 ARG@ SAME-VALUE?
   0 2 OPERAND@ 1 ARG@ SAME-VALUE?
   0 3 OPERAND@ 0 ARG@ SAME-VALUE?
   0 0 s" a64.cond" ATTR-KEY-IS?
   0 0 ATTR-INT ;

: SELECT-CASE ( -- )
   s" a selection whose arms are single values becomes a select and no branch"
   T-LABEL
   WBND [: SELECT-BODY ;] IR-CTX:WITH-CONTEXT
   A64IR-COND:LT A64IR:COND-CODE T=
   TTRUE TTRUE TTRUE TTRUE TTRUE TFALSE TTRUE
   3 T= 2 T= ;

\ The rest of the converted entry block: the answer is copied into a value of
\ its own and handed to the join by an ordinary one-way branch, exactly as every
\ other argument-carrying edge in this pass crosses. There is no two-way branch
\ left anywhere in it.
: SELECT-REST-BODY ( IR-CTX:ctx -- bool bool bool )
   HIR-MOD
   HIR-OPCODE:LT false BUILD-SELECT
   SELECTED-POOL READ!
   1 s" a64.mov" OPCODE-IS?
   2 s" a64.b" OPCODE-IS?
   2 s" a64.cbz" OPCODE-IS? ;

: SELECT-REST-CASE ( -- )
   s" the converted block ends in one unconditional branch to the join" T-LABEL
   WBND [: SELECT-REST-BODY ;] IR-CTX:WITH-CONTEXT
   TFALSE TTRUE TTRUE ;

\ A selection on a value no comparison beside it computed. There is nothing to
\ fuse, so the machine compares the value against zero itself and selects on
\ `ne` - which is the same polarity by a different route, because hir.brz takes
\ its second successor when the value is NOT zero.
: SELECT-VALUE-BODY ( IR-CTX:ctx -- n bool bool bool bool bool )
   HIR-MOD
   BUILD-SELECT-VALUE
   SELECTED-POOL READ!
   BLOCKS
   0 s" a64.selz" OPCODE-IS?
   0 s" a64.cmpsel" OPCODE-IS?
   0 0 OPERAND@ 0 ARG@ SAME-VALUE?
   0 1 OPERAND@ 2 ARG@ SAME-VALUE?
   0 2 OPERAND@ 1 ARG@ SAME-VALUE? ;

: SELECT-VALUE-CASE ( -- )
   s" a selection on a plain value becomes a zero test and a select" T-LABEL
   WBND [: SELECT-VALUE-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE TFALSE TTRUE 2 T= ;

\ Two arms that hand the join the SAME value need no select at all: there is
\ nothing to choose between. The branch still goes, because the region is still
\ admissible - what disappears is only the instruction that would have chosen.
\ This is also what a memory order crossing a converted selection is, and it is
\ the reason the conversion needs no case for one.
: SELECT-SAME-BODY ( IR-CTX:ctx -- n bool bool bool )
   HIR-MOD
   HIR-OPCODE:LT true BUILD-SELECT
   SELECTED-POOL READ!
   OPS
   0 s" a64.flag" OPCODE-IS?
   1 s" a64.mov" OPCODE-IS?
   2 s" a64.b" OPCODE-IS? ;

: SELECT-SAME-CASE ( -- )
   s" two arms handing over one value need no select" T-LABEL
   WBND [: SELECT-SAME-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE 3 T= ;

\ ---- the same selection over doubles ----------------------------------------
\ THE ADMISSION BOUNDARY, BOTH SIDES OF IT. The fused-branch fixture above is
\ this same selection with a division in one arm, and it still branches:
\ FUSE-CASE reads a64.cmpbr out of it, because a division may raise on a path
\ the program would not have taken. This one is the same selection with a DOUBLE
\ at the join, and it converts: what a double changes is which register file the
\ select answers in, not whether the region may be converted at all.
\
\ WHAT THE TYPE DECIDES, MEASURED. The entry block's first two operations are
\ the crossings that read the two argument cells as doubles; the comparison is
\ the general one, over the two CELLS, so it fuses into the select exactly as it
\ does when the answers are cells; and the select that comes out is a64.cmpseld,
\ which is a64.cmpsel with its two chosen-between operands and its result in the
\ floating file. Reading the opcode by name is what separates the two: a
\ conversion that chose the general form would be a Csel moving eight bytes out
\ of a register that does not hold them.
\
\ THE OPERANDS SAY THE POLARITY IS RIGHT, and they say it here for the same
\ reason SELECT-CASE reads four of them. A Csel and an Fcsel both write their
\ FIRST source when the condition holds, and the arm a Habu `if` takes is the
\ source branch's SECOND successor, so the second arm's value is the first
\ source. The swapped fixture below is the control: it is the same module with
\ the two arms exchanged, and it must answer the two crossings the other way
\ round. Without it these assertions would hold for a pass that ignored the
\ source order entirely.
: SELECT-REAL-BODY ( IR-CTX:ctx -- n n bool bool bool bool bool n )
   HIR-MOD
   BUILD-SELECT-REAL
   SELECTED-POOL-FP READ!
   BLOCKS
   OPS
   2 s" a64.cmpseld" OPCODE-IS?
   2 s" a64.cmpsel" OPCODE-IS?
   2 2 OPERAND@  1 0 RESULT@  SAME-VALUE?
   2 3 OPERAND@  0 0 RESULT@  SAME-VALUE?
   2 0 s" a64.cond" ATTR-KEY-IS?
   2 0 ATTR-INT ;

: SELECT-REAL-CASE ( -- )
   s" a selection whose join carries a double becomes a select in the D file"
   T-LABEL
   WBND [: SELECT-REAL-BODY ;] IR-CTX:WITH-CONTEXT
   A64IR-COND:LT A64IR:COND-CODE T=
   TTRUE TTRUE TTRUE TFALSE TTRUE
   5 T= 2 T= ;

: SELECT-REAL-SWAPPED-BODY ( IR-CTX:ctx -- bool bool bool )
   HIR-MOD
   BUILD-SELECT-REAL-SWAPPED
   SELECTED-POOL-FP READ!
   2 s" a64.cmpseld" OPCODE-IS?
   2 2 OPERAND@  0 0 RESULT@  SAME-VALUE?
   2 3 OPERAND@  1 0 RESULT@  SAME-VALUE? ;

: SELECT-REAL-SWAPPED-CASE ( -- )
   s" exchanging the two arms exchanges the two sources of the float select"
   T-LABEL
   WBND [: SELECT-REAL-SWAPPED-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE ;

\ The rest of the converted block, which is where the OTHER thing a double
\ changes shows: the answer crosses the one edge that is left as an a64.fmovdd
\ and not as the general move every cell selection copies with. A copy made with
\ the wrong one moves the low eight bytes of a register the double is not in.
: SELECT-REAL-REST-BODY ( IR-CTX:ctx -- bool bool bool bool )
   HIR-MOD
   BUILD-SELECT-REAL
   SELECTED-POOL-FP READ!
   3 s" a64.fmovdd" OPCODE-IS?
   3 s" a64.mov" OPCODE-IS?
   4 s" a64.b" OPCODE-IS?
   4 s" a64.cbz" OPCODE-IS? ;

: SELECT-REAL-REST-CASE ( -- )
   s" the double the region chose crosses its one edge as a floating copy"
   T-LABEL
   WBND [: SELECT-REAL-REST-BODY ;] IR-CTX:WITH-CONTEXT
   TFALSE TTRUE TFALSE TTRUE ;

\ THE OTHER SIDE OF THE NEW BOUNDARY, AND IT IS A POOL AND NOT A TYPE. The very
\ same module, selected under a contract that may write no floating register,
\ keeps its branch: an Fcsel reads its two sources at one instant and a routine
\ with no register of that file to hand out cannot hold one, whatever it puts
\ away. That is the same floor the general pool has always been held against,
\ asked of the file the answers live in - and a rule that admitted a double
\ without asking it would hand the allocator a routine it must refuse, turning
\ this optimisation into a compilation failure.
: SELECT-REAL-REFUSE-BODY ( IR-CTX:ctx -- n bool bool )
   HIR-MOD
   BUILD-SELECT-REAL
   SELECTED-POOL READ!
   BLOCKS
   2 s" a64.cmpbr" OPCODE-IS?
   2 s" a64.cmpseld" OPCODE-IS? ;

: SELECT-REAL-REFUSE-CASE ( -- )
   s" a routine with no floating register to hand out keeps its branch" T-LABEL
   WBND [: SELECT-REAL-REFUSE-BODY ;] IR-CTX:WITH-CONTEXT
   TFALSE TTRUE 4 T= ;

\ ---- the four selects whose flags an Fcmp wrote -----------------------------
\ A FLOAT comparison deciding between two doubles, which is RELU-F's file and
\ MAX-F's comparison. The comparison FUSES: it selects to no operation of its
\ own, and what comes out is one a64.fcmpseld over the two doubles compared and
\ the two chosen between. Written the long way it would be an a64.fflag - Fcmp,
\ Cset, Sub - and then a zero-test select on the number it answered, which is
\ five instructions and a register where this is two and none.
\
\ THE CONDITION IS THE NEGATIVE CONTROL. `f<` reads as less-than and the machine
\ condition called less-than is `lt`, which is TRUE when an Fcmp raises the
\ unordered flag; the table gives `mi`, which is false there. The two agree on
\ every ordered pair, so this assertion is the ONLY thing in the structural
\ suite that separates them, and it is what the NaN rows of
\ tools/codegen-compare-corpus3.f measure end to end.
: FSEL-REAL-BODY ( IR-CTX:ctx -- n n bool bool bool bool bool bool bool bool n )
   HIR-MOD
   BUILD-FSEL-REAL
   SELECTED-POOL-FP READ!
   BLOCKS
   OPS
   2 s" a64.fcmpseld" OPCODE-IS?
   2 s" a64.cmpseld" OPCODE-IS?
   2 s" a64.selzd" OPCODE-IS?
   2 s" a64.fflag" OPCODE-IS?
   2 0 OPERAND@  0 0 RESULT@  SAME-VALUE?
   2 1 OPERAND@  1 0 RESULT@  SAME-VALUE?
   2 2 OPERAND@  1 0 RESULT@  SAME-VALUE?
   2 3 OPERAND@  0 0 RESULT@  SAME-VALUE?
   2 0 ATTR-INT ;

: FSEL-REAL-CASE ( -- )
   s" a float compare choosing between doubles fuses into one select under mi"
   T-LABEL
   WBND [: FSEL-REAL-BODY ;] IR-CTX:WITH-CONTEXT
   A64IR-COND:MI A64IR:COND-CODE T=
   TTRUE TTRUE TTRUE TTRUE TFALSE TFALSE TFALSE TTRUE
   5 T= 2 T= ;

: FSEL-REAL-SWAPPED-BODY ( IR-CTX:ctx -- bool bool bool )
   HIR-MOD
   BUILD-FSEL-REAL-SWAPPED
   SELECTED-POOL-FP READ!
   2 s" a64.fcmpseld" OPCODE-IS?
   2 2 OPERAND@  0 0 RESULT@  SAME-VALUE?
   2 3 OPERAND@  1 0 RESULT@  SAME-VALUE? ;

: FSEL-REAL-SWAPPED-CASE ( -- )
   s" exchanging the two arms exchanges the two chosen sources and nothing else"
   T-LABEL
   WBND [: FSEL-REAL-SWAPPED-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE ;

\ The same comparison with the argument CELLS at the join, which is MAX-F's own
\ shape. What changes is one thing: the answer is chosen with a Csel and not an
\ Fcsel, so the form is a64.fcmpsel. The compared operands are still the two
\ doubles, which is what makes this the corner with the leading `f` and no
\ trailing one.
: FSEL-CELL-BODY ( IR-CTX:ctx -- n n bool bool bool bool bool bool bool n )
   HIR-MOD
   BUILD-FSEL-CELL
   SELECTED-POOL-FP READ!
   BLOCKS
   OPS
   2 s" a64.fcmpsel" OPCODE-IS?
   2 s" a64.cmpsel" OPCODE-IS?
   2 s" a64.fcmpseld" OPCODE-IS?
   2 0 OPERAND@  0 0 RESULT@  SAME-VALUE?
   2 1 OPERAND@  1 0 RESULT@  SAME-VALUE?
   2 2 OPERAND@  1 ARG@  SAME-VALUE?
   2 3 OPERAND@  0 ARG@  SAME-VALUE?
   2 0 ATTR-INT ;

: FSEL-CELL-CASE ( -- )
   s" the same compare choosing between cells is the general-answer form"
   T-LABEL
   WBND [: FSEL-CELL-BODY ;] IR-CTX:WITH-CONTEXT
   A64IR-COND:MI A64IR:COND-CODE T=
   TTRUE TTRUE TTRUE TTRUE TFALSE TFALSE TTRUE
   5 T= 2 T= ;

\ The comparison against the instruction's own zero, which is RELU-F's shape.
\ Its operand list is one SHORTER, because the zero is a form of the Fcmp and
\ not a register anything computed - so the chosen pair starts at operand one.
: FSELZ-REAL-BODY ( IR-CTX:ctx -- n n bool bool bool bool bool bool n )
   HIR-MOD
   BUILD-FSELZ-REAL
   SELECTED-POOL-FP READ!
   BLOCKS
   OPS
   2 s" a64.fcmpselzd" OPCODE-IS?
   2 s" a64.fcmpseld" OPCODE-IS?
   2 s" a64.selzd" OPCODE-IS?
   2 0 OPERAND@  0 0 RESULT@  SAME-VALUE?
   2 1 OPERAND@  1 0 RESULT@  SAME-VALUE?
   2 2 OPERAND@  0 0 RESULT@  SAME-VALUE?
   2 0 ATTR-INT ;

: FSELZ-REAL-CASE ( -- )
   s" a float zero compare choosing between doubles reads one register and mi"
   T-LABEL
   WBND [: FSELZ-REAL-BODY ;] IR-CTX:WITH-CONTEXT
   A64IR-COND:MI A64IR:COND-CODE T=
   TTRUE TTRUE TTRUE TFALSE TFALSE TTRUE
   5 T= 2 T= ;

\ And the fourth corner, on `f0=` so that the condition assertion is a second
\ row of the table rather than a third reading of the same one.
: FSELZ-CELL-BODY ( IR-CTX:ctx -- n n bool bool bool bool bool n )
   HIR-MOD
   BUILD-FSELZ-CELL
   SELECTED-POOL-FP READ!
   BLOCKS
   OPS
   1 s" a64.fcmpselz" OPCODE-IS?
   1 s" a64.selz" OPCODE-IS?
   1 0 OPERAND@  0 0 RESULT@  SAME-VALUE?
   1 1 OPERAND@  1 ARG@  SAME-VALUE?
   1 2 OPERAND@  0 ARG@  SAME-VALUE?
   1 0 ATTR-INT ;

: FSELZ-CELL-CASE ( -- )
   s" a float zero compare choosing between cells is the fourth corner, under equal"
   T-LABEL
   WBND [: FSELZ-CELL-BODY ;] IR-CTX:WITH-CONTEXT
   A64IR-COND:EQUAL A64IR:COND-CODE T=
   TTRUE TTRUE TTRUE TFALSE TTRUE
   4 T= 2 T= ;

\ ---- the floating floor each of the four sits on ----------------------------
\ A fused float select reads its compared D registers AND its chosen ones at one
\ instant, so the floor it is held against is the sum of the two - and the whole
\ point of this pair of cases is that the COMPARED half is now counted. Before
\ the fusion the compare was an a64.fflag standing on its own and the select's
\ floating floor was the arms alone; a rule that still counted only the arms
\ would put BUILD-FSEL-REAL's floor at two, and three registers would be enough.
\ It is four - two doubles compared plus two chosen - so three is not, and the
\ region stays branched.
\
\ THE PRESSURE QUESTION DOES NOT ANSWER THIS ONE, which is why three is the
\ number chosen. That region speculates two doubles and hands one across, so its
\ floating PRESSURE is three: at a pool of three the pressure test passes and
\ only the floor refuses, and at four both pass. The pair therefore reads the
\ floor and nothing else.
: FSEL-REAL-POOL3-BODY ( IR-CTX:ctx -- n bool bool )
   HIR-MOD
   BUILD-FSEL-REAL
   3 SELECTED-POOL-FN READ!
   BLOCKS
   2 s" a64.fcmpbr" OPCODE-IS?
   2 s" a64.fcmpseld" OPCODE-IS? ;

: FSEL-REAL-POOL4-BODY ( IR-CTX:ctx -- n bool bool )
   HIR-MOD
   BUILD-FSEL-REAL
   4 SELECTED-POOL-FN READ!
   BLOCKS
   2 s" a64.fcmpseld" OPCODE-IS?
   2 s" a64.fcmpbr" OPCODE-IS? ;

: FSEL-REAL-POOL-CASE ( -- )
   s" three floating registers cannot hold the fused float select and four can"
   T-LABEL
   WBND [: FSEL-REAL-POOL3-BODY ;] IR-CTX:WITH-CONTEXT
   TFALSE TTRUE 4 T=
   WBND [: FSEL-REAL-POOL4-BODY ;] IR-CTX:WITH-CONTEXT
   TFALSE TTRUE 2 T= ;

\ The other side of the same floor, and the one that says the compared half is
\ counted against the FLOATING file and not the general one. MAX-F's shape
\ chooses between two cells, so its general floor is the two arms alone and its
\ floating floor is the two doubles the Fcmp reads. A routine that may write no
\ floating register at all therefore keeps its branch even though every value
\ the select CHOOSES is a cell - which is exactly what changed, because the old
\ lowering put a Cset between the Fcmp and the select and asked nothing of the
\ floating pool at all.
: FSEL-CELL-REFUSE-BODY ( IR-CTX:ctx -- n bool bool )
   HIR-MOD
   BUILD-FSEL-CELL
   SELECTED-POOL READ!
   BLOCKS
   2 s" a64.fcmpbr" OPCODE-IS?
   2 s" a64.fcmpsel" OPCODE-IS? ;

: FSEL-CELL-REFUSE-CASE ( -- )
   s" a routine with no floating register keeps its branch even to choose cells"
   T-LABEL
   WBND [: FSEL-CELL-REFUSE-BODY ;] IR-CTX:WITH-CONTEXT
   TFALSE TTRUE 4 T= ;

\ ---- the fused compare-and-branch --------------------------------------------
\ The entry block of the branching shape holds ONE operation when the comparison
\ fuses: the source comparison selects to nothing at all, and the branch selects
\ to a64.cmpbr over the comparison's own two operands. The two successors carry
\ across in the source order, so the block the source branch reached when its
\ flag was zero is still the first one.
\
\ THE CONDITION IS THE COMPARISON'S OWN AND THE SUCCESSORS ARE SWAPPED, and that
\ is the whole polarity of the pass. A source `<` answers a flag that is true
\ when the relation holds, and the source branch takes its FIRST successor when
\ that flag is ZERO - the arm the relation did not choose - while a64.cmpbr
\ takes its first successor when the condition HOLDS. So the fused branch keeps
\ `lt` and takes the source branch's SECOND successor first: block two here,
\ with block one second. The condition is asserted against the dialect's own
\ code for `lt` rather than against a number, because
\ test/compiler/native-a64ir.f is what holds that code against the assembler.
: FUSE-BODY ( IR-CTX:ctx -- n bool bool bool bool bool n n n )
   HIR-MOD
   HIR-OPCODE:LT false BUILD-BRANCH
   SELECTED-POOL READ!
   OPS
   0 s" a64.cmpbr" OPCODE-IS?
   0 s" a64.flag" OPCODE-IS?
   0 0 OPERAND@ 0 ARG@ SAME-VALUE?
   0 1 OPERAND@ 1 ARG@ SAME-VALUE?
   0 0 s" a64.cond" ATTR-KEY-IS?
   0 0 ATTR-INT
   0 0 SUCC@
   0 1 SUCC@ ;

: FUSE-CASE ( -- )
   s" a single-use comparison and its branch select to one compare-and-branch"
   T-LABEL
   WBND [: FUSE-BODY ;] IR-CTX:WITH-CONTEXT
   1 T= 2 T=
   A64IR-COND:LT A64IR:COND-CODE T=
   TTRUE TTRUE TTRUE TFALSE TTRUE 1 T= ;

\ The other two comparisons, so the whole polarity table is pinned and not just
\ the row the corpus happens to exercise most: `<=` fuses under `le` and `=`
\ under `equal`, each with the source branch's second successor first. A
\ condition carried across as some other condition sends the fused branch down
\ the arm the source would not have taken.
: FUSE-LE-BODY ( IR-CTX:ctx -- bool n )
   HIR-MOD
   HIR-OPCODE:LE false BUILD-BRANCH
   SELECTED-POOL READ!
   0 s" a64.cmpbr" OPCODE-IS?
   0 0 ATTR-INT ;

: FUSE-LE-CASE ( -- )
   s" a fused less-or-equal branches on less-or-equal" T-LABEL
   WBND [: FUSE-LE-BODY ;] IR-CTX:WITH-CONTEXT
   A64IR-COND:LE A64IR:COND-CODE T= TTRUE ;

: FUSE-EQ-BODY ( IR-CTX:ctx -- bool n )
   HIR-MOD
   HIR-OPCODE:EQUAL false BUILD-BRANCH
   SELECTED-POOL READ!
   0 s" a64.cmpbr" OPCODE-IS?
   0 0 ATTR-INT ;

: FUSE-EQ-CASE ( -- )
   s" a fused equality branches on equal" T-LABEL
   WBND [: FUSE-EQ-BODY ;] IR-CTX:WITH-CONTEXT
   A64IR-COND:EQUAL A64IR:COND-CODE T= TTRUE ;

\ The same shape with the comparison's answer handed on to the join as well. It
\ now has three readers, so the flag really is needed as a number and the pair
\ stays two operations - the comparison under its OWN condition, and the
\ two-way branch over the value it defines. This is the fixture that makes the
\ use count load-bearing: remove the count and the entry block comes out one
\ operation, and the value the two arms hand over is defined by nothing.
: NOFUSE-BODY ( IR-CTX:ctx -- n bool bool bool bool n )
   HIR-MOD
   HIR-OPCODE:LT true BUILD-BRANCH
   SELECTED-POOL READ!
   OPS
   0 s" a64.flag" OPCODE-IS?
   1 s" a64.cbz" OPCODE-IS?
   0 s" a64.cmpbr" OPCODE-IS?
   1 0 OPERAND@ 0 0 RESULT@ SAME-VALUE?
   0 0 ATTR-INT ;

: NOFUSE-CASE ( -- )
   s" a comparison read a second time keeps its flag and its branch" T-LABEL
   WBND [: NOFUSE-BODY ;] IR-CTX:WITH-CONTEXT
   A64IR-COND:LT A64IR:COND-CODE T=
   TTRUE TFALSE TTRUE TTRUE 2 T= ;

\ And a comparison with no branch under it at all: its answer is what the word
\ leaves, so it is materialised exactly as before.
: FLAG-VALUE-BODY ( IR-CTX:ctx -- n bool bool bool )
   HIR-MOD
   BUILD-FLAG-VALUE
   SELECTED READ!
   OPS
   0 s" a64.flag" OPCODE-IS?
   1 s" a64.ret" OPCODE-IS?
   0 s" a64.cmpbr" OPCODE-IS? ;

: FLAG-VALUE-CASE ( -- )
   s" a comparison whose answer is the word's result keeps its flag" T-LABEL
   WBND [: FLAG-VALUE-BODY ;] IR-CTX:WITH-CONTEXT
   TFALSE TTRUE TTRUE 2 T= ;

\ ---- the fused FLOAT compare-and-branch --------------------------------------
\ The same three assertions the integer fusion gets, over the five float words:
\ the entry block holds ONE operation and it is the fused form, the operands are
\ the values the comparison was given in the comparison's order, and the
\ successors are the source branch's the other way round. What is added is which
\ machine form each shape becomes - the two-register Fcmp for a comparison of two
\ doubles and the compare-with-zero form for a comparison against zero - and the
\ condition each source word is made under.
\
\ THE CONDITIONS ARE THE POINT OF THIS CASE. A float less-than is lowered under
\ `mi` and NOT under `lt`, which is what makes a compiled `f<` answer false for a
\ NaN the way the interpreted one does: an Fcmp raises the unordered condition
\ and `lt` holds under it while `mi` does not. They are asserted against the
\ dialect's own codes rather than against numbers, because
\ test/compiler/native-a64ir.f is what holds those codes against the assembler,
\ and each one is also asserted to DIFFER from the condition a table that read
\ the relation's name would have picked.
: FFUSE-BODY ( IR-CTX:ctx -- n bool bool bool bool bool bool n n n )
   HIR-MOD
   HIR-OPCODE:FLT false BUILD-FBRANCH
   SELECTED-POOL READ!
   OPS
   2 s" a64.fcmpbr" OPCODE-IS?
   2 s" a64.cmpbr" OPCODE-IS?
   2 s" a64.fflag" OPCODE-IS?
   2 0 OPERAND@ 0 0 RESULT@ SAME-VALUE?
   2 1 OPERAND@ 1 0 RESULT@ SAME-VALUE?
   2 0 s" a64.cond" ATTR-KEY-IS?
   2 0 ATTR-INT
   2 0 SUCC@
   2 1 SUCC@ ;

: FFUSE-CASE ( -- )
   s" a single-use float comparison and its branch select to one float compare-and-branch"
   T-LABEL
   WBND [: FFUSE-BODY ;] IR-CTX:WITH-CONTEXT
   1 T= 2 T=
   A64IR-COND:MI A64IR:COND-CODE T=
   TTRUE TTRUE TTRUE TFALSE TFALSE TTRUE 3 T= ;

\ The other four, each read back as the machine form it becomes and the
\ condition it is made under. The two against zero become the compare-with-zero
\ form and carry ONE operand, which is what says the zero is the instruction's
\ and not a value something had to compute.
: FFUSE-GT-BODY ( IR-CTX:ctx -- bool n )
   HIR-MOD
   HIR-OPCODE:FGT false BUILD-FBRANCH
   SELECTED-POOL READ!
   2 s" a64.fcmpbr" OPCODE-IS?
   2 0 ATTR-INT ;

: FFUSE-EQ-BODY ( IR-CTX:ctx -- bool n )
   HIR-MOD
   HIR-OPCODE:FEQ false BUILD-FBRANCH
   SELECTED-POOL READ!
   2 s" a64.fcmpbr" OPCODE-IS?
   2 0 ATTR-INT ;

: FFUSE-LTZ-BODY ( IR-CTX:ctx -- bool bool n n )
   HIR-MOD
   HIR-OPCODE:FLTZ BUILD-FZBRANCH
   SELECTED-POOL READ!
   1 s" a64.fcmpbrz" OPCODE-IS?
   1 s" a64.fcmpbr" OPCODE-IS?
   1 OPERANDS
   1 0 ATTR-INT ;

: FFUSE-EQZ-BODY ( IR-CTX:ctx -- bool n n )
   HIR-MOD
   HIR-OPCODE:FEQZ BUILD-FZBRANCH
   SELECTED-POOL READ!
   1 s" a64.fcmpbrz" OPCODE-IS?
   1 OPERANDS
   1 0 ATTR-INT ;

: FFUSE-REST-CASE ( -- )
   s" a fused float greater-than branches on greater-than" T-LABEL
   WBND [: FFUSE-GT-BODY ;] IR-CTX:WITH-CONTEXT
   A64IR-COND:GT A64IR:COND-CODE T= TTRUE

   s" a fused float equality branches on equal" T-LABEL
   WBND [: FFUSE-EQ-BODY ;] IR-CTX:WITH-CONTEXT
   A64IR-COND:EQUAL A64IR:COND-CODE T= TTRUE

   s" a fused comparison against zero takes one operand and branches on `mi`" T-LABEL
   WBND [: FFUSE-LTZ-BODY ;] IR-CTX:WITH-CONTEXT
   A64IR-COND:MI A64IR:COND-CODE T= 1 T= TFALSE TTRUE

   s" and the equality against zero is the same form under `equal`" T-LABEL
   WBND [: FFUSE-EQZ-BODY ;] IR-CTX:WITH-CONTEXT
   A64IR-COND:EQUAL A64IR:COND-CODE T= 1 T= TTRUE

   s" none of the five is lowered under the condition its relation's NAME suggests" T-LABEL
   A64IR-COND:MI A64IR:COND-CODE  A64IR-COND:LT A64IR:COND-CODE  = TFALSE
   A64IR-COND:MI A64IR:COND-CODE  A64IR-COND:LE A64IR:COND-CODE  = TFALSE ;

\ A float comparison read a second time keeps its flag and its branch, exactly as
\ an integer one does - and the flag form is the FLOAT one, so a value of the
\ floating file never reaches the integer compare.
: FNOFUSE-BODY ( IR-CTX:ctx -- n bool bool bool bool n )
   HIR-MOD
   HIR-OPCODE:FLT true BUILD-FBRANCH
   SELECTED-POOL READ!
   OPS
   2 s" a64.fflag" OPCODE-IS?
   3 s" a64.cbz" OPCODE-IS?
   2 s" a64.fcmpbr" OPCODE-IS?
   3 0 OPERAND@ 2 0 RESULT@ SAME-VALUE?
   2 0 ATTR-INT ;

: FNOFUSE-CASE ( -- )
   s" a float comparison read a second time keeps its flag and its branch" T-LABEL
   WBND [: FNOFUSE-BODY ;] IR-CTX:WITH-CONTEXT
   A64IR-COND:MI A64IR:COND-CODE T=
   TTRUE TFALSE TTRUE TTRUE 4 T= ;

\ And a float comparison with no branch under it at all: its answer is what the
\ word leaves, so it is materialised - in the general register file, because a
\ Habu flag is a number.
: FFLAG-VALUE-BODY ( IR-CTX:ctx -- n bool bool bool )
   HIR-MOD
   HIR-OPCODE:FEQ BUILD-FFLAG-VALUE
   SELECTED READ!
   OPS
   2 s" a64.fflag" OPCODE-IS?
   3 s" a64.ret" OPCODE-IS?
   2 s" a64.fcmpbr" OPCODE-IS? ;

: FZFLAG-VALUE-BODY ( IR-CTX:ctx -- n bool n )
   HIR-MOD
   HIR-OPCODE:FEQZ BUILD-FZFLAG-VALUE
   SELECTED READ!
   OPS
   1 s" a64.fflagz" OPCODE-IS?
   1 OPERANDS ;

: FFLAG-VALUE-CASE ( -- )
   s" a float comparison whose answer is the word's result keeps its flag" T-LABEL
   WBND [: FFLAG-VALUE-BODY ;] IR-CTX:WITH-CONTEXT
   TFALSE TTRUE TTRUE 4 T=

   s" and the comparison against zero materialises through the zero form" T-LABEL
   WBND [: FZFLAG-VALUE-BODY ;] IR-CTX:WITH-CONTEXT
   1 T= TTRUE 3 T= ;

\ ---- the move-wide chain -----------------------------------------------------
: SMALL-BODY ( IR-CTX:ctx -- n bool n n bool bool bool )
   HIR-MOD
   BUILD-SMALL
   SELECTED READ!
   OPS
   0 s" a64.movz" OPCODE-IS?
   0 0 ATTR-INT
   0 1 ATTR-INT
   0 0 s" a64.imm" ATTR-KEY-IS?
   0 1 s" a64.shift" ATTR-KEY-IS?
   1 s" a64.ret" OPCODE-IS? ;

: SMALL-CASE ( -- )
   s" a literal inside one half selects to a single move" T-LABEL
   WBND [: SMALL-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE 0 T= 7 T= TTRUE 2 T= ;

: WIDE-BODY ( IR-CTX:ctx -- n bool bool bool n n n n bool bool )
   HIR-MOD
   BUILD-WIDE
   SELECTED READ!
   OPS
   0 s" a64.movz" OPCODE-IS?
   1 s" a64.movk" OPCODE-IS?
   2 s" a64.ret" OPCODE-IS?
   0 0 ATTR-INT
   0 1 ATTR-INT
   1 0 ATTR-INT
   1 1 ATTR-INT
   1 0 OPERAND@ 0 0 RESULT@ SAME-VALUE?
   2 0 OPERAND@ 1 0 RESULT@ SAME-VALUE? ;

: WIDE-CASE ( -- )
   s" a literal across two halves selects to a move and one overwrite" T-LABEL
   WBND [: WIDE-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE
   48 T= $1234 T= 0 T= $5678 T=
   TTRUE TTRUE TTRUE 3 T= ;

\ ---- what the selected function keeps ----------------------------------------
: FUN-BODY ( IR-CTX:ctx -- bool bool bool bool )
   HIR-MOD
   BUILD-DIFF
   SELECTED READ!
   R-SYMP RV R-SYMR RV  RF RK  RK 0 IR-ID:PACK-FUN  IR-FUN:FSYMBOL@
   s" SQUARE" IR-SYM:FEQ?
   RF  RK 0 IR-ID:PACK-FUN  IR-FUN:FLINKAGE@
   IR--FUN-LINKAGE:DEFINED IR--FUN-LINKAGE:EQ
   RF  RK 0 IR-ID:PACK-FUN  IR-FUN:FVISIBILITY@
   IR--FUN-VISIBILITY:EXPORTED IR--FUN-VISIBILITY:EQ
   RF  RK 0 IR-ID:PACK-FUN  IR-FUN:FCONVENTION@
   IR--FUN-CONVENTION:HABU IR--FUN-CONVENTION:EQ ;

: FUN-CASE ( -- )
   s" the selected function keeps its name, linkage, visibility and convention" T-LABEL
   WBND [: FUN-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE TTRUE ;

\ ---- refusals ----------------------------------------------------------------
: NO-BIND-BODY ( IR-CTX:ctx -- )
   HIR-MOD
   BUILD-SQUARE
   CC BB IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   CC m A64-BUILDER TXT TXT-N NO-PLACES A64SEL:SELECT drop ;

: TWICE-BIND-BODY ( IR-CTX:ctx -- )
   HIR-MOD
   CC BB A64SEL:BIND-SOURCE
   CC BB A64SEL:BIND-SOURCE ;

: WRONG-TEXT-BODY ( IR-CTX:ctx -- )
   HIR-MOD
   BUILD-SQUARE
   CC BB A64SEL:BIND-SOURCE
   CC BB IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   CC m A64-BUILDER OTHER-TXT NO-PLACES A64SEL:SELECT drop ;

: WRONG-DIALECT-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   IR-BUILD:PLAN-BEGIN
   IR-BUILD:PLAN-DEFAULT
   c A64IR:NEW-BUILDER {: b:IR-BUILD:builder :}
   c b A64SEL:BIND-SOURCE ;

: TWO-SOURCES-BODY ( IR-CTX:ctx -- )
   HIR-MOD
   BUILD-SQUARE
   CC BB  OTHER-TXT  IR-BUILD:ADD-SOURCE drop
   CC BB A64SEL:BIND-SOURCE
   CC BB IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   CC m A64-BUILDER TXT TXT-N NO-PLACES A64SEL:SELECT drop ;

: EXTRA-OPCODE-BODY ( IR-CTX:ctx -- )
   HIR-MOD
   BUILD-EXTRA
   SELECTED drop ;

: TRAP-BODY ( IR-CTX:ctx -- )
   HIR-MOD
   BUILD-ADD
   SELECTED drop ;

: NO-BIND ( -- )
   WBND [: NO-BIND-BODY ;] IR-CTX:WITH-CONTEXT ;

: TWICE-BIND ( -- )
   WBND [: TWICE-BIND-BODY ;] IR-CTX:WITH-CONTEXT ;

: WRONG-TEXT ( -- )
   WBND [: WRONG-TEXT-BODY ;] IR-CTX:WITH-CONTEXT ;

: WRONG-DIALECT ( -- )
   WBND [: WRONG-DIALECT-BODY ;] IR-CTX:WITH-CONTEXT ;

: TWO-SOURCES ( -- )
   WBND [: TWO-SOURCES-BODY ;] IR-CTX:WITH-CONTEXT ;

: EXTRA-OPCODE ( -- )
   WBND [: EXTRA-OPCODE-BODY ;] IR-CTX:WITH-CONTEXT ;

: TRAPPING ( -- )
   TBND [: TRAP-BODY ;] IR-CTX:WITH-CONTEXT ;

\ Each refusal leaves a binding behind or takes one, so the binding is released
\ between cases and a case never selects against a neighbour's.
: DROP-BINDING ( -- )
   A64SEL:RELEASE ;

: BIND-REFUSE-CASES ( -- )
   s" selecting without a binding is refused" T-LABEL
   [: NO-BIND ;] E-A64SEL-BIND TTHROWSQ
   s" a second binding over a live one is refused" T-LABEL
   [: TWICE-BIND ;] E-A64SEL-BIND TTHROWSQ
   DROP-BINDING ;

: SOURCE-REFUSE-CASES ( -- )
   s" text that is not what the module was compiled from is refused" T-LABEL
   [: WRONG-TEXT ;] E-A64SEL-SOURCE TTHROWSQ
   s" binding a module of another dialect is refused" T-LABEL
   [: WRONG-DIALECT ;] E-A64SEL-SOURCE TTHROWSQ ;

: SHAPE-REFUSE-CASES ( -- )
   s" a module with more than one registered source is refused" T-LABEL
   [: TWO-SOURCES ;] E-A64SEL-SHAPE TTHROWSQ ;

: OPCODE-REFUSE-CASES ( -- )
   s" an operation of an opcode with no selection rule is refused" T-LABEL
   [: EXTRA-OPCODE ;] E-A64SEL-OPCODE TTHROWSQ ;

: TRAP-REFUSE-CASES ( -- )
   s" arithmetic that may trap has no machine lowering and is refused" T-LABEL
   [: TRAPPING ;] E-A64SEL-TRAP TTHROWSQ ;

\ ---- the data-stack convention -----------------------------------------------
\ The same one-argument function, selected under a contract that declares its
\ argument in data-stack slot zero and its result in slot zero. What the case
\ reads back is the whole of what the convention becomes: the block takes no
\ argument at all, the pointer is moved down over one cell, the argument is a
\ load out of slot zero, the result is a store into slot zero, the pointer is
\ moved up over one cell, and the return carries nothing because the result is
\ already published. Each of the four operations is asserted by opcode AND by the
\ field it carries, so an entry that named the wrong slot or moved the pointer by
\ the wrong amount is a different assertion rather than the same shape.
: SLOTS-N ( n -- A64EFF:placeseq )
   {: n:n :}
   A64EFF:SEQ-NONE
   n 0 ?do i A64EFF:SEQ-WITH-SLOT loop ;

: HABU-CONV ( n n -- A64EFF:routine )
   {: in:n out:n :}
   in SLOTS-N  out SLOTS-N  A64EFF:GPR-NONE
   A64EFF:FPR-NONE A64EFF:FPR-NONE A64EFF:FPR-NONE
   A64EFF-NZCV:UNTOUCHED A64EFF-LINK:PRESERVED A64EFF-CONTROL:RETURNS
   A64EFF:TRAITS-NONE 0 0 A64EFF:ROUTINE ;

\ A contract that declares the routine CALLS, over a body that contains no call.
\ The declaration is what makes this pass reserve a frame and save the caller's
\ return address, so a routine that declared it and never called would carry two
\ instructions and a stack pointer nobody can account for. The two derivations of
\ one fact - the contract's trait and the calls this pass really built - have to
\ agree, and this is the direction test/compiler/native-chain.f cannot measure:
\ a refused case abandons a context holding two modules, and that suite can
\ afford exactly one of them.
: CALL-CONV ( n n -- A64EFF:routine )
   {: in:n out:n :}
   in SLOTS-N  out SLOTS-N  A64EFF:GPR-NONE
   A64EFF:FPR-NONE A64EFF:FPR-NONE A64EFF:FPR-NONE
   A64EFF-NZCV:UNTOUCHED A64EFF-LINK:PRESERVED A64EFF-CONTROL:RETURNS
   A64EFF:T-CALL A64EFF:SP-ALIGN 0 A64EFF:ROUTINE ;

\ A contract whose argument side names a data-stack slot AND a register. There is
\ no entry sequence for a convention that puts one argument on the stack and the
\ next in a register, so it is refused rather than half-built.
: MIXED-CONV ( -- A64EFF:routine )
   A64EFF:SEQ-NONE 0 A64EFF:SEQ-WITH-SLOT 0 A64EFF:SEQ-WITH
   A64EFF:SEQ-NONE A64EFF:GPR-NONE
   A64EFF:FPR-NONE A64EFF:FPR-NONE A64EFF:FPR-NONE
   A64EFF-NZCV:UNTOUCHED A64EFF-LINK:PRESERVED A64EFF-CONTROL:RETURNS
   A64EFF:TRAITS-NONE 0 0 A64EFF:ROUTINE ;

: SELECTED-HABU ( n n -- IR-BUILD:module )
   {: in:n out:n :}
   CC BB A64SEL:BIND-SOURCE
   CC BB IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   CC m A64-BUILDER TXT TXT-N  in out HABU-CONV  A64SEL:SELECT ;

: DSTACK-BODY ( IR-CTX:ctx -- n n bool n bool n bool bool n bool n n )
   HIR-MOD
   BUILD-SQUARE
   1 1 SELECTED-HABU READ!
   R-BLKR RV BLK0 IR-FUN:FARG-COUNT
   OPS
   0 s" a64.dtake" OPCODE-IS?
   0 0 ATTR-INT
   1 s" a64.dload" OPCODE-IS?
   1 0 ATTR-INT
   1 0 s" a64.dslot" ATTR-KEY-IS?
   3 s" a64.dstore" OPCODE-IS?
   3 0 ATTR-INT
   4 s" a64.dpublish" OPCODE-IS?
   4 0 ATTR-INT
   R-OPR RV 5 OP@ IR-OP:FOPERANDS ;

: DSTACK-CASE ( -- )
   s" a routine's data-stack convention is selected into its own operations" T-LABEL
   WBND [: DSTACK-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= 8 T= TTRUE 0 T= TTRUE TTRUE 0 T= TTRUE 8 T= TTRUE 6 T= 0 T= ;

\ ---- a value that only crosses the routine -----------------------------------
\ THE RESIDENCY ANSWER, at its smallest. A routine that hands its argument back
\ unchanged publishes it out of the very cell the caller wrote it into, so the
\ load that would lift it into a register and the store that would put it back
\ are both instructions with nothing to do. What is left is the two pointer
\ moves - which are not this pass's to remove, and say so under their own dot
\ habu-place-the-data-9f128e58 - and the return.
\
\ THE PAIR IS THE POINT. `swap` is the same routine with its two results in each
\ other's cells: nothing is where it will be published from, so every load and
\ every store is built. A pass that dropped stores by counting rather than by
\ asking what the cell holds would pass the first case and lose the second, which
\ is why they are asserted together.
: PASS-BODY ( IR-CTX:ctx -- n bool bool bool )
   HIR-MOD
   BUILD-PASS
   1 1 SELECTED-HABU READ!
   OPS
   0 s" a64.dtake" OPCODE-IS?
   1 s" a64.dpublish" OPCODE-IS?
   2 s" a64.ret" OPCODE-IS? ;

: PASS-CASE ( -- )
   s" a value that only crosses the routine never leaves its slot" T-LABEL
   WBND [: PASS-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE 3 T= ;

: EXCH-BODY ( IR-CTX:ctx -- n bool bool bool bool bool bool )
   HIR-MOD
   BUILD-EXCH
   2 2 SELECTED-HABU READ!
   OPS
   0 s" a64.dtake" OPCODE-IS?
   1 s" a64.dload" OPCODE-IS?
   2 s" a64.dload" OPCODE-IS?
   3 s" a64.dstore" OPCODE-IS?
   4 s" a64.dstore" OPCODE-IS?
   5 s" a64.dpublish" OPCODE-IS? ;

: EXCH-CASE ( -- )
   s" and a value published from another value's slot is still moved" T-LABEL
   WBND [: EXCH-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE 7 T= ;

\ ---- the memory operations, lowered ------------------------------------------
\ What the two addressed forms are wired to, read off the selected module. The
\ source order is minted by hir.mem, which selects to no instruction at all: the
\ token the machine store takes is the one a64.dtake minted and a64.dload passed
\ on, so the whole routine - entry, body and exit - is on one chain. The store's
\ two register operands are checked against the values they must be, and the
\ load's order operand against the store's answer, so a lowering that dropped a
\ link or swapped the value and the address is a different VALUE here.
\
\ The operation numbering: 0 dtake, 1 dload, 2 movz (the address), 3 astr,
\ 4 movz (the address again), 5 aldr, 6 movz 1, 7 add, 8 dstore, 9 dpublish,
\ 10 ret.
: MEM-BODY ( IR-CTX:ctx -- n bool bool bool bool bool bool bool bool )
   HIR-MOD
   BUILD-BUMP
   1 1 SELECTED-HABU READ!
   OPS
   3 s" a64.astr" OPCODE-IS?
   5 s" a64.aldr" OPCODE-IS?
   3 0 OPERAND@  1 0 RESULT@  SAME-VALUE?
   3 1 OPERAND@  2 0 RESULT@  SAME-VALUE?
   3 2 OPERAND@  1 1 RESULT@  SAME-VALUE?
   5 0 OPERAND@  4 0 RESULT@  SAME-VALUE?
   5 1 OPERAND@  3 0 RESULT@  SAME-VALUE?
   8 1 OPERAND@  5 1 RESULT@  SAME-VALUE? ;

: MEM-CASE ( -- )
   s" a store and a load select to the addressed forms on one memory order" T-LABEL
   WBND [: MEM-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE 11 T= ;

\ A memory operation in a routine whose convention names only registers. The
\ generic memory order of this dialect begins where the routine takes the
\ caller's operands, and a routine that takes none has no beginning for it, so
\ the lowering is refused by name rather than invented.
: MEM-REG-BODY ( IR-CTX:ctx -- )
   HIR-MOD
   BUILD-BUMP
   SELECTED drop ;

: MEM-REG ( -- )
   WBND [: MEM-REG-BODY ;] IR-CTX:WITH-CONTEXT ;

: MEM-REG-REFUSE-CASE ( -- )
   s" a memory operation in a register-convention routine is refused" T-LABEL
   [: MEM-REG ;] E-A64SEL-MEM TTHROWSQ ;

: MIXED-BODY ( IR-CTX:ctx -- )
   HIR-MOD
   BUILD-SQUARE
   CC BB A64SEL:BIND-SOURCE
   CC BB IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   CC m A64-BUILDER TXT TXT-N MIXED-CONV A64SEL:SELECT drop ;

: ARITY-BODY ( IR-CTX:ctx -- )
   HIR-MOD
   BUILD-SQUARE
   2 1 SELECTED-HABU drop ;

: MIXED ( -- )
   WBND [: MIXED-BODY ;] IR-CTX:WITH-CONTEXT ;

: DARITY ( -- )
   WBND [: ARITY-BODY ;] IR-CTX:WITH-CONTEXT ;

\ The same square, selected under a contract that says it calls.
: CALL-NONE-BODY ( IR-CTX:ctx -- )
   HIR-MOD
   BUILD-SQUARE
   CC BB A64SEL:BIND-SOURCE
   CC BB IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   CC m A64-BUILDER TXT TXT-N  1 1 CALL-CONV  A64SEL:SELECT drop ;

: CALL-NONE ( -- )
   WBND [: CALL-NONE-BODY ;] IR-CTX:WITH-CONTEXT ;

: CALL-NONE-REFUSE-CASE ( -- )
   s" a contract declaring a call over a body with none is refused" T-LABEL
   [: CALL-NONE ;] E-A64SEL-CALL TTHROWSQ ;

\ One refusal per group: each abandons a context holding TWO modules - the source
\ module the fixture built and the machine builder the selection opened - so two
\ of them in one group exhaust the live-arena registry and the second case
\ reports a refusal that really happened as a failure.
: MIXED-REFUSE-CASE ( -- )
   s" a convention mixing register places with data-stack places is refused" T-LABEL
   [: MIXED ;] E-A64SEL-PLACE TTHROWSQ ;

: DARITY-REFUSE-CASE ( -- )
   s" a convention naming more data-stack arguments than the word has is refused" T-LABEL
   [: DARITY ;] E-A64SEL-PLACE TTHROWSQ ;

\ ---- groups ------------------------------------------------------------------
: GROUP-BIND-REFUSE ( IR-CTX:ctx -- )
   drop
   BIND-REFUSE-CASES ;

: GROUP-SOURCE-REFUSE ( IR-CTX:ctx -- )
   drop
   SOURCE-REFUSE-CASES ;

: GROUP-SHAPE-REFUSE ( IR-CTX:ctx -- )
   drop
   SHAPE-REFUSE-CASES ;

: GROUP-OPCODE-REFUSE ( IR-CTX:ctx -- )
   drop
   OPCODE-REFUSE-CASES ;

: GROUP-TRAP-REFUSE ( IR-CTX:ctx -- )
   drop
   TRAP-REFUSE-CASES ;

: GROUP-MEM-REG-REFUSE ( IR-CTX:ctx -- )
   drop
   MEM-REG-REFUSE-CASE ;

: GROUP-MIXED-REFUSE ( IR-CTX:ctx -- )
   drop
   MIXED-REFUSE-CASE ;

: GROUP-DARITY-REFUSE ( IR-CTX:ctx -- )
   drop
   DARITY-REFUSE-CASE ;

: GROUP-CALL-NONE-REFUSE ( IR-CTX:ctx -- )
   drop
   CALL-NONE-REFUSE-CASE ;

public

: RUN ( -- )
   T-RESET
   SQUARE-CASE
   DIFF-CASE
   DIV-CASE
   ADD-CASE
   FUSE-CASE
   FUSE-LE-CASE
   FUSE-EQ-CASE
   NOFUSE-CASE
   SELECT-CASE
   SELECT-REST-CASE
   SELECT-VALUE-CASE
   SELECT-SAME-CASE
   SELECT-REAL-CASE
   SELECT-REAL-SWAPPED-CASE
   SELECT-REAL-REST-CASE
   SELECT-REAL-REFUSE-CASE
   FSEL-REAL-CASE
   FSEL-REAL-SWAPPED-CASE
   FSEL-CELL-CASE
   FSELZ-REAL-CASE
   FSELZ-CELL-CASE
   FSEL-REAL-POOL-CASE
   FSEL-CELL-REFUSE-CASE
   FLAG-VALUE-CASE
   FFUSE-CASE
   FFUSE-REST-CASE
   FNOFUSE-CASE
   FFLAG-VALUE-CASE
   SMALL-CASE
   WIDE-CASE
   FUN-CASE
   DSTACK-CASE
   PASS-CASE
   EXCH-CASE
   MEM-CASE
   WBND [: GROUP-BIND-REFUSE ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-SOURCE-REFUSE ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-SHAPE-REFUSE ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-OPCODE-REFUSE ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-TRAP-REFUSE ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-MEM-REG-REFUSE ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-MIXED-REFUSE ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-DARITY-REFUSE ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-CALL-NONE-REFUSE ;] IR-CTX:WITH-CONTEXT
   T-REPORT ;

;package

A64SEL-TEST:RUN

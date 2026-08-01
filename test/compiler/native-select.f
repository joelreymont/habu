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
\ `: MAX2 ( n n -- n ) 2dup < if swap then drop ;` as the elaborator leaves it:
\ four blocks - the entry that compares and branches, one stub per arm because a
\ two-way branch carries no values, and the join that takes the chosen value as
\ its argument. It is built here rather than elaborated for the reason every
\ other fixture in this file is, and it is built ONCE and used three ways: the
\ comparison it opens with is a parameter, and the value its two arms hand on is
\ a parameter, so the fused case, the three conditions and the multi-use
\ fall-back are the same shape with one thing changed.
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
   carry if f 3 BR1 else x 3 BR1 then
   BLOCK+
   carry if f 3 BR1 else y 3 BR1 then
   BLOCK+
   ARG+ RET1
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

\ An operation of a sixth opcode, defined into the same dialect's table. Nothing
\ in the substrate forbids it and the module verifies, so the pass has to refuse
\ it by name rather than by never meeting it.
: EXTRA-SCHEMA ( -- IR-ID:ir-symbol-id )
   CC BB s" hir.xor" IR-BUILD:INTERN-SYMBOL {: op:IR-ID:ir-symbol-id :}
   op IR-SCHEMA:BEGIN-OP
   CELLT IR-SCHEMA:ADD-OPERAND
   CELLT IR-SCHEMA:ADD-OPERAND
   CELLT IR-SCHEMA:ADD-RESULT
   false 0 0 IR-SCHEMA:SET-CONTROL
   IR-SCHEMA:SET-PURE
   false IR-SCHEMA:SET-TRAP
   CTARGET-ARCH:AARCH64 CTARGET:F-BASE IR-SCHEMA:SET-TARGET
   CC BB s" hir.rule.xor" IR-BUILD:INTERN-SYMBOL IR-SCHEMA:SET-RULE
   CC BB s" hir.render.xor" IR-BUILD:INTERN-SYMBOL IR-SCHEMA:SET-RENDERER
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

\ Bind the source dialect while the module is still live, freeze it, and select.
: SELECTED ( -- IR-BUILD:module )
   CC BB A64SEL:BIND-SOURCE
   CC BB IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   CC m A64-BUILDER TXT TXT-N NO-PLACES A64SEL:SELECT ;

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

: ATTR-INT ( n n -- n )
   {: i:n k:n :}
   R-ATTR RV  R-OPP RV R-OPR RV RK i OP@ k IR-OP:FATTR@  IR-ATTR:FINT@ ;

: ATTR-KEY-IS? ( n n ptr u8 n -- bool )
   {: i:n k:n p u:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   R-SYMP RV R-SYMR RV
   R-OPP RV R-OPR RV RK i OP@ k IR-OP:FATTR-KEY@
   p u IR-SYM:FEQ? ;

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
   SELECTED READ!
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
   SELECTED READ!
   0 s" a64.cmpbr" OPCODE-IS?
   0 0 ATTR-INT ;

: FUSE-LE-CASE ( -- )
   s" a fused less-or-equal branches on less-or-equal" T-LABEL
   WBND [: FUSE-LE-BODY ;] IR-CTX:WITH-CONTEXT
   A64IR-COND:LE A64IR:COND-CODE T= TTRUE ;

: FUSE-EQ-BODY ( IR-CTX:ctx -- bool n )
   HIR-MOD
   HIR-OPCODE:EQUAL false BUILD-BRANCH
   SELECTED READ!
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
   SELECTED READ!
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
   FLAG-VALUE-CASE
   SMALL-CASE
   WIDE-CASE
   FUN-CASE
   DSTACK-CASE
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

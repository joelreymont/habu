\ native-emit-shapes.f - the shared builders and helpers of the ARM64 emission
\ tests.
\
\ Holds the module fixture, the shape builders its two users share, built into
\ HIR or straight into the machine dialect by hand, the chain that emits them and
\ the readers of the emission. Its users reopen its package A64EMIT-TEST to reach
\ these private words: test/compiler/native-emit.f, the product suite, compares
\ the emitted words, and test/compiler/native-emit-run-child.f, which that
\ suite runs as a window child, publishes and calls them. The child runs the
\ same shapes, emitted by the unsealed image's own baked emitter, which the same
\ native build makes from the same sources.
\
\ WHERE THE CHAIN ITSELF IS DRIVEN. Binding the two dialects, selecting,
\ allocating, accepting and emitting are the same four stages in the same order
\ for every caller, so they live in test/compiler/native-chain-fixture.f and
\ this file drives them from there. What is this file's own is how each shape is
\ built into HIR by hand, which is what a suite about encodings has to state
\ itself.
\
\ WHY THE HOSTILE MODULE IS BUILT IN THE MACHINE DIALECT. An operation of a form
\ outside the dialect's family is a shape the selector never produces, so it is
\ built straight into A64IR - and it is emitted without an allocation, because the
\ allocator refuses it before the emitter would ever see it. That is the point:
\ the emitter must refuse it under its own name rather than by never meeting it.
\ A module of two functions is built the same way and for the opposite reason: it
\ is what a definition that makes a quotation compiles to, and the emission has to
\ hold both of them end to end.

require src/compiler/native/select.f
require src/compiler/native/emit.f
require src/compiler/native/spill.f
require test/compiler/native-chain-fixture.f

package A64EMIT-TEST
private

\ ---- bindings ----------------------------------------------------------------
\ The machine these instructions are for, from the shared chain fixture.
: WBND ( -- CBIND:binding )
   NFIX:BINDING ;

\ The same numeric policy on an AArch64 core whose byte order this backend does
\ not serve. Its architecture HAS a registered backend, so emission reaches this
\ backend's own refusal instead of the registry's.
: BEBND ( -- CBIND:binding )
   CTARGET-ARCH:AARCH64 CTARGET-ABI:AAPCS64-LINUX CTARGET-ENDIAN:BIG
   CTARGET-PTR--WIDTH:BITS64
   CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH CTARGET:CONTRACT
   CNUM-OVERFLOW:WRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY
   CBIND:BIND ;

\ The same numeric policy on a machine that executes none of these instructions.
: PBND ( -- CBIND:binding )
   CTARGET-ARCH:PTX CTARGET-ABI:PTX-KERNEL CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64
   CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH CTARGET:CONTRACT
   CNUM-OVERFLOW:WRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY
   CBIND:BIND ;

\ ---- the fixture's source text -----------------------------------------------
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

: SIGN ( n n -- IR-ID:ir-type-id )
   {: in:n out:n :}
   CELLT {: t:IR-ID:ir-type-id :}
   IR-TYPE:FN-BEGIN
   in 0 ?do t IR-TYPE:FN-PARAM loop
   out 0 ?do t IR-TYPE:FN-RESULT loop
   CC BB IR-BUILD:INTERN-CODE-REF ;

: OPEN-FUN ( ptr u8 n n n -- )
   {: p u:n in:n out:n :}
   CC BB  CC BB p u IR-BUILD:INTERN-SYMBOL  IR-BUILD:BEGIN-FUN
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

\ ---- source modules ----------------------------------------------------------
: HIR-MOD ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   IR-BUILD:PLAN-BEGIN
   IR-BUILD:PLAN-DEFAULT
   c HIR:NEW-BUILDER {: b:IR-BUILD:builder :}
   c b HIR:REGISTER
   c 0 W-CTX !
   b 0 W-BLD !
   c b TXT TXT-N IR-BUILD:ADD-SOURCE 0 W-SRC ! ;

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
   CC BB  CC BB HIR:KEY-ADDR  CC BB HIR:ADDR-NONE HIR:ADDR-ATTR
   IR-BUILD:ADD-ATTR
   CLOSE-VALUE ;

: RET1 ( IR-ID:ir-value-id -- )
   {: v:IR-ID:ir-value-id :}
   HIR-OPCODE:RETURN CLOSE-ST CLOSE-LN OPEN-OP
   CC BB v IR-BUILD:ADD-OPERAND
   CC BB IR-BUILD:END-OP drop ;

\ `: SQUARE ( n -- n ) dup * ;`
: BUILD-SQUARE ( -- )
   s" SQUARE" 1 1 OPEN-FUN
   ARG+ {: a:IR-ID:ir-value-id :}
   HIR-OPCODE:MUL a a BINOP RET1
   CLOSE-FUN ;

\ `: DIFF ( n n -- n ) - ;`
: BUILD-DIFF ( -- )
   s" DIFF" 2 1 OPEN-FUN
   ARG+ {: x:IR-ID:ir-value-id :}
   ARG+ {: y:IR-ID:ir-value-id :}
   HIR-OPCODE:SUB x y BINOP RET1
   CLOSE-FUN ;

\ `: QUOT ( n n -- n ) / ;`
: BUILD-DIV ( -- )
   s" QUOT" 2 1 OPEN-FUN
   ARG+ {: x:IR-ID:ir-value-id :}
   ARG+ {: y:IR-ID:ir-value-id :}
   HIR-OPCODE:DIV x y BINOP RET1
   CLOSE-FUN ;

\ `: SUM3 ( a b c -- n ) + + ;`
: BUILD-SUM3 ( -- )
   s" SUM3" 3 1 OPEN-FUN
   ARG+ {: x:IR-ID:ir-value-id :}
   ARG+ {: y:IR-ID:ir-value-id :}
   ARG+ {: z:IR-ID:ir-value-id :}
   HIR-OPCODE:ADD x y BINOP {: t:IR-ID:ir-value-id :}
   HIR-OPCODE:ADD t z BINOP RET1
   CLOSE-FUN ;

\ `: REUSE ( a b -- n ) over + + ;`: the first argument is read again after the
\ first sum, so the first sum lands in a register that is neither of its own
\ operands' - the one shape here where an instruction's destination field and its
\ first source field differ.
: BUILD-REUSE ( -- )
   s" REUSE" 2 1 OPEN-FUN
   ARG+ {: x:IR-ID:ir-value-id :}
   ARG+ {: y:IR-ID:ir-value-id :}
   HIR-OPCODE:ADD x y BINOP {: t:IR-ID:ir-value-id :}
   HIR-OPCODE:ADD x t BINOP RET1
   CLOSE-FUN ;

\ ---- a body that reads and writes memory -------------------------------------
\ `: BUMP ( n -- n ) A ! A @ 1+ dup A ! ;` with A a fixed address, built by hand
\ so the two addressed instructions can be read back as the exact words they are.
\ The address is a small even number and this shape is never EXECUTED here: what
\ is being proved is which register field each operand lands in, and running it
\ would only prove that the number is not a real cell. The chain suite runs the
\ same body against a cell the engine really created.
$1000 constant BUMP-ADDR

: MEMT ( -- IR-ID:ir-type-id )
   CC BB HIR:MEM-TYPE ;

\ The memory the definition is entered with: no operand, one order.
: MEM0 ( -- IR-ID:ir-value-id )
   HIR-OPCODE:MEM BODY-ST BODY-LN OPEN-OP
   CC BB MEMT IR-BUILD:ADD-RESULT
   CLOSE-VALUE ;

\ One store: the value, the address, the order in - and the order out.
: STORE1 ( IR-ID:ir-value-id IR-ID:ir-value-id IR-ID:ir-value-id -- IR-ID:ir-value-id )
   {: v:IR-ID:ir-value-id a:IR-ID:ir-value-id k:IR-ID:ir-value-id :}
   HIR-OPCODE:STORE BODY-ST BODY-LN OPEN-OP
   CC BB v IR-BUILD:ADD-OPERAND
   CC BB a IR-BUILD:ADD-OPERAND
   CC BB k IR-BUILD:ADD-OPERAND
   CC BB MEMT IR-BUILD:ADD-RESULT
   CLOSE-VALUE ;

\ One load: the address and the order in, the loaded cell and the order out. The
\ order is the second result, so the loaded value is read the way every other
\ value-producing operation's is.
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
   s" SQUARE" 1 1 OPEN-FUN
   ARG+ {: x:IR-ID:ir-value-id :}
   MEM0 {: k0:IR-ID:ir-value-id :}
   BUMP-ADDR CONSTOP {: a0:IR-ID:ir-value-id :}
   x a0 k0 STORE1 {: k1:IR-ID:ir-value-id :}
   BUMP-ADDR CONSTOP {: a1:IR-ID:ir-value-id :}
   a1 k1 LOAD1 {: got:IR-ID:ir-value-id k2:IR-ID:ir-value-id :}
   1 CONSTOP {: one:IR-ID:ir-value-id :}
   HIR-OPCODE:ADD got one BINOP {: up:IR-ID:ir-value-id :}
   BUMP-ADDR CONSTOP {: a2:IR-ID:ir-value-id :}
   up a2 k2 STORE1 drop
   up RET1
   CLOSE-FUN ;

\ A literal across two halves: a move-wide, then an overwrite that keeps it.
: BUILD-WIDE ( -- )
   s" WIDE" 0 1 OPEN-FUN
   $1234000000005678 CONSTOP RET1
   CLOSE-FUN ;

\ ---- running the whole chain -------------------------------------------------
\ Select, allocate, accept and emit for a leaf routine of `n` registers. Every
\ positive case goes through the whole chain, so nothing here emits from a claim
\ the validator has not agreed with.
: EMITTED ( n -- )
   {: n:n :}
   CC BB n NFIX:RUN ;

\ The same, out of a pool that starts at `base`.
: EMITTED-FROM ( n n -- )
   {: base:n n:n :}
   CC BB base n NFIX:RUN-FROM ;

\ The same under the convention a Habu word is entered and left through. A body
\ that touches memory needs it: the generic memory order of a routine begins
\ where the routine takes the caller's operands, so a routine that takes none is
\ refused at selection by name.
: EMITTED-HABU ( n n n -- )
   {: n:n in:n out:n :}
   CC BB 0 n in out NFIX:RUN-HABU ;

\ ---- reading the emission ----------------------------------------------------
: BYTE-AT ( n -- n )
   A64EMIT:BYTES swap + c@ ;

: SPAN-START-AT ( n -- n )
   A64EMIT:MAP-SPAN@ IR-SOURCE:SPAN-START ;

: SPAN-LEN-AT ( n -- n )
   A64EMIT:MAP-SPAN@ IR-SOURCE:SPAN-LEN ;

\ The division's refusal branches to an ADDRESS and not to a constant, so the
\ expected word cannot be written out: the displacement is decoded back into the
\ address it names and held against the entry the dictionary gives for its callee.
\ A word that is not a `bl` is refused as that rather than read as a distance.
: BL-TARGET ( n -- n )
   {: i:n :}
   i A64EMIT:WORD@ {: w:n :}
   w $FC000000 and $94000000 <> if s" native-emit: not a bl" 76 die then
   w $03FFFFFF and {: imm:n :}
   imm $02000000 and 0<> if imm $04000000 - else imm then
   4 *  A64EMIT:PLACEMENT +  i 4 * + ;

: DIV-ZERO-ENTRY ( -- n )
   s" (DIV-ZERO)" NDICT:HELPER-TARGET ;

: SPAN-SRC-AT ( n -- n )
   A64EMIT:MAP-SPAN@ IR-SOURCE:SPAN-SRC IR-ID:SOURCE-LOCAL ;

\ The register the returned value ended up in. The last value the module defines
\ is the one the return carries in every shape below.
: RESULT-REG ( -- n )
   NFIX:RESULT-REG ;

\ ---- machine modules built by hand -------------------------------------------
\ The shapes the selector never produces. Everything below builds straight into
\ the machine dialect. The bindings are taken separately from the building,
\ because several of these cases need a module with one binding, or none.
: A64-NEW ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   IR-BUILD:PLAN-BEGIN
   IR-BUILD:PLAN-DEFAULT
   c A64IR:NEW-BUILDER {: b:IR-BUILD:builder :}
   c 0 W-CTX !
   b 0 W-BLD !
   c b A64IR:REGISTER
   c b TXT TXT-N IR-BUILD:ADD-SOURCE 0 W-SRC ! ;

: BIND-EMIT ( -- )
   CC BB A64EMIT:BIND-DIALECT ;

: BIND-RA ( -- )
   CC BB A64IR:MACHINE  CC BB A64IR:VOCABULARY  A64RA:BIND-DIALECT ;

: BIND-RAV ( -- )
   CC BB  CC BB A64IR:VOCABULARY  A64RAV:BIND-DIALECT ;

: M-OPEN ( A64IR:opcode -- )
   {: o:A64IR:opcode :}
   CC BB  CC BB o A64IR:OPCODE  IR-BUILD:BEGIN-OP
   CC BB  BODY-ST BODY-LN SPN  IR-BUILD:SET-OP-SPAN ;

: M-RESULT+ ( -- )
   CC BB  CC BB A64IR:GPR-TYPE  IR-BUILD:ADD-RESULT ;

: M-MOVZ ( n -- IR-ID:ir-value-id )
   {: imm:n :}
   A64IR-OPCODE:MOVZ M-OPEN
   M-RESULT+
   CC BB  CC BB A64IR:KEY-IMM    CC BB imm A64IR:IMM-ATTR   IR-BUILD:ADD-ATTR
   CC BB  CC BB A64IR:KEY-SHIFT  CC BB 0 A64IR:SHIFT-ATTR   IR-BUILD:ADD-ATTR
   CC BB  CC BB A64IR:KEY-ADDR   CC BB A64IR:ADDR-NONE A64IR:ADDR-ATTR IR-BUILD:ADD-ATTR
   CLOSE-VALUE ;

\ The same three move-wide forms with the relocation kind chosen by the caller,
\ so a case can build a chain the producers in this tree never build. The three
\ refusal builders in test/compiler/native-emit.f, BUILD-MOVN-ADDR,
\ BUILD-SHORT-RUN and BUILD-SPLIT-RUN, use them to build shapes the emitter
\ refuses, each a shape a rewrite between selection and emission could
\ plausibly produce.
: M-WIDE ( A64IR:opcode n n n -- IR-ID:ir-value-id )
   {: o:A64IR:opcode imm:n sh:n kind:n :}
   o M-OPEN
   M-RESULT+
   CC BB  CC BB A64IR:KEY-IMM    CC BB imm A64IR:IMM-ATTR   IR-BUILD:ADD-ATTR
   CC BB  CC BB A64IR:KEY-SHIFT  CC BB sh A64IR:SHIFT-ATTR  IR-BUILD:ADD-ATTR
   CC BB  CC BB A64IR:KEY-ADDR   CC BB kind A64IR:ADDR-ATTR IR-BUILD:ADD-ATTR
   CLOSE-VALUE ;

\ A movk keeps the halves already in place, so it takes the running value as an
\ operand - which is what chains the four lanes into one register.
: M-WIDE-K ( IR-ID:ir-value-id n n n -- IR-ID:ir-value-id )
   {: v:IR-ID:ir-value-id imm:n sh:n kind:n :}
   A64IR-OPCODE:MOVK M-OPEN
   CC BB v IR-BUILD:ADD-OPERAND
   M-RESULT+
   CC BB  CC BB A64IR:KEY-IMM    CC BB imm A64IR:IMM-ATTR   IR-BUILD:ADD-ATTR
   CC BB  CC BB A64IR:KEY-SHIFT  CC BB sh A64IR:SHIFT-ATTR  IR-BUILD:ADD-ATTR
   CC BB  CC BB A64IR:KEY-ADDR   CC BB kind A64IR:ADDR-ATTR IR-BUILD:ADD-ATTR
   CLOSE-VALUE ;

: M-RET ( IR-ID:ir-value-id -- )
   {: v:IR-ID:ir-value-id :}
   A64IR-OPCODE:RET M-OPEN
   CC BB v IR-BUILD:ADD-OPERAND
   CC BB IR-BUILD:END-OP drop ;

: M-ADD ( IR-ID:ir-value-id IR-ID:ir-value-id -- IR-ID:ir-value-id )
   {: x:IR-ID:ir-value-id y:IR-ID:ir-value-id :}
   A64IR-OPCODE:ADD M-OPEN
   CC BB x IR-BUILD:ADD-OPERAND
   CC BB y IR-BUILD:ADD-OPERAND
   M-RESULT+
   CLOSE-VALUE ;

: M-FREEZE ( -- IR-BUILD:module )
   CC BB IR-BUILD:FREEZE ;

: BIND-SPILL ( -- )
   CC BB  CC BB A64IR:LOWERING  [: A64IR:ENSURE-NAMED ;] A64SPILL:BIND-DIALECT ;

\ A CONSTANT NO RE-EMISSION CAN STAND FOR. A class whose one value was written
\ by a move-wide is written AGAIN where it is read rather than put away
\ (src/compiler/native/regalloc.f MB-REMATABLE?), so a body meant to reach the
\ FRAME cannot hold its pressure in plain literals. Each of these is a literal
\ added to itself: its defining operation reads a register, which is what
\ excludes it structurally, and the seed is live for exactly one position so the
\ peak is the same as five plain literals.
: M-CONST ( n -- IR-ID:ir-value-id )
   M-MOVZ {: z:IR-ID:ir-value-id :}
   z z M-ADD ;

\ Five values made before any of them is read, so five are live at once and
\ three registers cannot hold them. This is the shape the whole spill route
\ exists for, and the only way to know the route is right is to run the bytes it
\ produces. BUILD-REMAT-CHAIN below is the same shape in plain move-wides, which
\ takes the OTHER route and no frame at all.
: BUILD-CHAIN ( -- )
   s" CHAIN" 0 1 OPEN-FUN
   $11 M-CONST {: a:IR-ID:ir-value-id :}
   $22 M-CONST {: b:IR-ID:ir-value-id :}
   $33 M-CONST {: c:IR-ID:ir-value-id :}
   $44 M-CONST {: d:IR-ID:ir-value-id :}
   $55 M-CONST {: e:IR-ID:ir-value-id :}
   a b M-ADD {: s1:IR-ID:ir-value-id :}
   s1 c M-ADD {: s2:IR-ID:ir-value-id :}
   s2 d M-ADD {: s3:IR-ID:ir-value-id :}
   s3 e M-ADD M-RET
   CLOSE-FUN ;

\ The same five values as plain move-wides. Every one of them is a class the walk
\ can write again where it is read, so this body takes NO frame: what the bytes
\ show is a routine with no reserve at all and a move-wide standing in front of
\ each addition that reads one of the two the registers could not hold.
: BUILD-REMAT-CHAIN ( -- )
   s" RCHAIN" 0 1 OPEN-FUN
   $11 M-MOVZ {: a:IR-ID:ir-value-id :}
   $22 M-MOVZ {: b:IR-ID:ir-value-id :}
   $33 M-MOVZ {: c:IR-ID:ir-value-id :}
   $44 M-MOVZ {: d:IR-ID:ir-value-id :}
   $55 M-MOVZ {: e:IR-ID:ir-value-id :}
   a b M-ADD {: s1:IR-ID:ir-value-id :}
   s1 c M-ADD {: s2:IR-ID:ir-value-id :}
   s2 d M-ADD {: s3:IR-ID:ir-value-id :}
   s3 e M-ADD M-RET
   CLOSE-FUN ;

\ A plain one-function machine module the state and identity cases can use.
: BUILD-PLAIN ( -- )
   s" PLAIN" 0 1 OPEN-FUN
   7 M-MOVZ M-RET
   CLOSE-FUN ;

\ TWO FUNCTIONS IN ONE MODULE, which is what a definition that makes a quotation
\ compiles to: the first is the routine the definition names and the second is the
\ body of its quotation. They carry different literals so the emission can be read
\ back and each function's instructions told from the other's - two functions
\ emitting the same bytes would leave an emitter that wrote the first one twice
\ indistinguishable from one that wrote both.
: BUILD-TWO-FUNS ( -- )
   s" ONE" 0 1 OPEN-FUN
   7 M-MOVZ M-RET
   CLOSE-FUN
   s" TWO" 0 1 OPEN-FUN
   9 M-MOVZ M-RET
   CLOSE-FUN ;

\ A seventh machine operation, defined into this dialect's own table. Nothing in
\ the substrate forbids it and the module verifies, so the emitter has to refuse
\ it by name rather than by never meeting it: an unmodelled form has no encoding
\ here and there is nothing safe to emit in its place.
: EXTRA-SCHEMA ( -- IR-ID:ir-symbol-id )
   CC BB s" a64.neg" IR-BUILD:INTERN-SYMBOL {: op:IR-ID:ir-symbol-id :}
   op IR-SCHEMA:BEGIN-OP
   CC BB A64IR:GPR-TYPE IR-SCHEMA:ADD-OPERAND
   CC BB A64IR:GPR-TYPE IR-SCHEMA:ADD-RESULT
   false 0 0 IR-SCHEMA:SET-CONTROL
   IR-SCHEMA:SET-PURE
   false IR-SCHEMA:SET-TRAP
   CTARGET-ARCH:AARCH64 CTARGET:F-BASE IR-SCHEMA:SET-TARGET
   CC BB s" a64.rule.neg" IR-BUILD:INTERN-SYMBOL IR-SCHEMA:SET-RULE
   CC BB s" a64.render.neg" IR-BUILD:INTERN-SYMBOL IR-SCHEMA:SET-RENDERER
   CC BB IR-BUILD:DEFINE-OP
   op ;

: BUILD-EXTRA ( -- )
   EXTRA-SCHEMA {: op:IR-ID:ir-symbol-id :}
   s" NEG" 0 1 OPEN-FUN
   7 M-MOVZ {: v:IR-ID:ir-value-id :}
   CC BB op IR-BUILD:BEGIN-OP
   CC BB  BODY-ST BODY-LN SPN  IR-BUILD:SET-OP-SPAN
   CC BB v IR-BUILD:ADD-OPERAND
   M-RESULT+
   CLOSE-VALUE M-RET
   CLOSE-FUN ;

\ One hand-built module taken through the whole chain, so the cases that need a
\ sealed emission cost one module rather than two.
: PLAIN-EMITTED ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c A64-NEW
   BIND-RA
   BIND-RAV
   BIND-EMIT
   BUILD-PLAIN
   M-FREEZE {: m:IR-BUILD:module :}
   c m 0 4 NFIX:FINISH ;

\ ---- a program that does not fit ---------------------------------------------
\ The whole spill route, ending in bytes that run: allocate the chain, lower the
\ spill decisions into a module whose stores and loads are operations, allocate
\ that, accept it, and emit. What the executed answer proves is what no table of
\ expected words can - that the value put into a frame slot is the value that
\ comes back out of it, that the frame the routine takes is the frame it gives
\ back, and that the stack pointer the loads and stores are relative to is where
\ the reserve left it.
: SPILL-EMITTED ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c A64-NEW
   BIND-RA
   BIND-SPILL
   BUILD-CHAIN
   M-FREEZE {: m0:IR-BUILD:module :}
   c m0 3 16 NFIX:LEAF-FRAMED A64RA:ALLOCATE
   IR-BUILD:PLAN-BEGIN
   IR-BUILD:PLAN-DEFAULT
   c A64IR:NEW-BUILDER {: nb:IR-BUILD:builder :}
   c nb A64IR:MACHINE  c nb A64IR:VOCABULARY  A64RA:BIND-DIALECT
   c nb  c nb A64IR:VOCABULARY  A64RAV:BIND-DIALECT
   c nb A64EMIT:BIND-DIALECT
   c m0 nb  c nb A64IR:LOWERING  A64SPILL:REWRITE {: m1:IR-BUILD:module :}
   c m1 3 16 NFIX:LEAF-FRAMED A64RA:ALLOCATE
   m1 3 16 NFIX:LEAF-FRAMED A64RAV:ACCEPT
   c m1 A64EMIT:EMIT
   A64SPILL:RELEASE ;

: HAS-WORD? ( n -- bool )
   {: word:n :}
   A64EMIT:INSNS 0 ?do
      i A64EMIT:WORD@ word = if true unloop exit then
   loop
   false ;

\ ---- the same program, written again instead of put away ---------------------
\ THE FIVE VALUES AS PLAIN MOVE-WIDES. Each is then a class the walk can write
\ AGAIN in front of the addition that reads it, so the two it cannot hold cost
\ two move-wides and no frame: the routine reserves nothing, gives nothing back,
\ and its contract declares a frame of zero. What the bytes show is the whole of
\ that - the first instruction is a move-wide and not a stack adjustment - and
\ what the run shows is that a re-emitted constant is the constant it stood for,
\ which no table of expected words can say.
: REMAT-EMITTED ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c A64-NEW
   BIND-RA
   BIND-SPILL
   BUILD-REMAT-CHAIN
   M-FREEZE {: m0:IR-BUILD:module :}
   c m0 3 0 NFIX:LEAF-FRAMED A64RA:ALLOCATE
   IR-BUILD:PLAN-BEGIN
   IR-BUILD:PLAN-DEFAULT
   c A64IR:NEW-BUILDER {: nb:IR-BUILD:builder :}
   c nb A64IR:MACHINE  c nb A64IR:VOCABULARY  A64RA:BIND-DIALECT
   c nb  c nb A64IR:VOCABULARY  A64RAV:BIND-DIALECT
   c nb A64EMIT:BIND-DIALECT
   c m0 nb  c nb A64IR:LOWERING  A64SPILL:REWRITE {: m1:IR-BUILD:module :}
   c m1 3 0 NFIX:LEAF-FRAMED A64RA:ALLOCATE
   m1 3 0 NFIX:LEAF-FRAMED A64RAV:ACCEPT
   c m1 A64EMIT:EMIT
   A64SPILL:RELEASE ;

\ ---- a returned value put where the contract says it leaves ------------------
\ `SECOND ( a b -- b )` under the C ABI: the arguments arrive in x0 and x1 and
\ the returned value leaves in x0, so the value the return carries is in the
\ register its caller chose and has to be in a different one where control
\ leaves. The allocator plans a copy, the lowering makes it an operation, and
\ this is what proves the copy is a real instruction: the emitted word is the
\ ARM64 spelling of a move, and calling the routine gives back the SECOND
\ argument - which it cannot do if the copy was dropped, encoded backwards, or
\ landed in another register.
: BUILD-SECOND ( -- )
   s" SECOND" 2 1 OPEN-FUN
   ARG+ drop
   ARG+ {: b:IR-ID:ir-value-id :}
   b M-RET
   CLOSE-FUN ;

: SECOND-ABI ( -- NEFF:routine )
   0 4 2 1 NFIX:LEAF-ABI ;

: SECOND-EMITTED ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c A64-NEW
   BIND-RA
   BIND-SPILL
   BUILD-SECOND
   M-FREEZE {: m0:IR-BUILD:module :}
   c m0 SECOND-ABI A64RA:ALLOCATE
   IR-BUILD:PLAN-BEGIN
   IR-BUILD:PLAN-DEFAULT
   c A64IR:NEW-BUILDER {: nb:IR-BUILD:builder :}
   c nb A64IR:MACHINE  c nb A64IR:VOCABULARY  A64RA:BIND-DIALECT
   c nb  c nb A64IR:VOCABULARY  A64RAV:BIND-DIALECT
   c nb A64EMIT:BIND-DIALECT
   c m0 nb  c nb A64IR:LOWERING  A64SPILL:REWRITE {: m1:IR-BUILD:module :}
   c m1 SECOND-ABI A64RA:ALLOCATE
   m1 SECOND-ABI A64RAV:ACCEPT
   c m1 A64EMIT:EMIT
   A64SPILL:RELEASE ;

;package

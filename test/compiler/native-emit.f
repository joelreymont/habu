\ native-emit.f - checked ARM64 instruction-emission tests.
\
\ Proves the contract of src/compiler/native/emit.f: an accepted straight-line
\ A64IR module becomes exactly the ARM64 instructions its operations are, placed
\ little-endian in a buffer, with one source-map row per instruction tying its
\ byte offset back to the span of the operation it came from; and a module this
\ leaf cannot emit, a register assignment nobody accepted, one accepted for
\ another module, one a later allocation has replaced, a machine these
\ instructions are not for, and a reader asking about an emission that never
\ happened are each refused by name.
\
\ WHY THE BYTES ARE EXECUTED AND NOT ONLY COMPARED. A table of expected words is
\ necessary and not sufficient: it can only disagree with an emitter that changed,
\ never with one that was always wrong, because the expected words and the
\ emitter can be wrong in the same way. Five of the shapes below are therefore
\ published into the engine's own code space and CALLED as leaf routines, through
\ test/compiler/native-run-fixture.f, with the arguments the source-level
\ arithmetic takes and its answer compared. That file's header says why the byte
\ offsets come from the source map and why the result register is asserted before
\ every call; each executing case below makes that assertion.
\
\ WHERE THE CHAIN ITSELF IS DRIVEN. Binding the two dialects, selecting,
\ allocating, accepting and emitting are the same four stages in the same order
\ for every caller, so they live in test/compiler/native-chain-fixture.f and this
\ suite drives them from there. What is this file's own is how each shape is
\ built into HIR by hand, which is what a suite about encodings has to state
\ itself.
\
\ WHY ONE OF THE BYTE CASES ALLOCATES OUT OF A HIGH POOL. The low registers are
\ where an emitter that ignored the allocation entirely would put things anyway.
\ The three-argument shape is therefore emitted twice, once from a pool that
\ starts at register zero and once from a pool that starts at register four, and
\ the second one's expected words are different in every register field.
\
\ WHY THE HOSTILE MODULE IS BUILT IN THE MACHINE DIALECT. An operation of a form
\ outside the dialect's family is a shape the selector never produces, so it is
\ built straight into A64IR - and it is emitted without an allocation, because the
\ allocator refuses it before the emitter would ever see it. That is the point:
\ the emitter must refuse it under its own name rather than by never meeting it.
\ A module of two functions is built the same way and for the opposite reason: it
\ is what a definition that makes a quotation compiles to, and the emission has to
\ hold both of them end to end.
\
\ ONE FIXTURE PER CONTEXT. A module holds about seventeen arenas and the live
\ arena registry holds sixty-four, so a case that builds a source module and a
\ machine module is already close to full and a case that builds two machine
\ modules is too. Every case therefore runs in its own context, and a refusing
\ case runs inside an enclosing one because an abandoned context gives its
\ registry slots back only when a live enclosing context leaves normally.

require lib/test.f
require src/compiler/native/select.f
require src/compiler/native/emit.f
require src/compiler/native/spill.f
require test/compiler/native-chain-fixture.f
require test/compiler/native-run-fixture.f

package A64EMIT-TEST
private

\ ---- bindings ----------------------------------------------------------------
\ The machine these instructions are for, from the shared chain fixture.
: WBND ( -- CBIND:binding )
   NFIX:BINDING ;

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
   {: p u:n in:n out:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
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
   CC BB TXT TXT-N n NFIX:RUN ;

\ The same, out of a pool that starts at `base`.
: EMITTED-FROM ( n n -- )
   {: base:n n:n :}
   CC BB TXT TXT-N base n NFIX:RUN-FROM ;

\ The same under the convention a Habu word is entered and left through. A body
\ that touches memory needs it: the generic memory order of a routine begins
\ where the routine takes the caller's operands, so a routine that takes none is
\ refused at selection by name.
: EMITTED-HABU ( n n n -- )
   {: n:n in:n out:n :}
   CC BB TXT TXT-N 0 n in out NFIX:RUN-HABU ;

\ ---- reading the emission ----------------------------------------------------
: BYTE-AT ( n -- n )
   A64EMIT:BYTES swap + c@ ;

: SPAN-START-AT ( n -- n )
   A64EMIT:MAP-SPAN@ IR-SOURCE:SPAN-START ;

: SPAN-LEN-AT ( n -- n )
   A64EMIT:MAP-SPAN@ IR-SOURCE:SPAN-LEN ;

: SPAN-SRC-AT ( n -- n )
   A64EMIT:MAP-SPAN@ IR-SOURCE:SPAN-SRC IR-ID:SOURCE-LOCAL ;

\ The register the returned value ended up in. The last value the module defines
\ is the one the return carries in every shape below.
: RESULT-REG ( -- n )
   NFIX:RESULT-REG ;

\ ---- publishing and calling the emitted bytes --------------------------------
\ The store into code space and the C-ABI call are the two engine boundaries, and
\ they live in test/compiler/native-run-fixture.f so the comparison harness runs
\ the emitted bytes exactly the way this suite does.
: PUBLISH ( -- n )       NRUN:PUBLISH ;
: EXEC0 ( n -- n )       NRUN:EXEC0 ;
: EXEC1 ( n n -- n )     NRUN:EXEC1 ;
: EXEC2 ( n n n -- n )   NRUN:EXEC2 ;
: EXEC3 ( n n n n -- n ) NRUN:EXEC3 ;

\ ---- the emitted bytes -------------------------------------------------------
\ `mul x0, x0, x0` then `ret`. The bytes are written out here rather than
\ recomputed from the encoders, so the expected value is independent of the
\ emitter and of the assembler both.
: SQUARE-BODY ( IR-CTX:ctx -- n n n n )
   HIR-MOD
   BUILD-SQUARE
   4 EMITTED
   A64EMIT:INSNS
   A64EMIT:SIZE
   0 A64EMIT:WORD@
   1 A64EMIT:WORD@ ;

: SQUARE-CASE ( -- )
   s" a multiply and a return emit as the two instructions they are" T-LABEL
   WBND [: SQUARE-BODY ;] IR-CTX:WITH-CONTEXT
   $D65F03C0 T= $9B007C00 T= 8 T= 2 T= ;

\ The same two instructions read one byte at a time: the buffer really holds the
\ little-endian placement the machine reads, and the map's offsets index it.
: BYTES-BODY ( IR-CTX:ctx -- n n n n n n )
   HIR-MOD
   BUILD-SQUARE
   4 EMITTED
   0 BYTE-AT
   1 BYTE-AT
   2 BYTE-AT
   3 BYTE-AT
   0 A64EMIT:MAP-OFFSET@
   1 A64EMIT:MAP-OFFSET@ ;

: BYTES-CASE ( -- )
   s" the instruction words are placed little-endian at the mapped offsets" T-LABEL
   WBND [: BYTES-BODY ;] IR-CTX:WITH-CONTEXT
   4 T= 0 T= $9B T= 0 T= $7C T= 0 T= ;

\ `sub x0, x0, x1` then `ret`.
: DIFF-BODY ( IR-CTX:ctx -- n n n n )
   HIR-MOD
   BUILD-DIFF
   4 EMITTED
   A64EMIT:INSNS
   0 A64EMIT:WORD@
   1 A64EMIT:WORD@
   RESULT-REG ;

: DIFF-CASE ( -- )
   s" a subtraction emits with its operands in the order the source has them" T-LABEL
   WBND [: DIFF-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= $D65F03C0 T= $CB010000 T= 2 T= ;

\ `cbnz x1, +2`, `brk`, `sdiv x0, x0, x1`, `ret`. The division is ONE operation
\ of the machine dialect and three instructions, and the two in front of the
\ divide are the whole of what makes a compiled division agree with an
\ interpreted one: ARM64's Sdiv answers zero for a zero divisor, and the
\ engine's own `/` ends the process instead (src/habu/habu1.f BDIV0?). Deleting
\ either of them, or moving the guard's distance off two, reddens here.
: DIV-BODY ( IR-CTX:ctx -- n n n n n )
   HIR-MOD
   BUILD-DIV
   4 EMITTED
   A64EMIT:INSNS
   0 A64EMIT:WORD@
   1 A64EMIT:WORD@
   2 A64EMIT:WORD@
   3 A64EMIT:WORD@ ;

: DIV-CASE ( -- )
   s" a division emits the zero-divisor guard the engine's own divide has" T-LABEL
   WBND [: DIV-BODY ;] IR-CTX:WITH-CONTEXT
   $D65F03C0 T= $9AC10C00 T= $D4200000 T= $B5000041 T= 4 T= ;

\ And it computes what the engine computes, truncating toward zero rather than
\ flooring: -7 over 2 is -3 and not -4. The two negative cases are what say the
\ rounding of a compiled division is the rounding of an interpreted one.
: RUN-DIV-BODY ( IR-CTX:ctx -- n n n )
   HIR-MOD
   BUILD-DIV
   4 EMITTED
   PUBLISH {: fn:n :}
   7 2 fn EXEC2
   -7 2 fn EXEC2
   7 -2 fn EXEC2 ;

: RUN-DIV-CASE ( -- )
   s" the emitted division truncates toward zero, as the engine's does" T-LABEL
   WBND [: RUN-DIV-BODY ;] IR-CTX:WITH-CONTEXT
   -3 T= -3 T= 3 T= ;

\ `add x0, x0, x1`, `add x0, x0, x2`, `ret`.
: SUM3-BODY ( IR-CTX:ctx -- n n n n )
   HIR-MOD
   BUILD-SUM3
   4 EMITTED
   A64EMIT:INSNS
   0 A64EMIT:WORD@
   1 A64EMIT:WORD@
   2 A64EMIT:WORD@ ;

: SUM3-CASE ( -- )
   s" two additions emit with the registers the allocation gave them" T-LABEL
   WBND [: SUM3-BODY ;] IR-CTX:WITH-CONTEXT
   $D65F03C0 T= $8B020000 T= $8B010000 T= 3 T= ;

\ `add x1, x0, x1`, `add x0, x0, x1`, `ret`. The first addition writes a register
\ that is not the one it reads first, so an emitter that took the destination off
\ the first operand - or the first operand off the destination - is wrong here and
\ nowhere else in this file.
: REUSE-BODY ( IR-CTX:ctx -- n n n n )
   HIR-MOD
   BUILD-REUSE
   4 EMITTED
   A64EMIT:INSNS
   0 A64EMIT:WORD@
   1 A64EMIT:WORD@
   2 A64EMIT:WORD@ ;

: REUSE-CASE ( -- )
   s" an addition whose result outlives neither operand keeps all three fields apart" T-LABEL
   WBND [: REUSE-BODY ;] IR-CTX:WITH-CONTEXT
   $D65F03C0 T= $8B010000 T= $8B010001 T= 3 T= ;

\ The same shape out of a pool that starts at register four: `add x4, x4, x5`,
\ `add x4, x4, x6`, `ret`. Every register field differs from the case above.
: SUM3-HIGH-BODY ( IR-CTX:ctx -- n n n n )
   HIR-MOD
   BUILD-SUM3
   4 3 EMITTED-FROM
   A64EMIT:INSNS
   0 A64EMIT:WORD@
   1 A64EMIT:WORD@
   2 A64EMIT:WORD@ ;

: SUM3-HIGH-CASE ( -- )
   s" a pool that starts above register zero reaches every register field" T-LABEL
   WBND [: SUM3-HIGH-BODY ;] IR-CTX:WITH-CONTEXT
   $D65F03C0 T= $8B060084 T= $8B050084 T= 3 T= ;

\ `movz x0, #$5678` , `movk x0, #$1234, lsl 48`, `ret`. The half selector in the
\ encoding is three, and the dialect records the shift as forty-eight bits, so
\ this is also where that conversion is measured.
: WIDE-BODY ( IR-CTX:ctx -- n n n n )
   HIR-MOD
   BUILD-WIDE
   4 EMITTED
   A64EMIT:INSNS
   0 A64EMIT:WORD@
   1 A64EMIT:WORD@
   2 A64EMIT:WORD@ ;

: WIDE-CASE ( -- )
   s" a two-half literal emits as a move-wide and its overwrite" T-LABEL
   WBND [: WIDE-BODY ;] IR-CTX:WITH-CONTEXT
   $D65F03C0 T= $F2E24680 T= $D28ACF00 T= 3 T= ;

\ ---- the source map ----------------------------------------------------------
\ The literal's two instructions both answer for the body word the constant came
\ from, and the return answers for the closing `;`. One source, three offsets,
\ two distinct spans.
: MAP-BODY ( IR-CTX:ctx -- n n n n n n n n n n )
   HIR-MOD
   BUILD-WIDE
   4 EMITTED
   0 A64EMIT:MAP-OFFSET@
   1 A64EMIT:MAP-OFFSET@
   2 A64EMIT:MAP-OFFSET@
   0 SPAN-SRC-AT
   0 SPAN-START-AT
   0 SPAN-LEN-AT
   1 SPAN-START-AT
   1 SPAN-LEN-AT
   2 SPAN-START-AT
   2 SPAN-LEN-AT ;

: MAP-CASE ( -- )
   s" every emitted instruction maps to the span of the operation it came from" T-LABEL
   WBND [: MAP-BODY ;] IR-CTX:WITH-CONTEXT
   CLOSE-LN T= CLOSE-ST T=
   BODY-LN T= BODY-ST T=
   BODY-LN T= BODY-ST T=
   0 T= 8 T= 4 T= 0 T= ;

\ ---- the emitted bytes, executed ---------------------------------------------
\ Published into the engine's own code space and called as a leaf routine. The
\ answer is the source-level arithmetic's, not the emitter's idea of it.
: RUN-SQUARE-BODY ( IR-CTX:ctx -- n n )
   HIR-MOD
   BUILD-SQUARE
   4 EMITTED
   RESULT-REG
   7 PUBLISH EXEC1 ;

: RUN-SQUARE-CASE ( -- )
   s" the emitted square really squares when the machine runs it" T-LABEL
   WBND [: RUN-SQUARE-BODY ;] IR-CTX:WITH-CONTEXT
   49 T= 0 T= ;

: RUN-DIFF-BODY ( IR-CTX:ctx -- n n )
   HIR-MOD
   BUILD-DIFF
   4 EMITTED
   RESULT-REG
   9 4 PUBLISH EXEC2 ;

: RUN-DIFF-CASE ( -- )
   s" the emitted difference subtracts the second argument from the first" T-LABEL
   WBND [: RUN-DIFF-BODY ;] IR-CTX:WITH-CONTEXT
   5 T= 0 T= ;

: RUN-SUM3-BODY ( IR-CTX:ctx -- n n )
   HIR-MOD
   BUILD-SUM3
   4 EMITTED
   RESULT-REG
   1 2 3 PUBLISH EXEC3 ;

: RUN-SUM3-CASE ( -- )
   s" the emitted three-argument sum really adds all three" T-LABEL
   WBND [: RUN-SUM3-BODY ;] IR-CTX:WITH-CONTEXT
   6 T= 0 T= ;

: RUN-REUSE-BODY ( IR-CTX:ctx -- n n )
   HIR-MOD
   BUILD-REUSE
   4 EMITTED
   RESULT-REG
   10 3 PUBLISH EXEC2 ;

: RUN-REUSE-CASE ( -- )
   s" the emitted reuse shape adds the first argument in twice" T-LABEL
   WBND [: RUN-REUSE-BODY ;] IR-CTX:WITH-CONTEXT
   23 T= 0 T= ;

: RUN-WIDE-BODY ( IR-CTX:ctx -- n n )
   HIR-MOD
   BUILD-WIDE
   4 EMITTED
   RESULT-REG
   PUBLISH EXEC0 ;

: RUN-WIDE-CASE ( -- )
   s" the emitted move-wide chain materialises the whole literal" T-LABEL
   WBND [: RUN-WIDE-BODY ;] IR-CTX:WITH-CONTEXT
   $1234000000005678 T= 0 T= ;

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
   CC BB A64RA:BIND-DIALECT ;

: BIND-RAV ( -- )
   CC BB A64RAV:BIND-DIALECT ;

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
\ so a case can build a chain the producers in this tree never build. Every one
\ of the shapes below is refused by the emitter, and each is a shape a rewrite
\ between selection and emission could plausibly produce.
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
   CC BB A64SPILL:BIND-DIALECT ;

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

\ `movz x0, #7` then `ret`: a module built by hand emits exactly as one that came
\ through selection does.
: PLAIN-BODY ( IR-CTX:ctx -- n n n bool )
   PLAIN-EMITTED
   A64EMIT:INSNS
   0 A64EMIT:WORD@
   1 A64EMIT:WORD@
   A64EMIT:SEALED? ;

: PLAIN-CASE ( -- )
   s" a hand-built machine module emits the instructions it names" T-LABEL
   WBND [: PLAIN-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE $D65F03C0 T= $D28000E0 T= 2 T= ;

\ ---- the two addressed instructions, as the exact words they are -------------
\ The whole reason to pin these two words rather than only run the body: an
\ addressed store takes a value and an address, and the two are both registers,
\ so a routine that swapped them would write the address into whatever cell the
\ VALUE happens to name. Running it then fails by dying somewhere else, which
\ proves nothing about which field is which. The emitted word says it exactly:
\ the store's transfer field is the value's register and its base field is the
\ address's, and the load's transfer field is the loaded value's register. Both
\ offsets are zero, which is `[Xn]`, and a form that grew an offset it should not
\ have moves these numbers.
\
\ AND THE ROUTINE IS ELEVEN INSTRUCTIONS AND NOT THIRTEEN, which is where the
\ positions below come from. It takes one cell and leaves one, so the place the
\ caller left the data-stack pointer and the place it expects it back are the
\ same place; the routine stands there, and the two adjustments that used to
\ bracket the body are distances of zero that no instruction is written for.
: BUMP-BODY ( IR-CTX:ctx -- n n n n )
   HIR-MOD
   BUILD-BUMP
   6 1 1 EMITTED-HABU
   A64EMIT:INSNS
   2 A64EMIT:WORD@                   \ str x0, [x1] - the argument into the cell
   4 A64EMIT:WORD@                   \ ldr x0, [x0] - and back out of it
   8 A64EMIT:WORD@ ;                 \ str x0, [x1] - the bumped value in again

: BUMP-CASE ( -- )
   s" an addressed store and load emit through the registers they name" T-LABEL
   WBND [: BUMP-BODY ;] IR-CTX:WITH-CONTEXT
   $F9000020 T= $F9400000 T= $F9000020 T= 11 T= ;

\ ---- the two addressing modes a data-stack access is written in --------------
\ THE WHOLE OF WHAT THE PLACEMENT COSTS THE ENCODER. A routine stands where the
\ fewest pointer adjustments are needed, so the cell an access names can be UNDER
\ the pointer as easily as over it - and under it has no spelling in the scaled
\ unsigned Ldr and Str. It is the unscaled SIGNED pair, Ldur and Stur, and which
\ of the two an access is written in is decided by the sign of its offset and
\ nothing else.
\
\ WHY THE EXACT WORDS AND NOT THE MNEMONIC. The two forms differ in one bit of
\ the size field and hold their offsets in DIFFERENT fields - twelve scaled bits
\ at bit ten against nine signed bits at bit twelve - so an access written in the
\ wrong one reads a cell somewhere else entirely rather than failing to encode.
\ Squaring one cell is the smallest routine that has both: it stands at 8, so its
\ load and its store are both eight bytes under the pointer, and both are the
\ negative form. The routine is also RUN, in RUN-SQUARE-CASE above, over the same
\ contract - so a wrong field would answer something other than forty-nine as
\ well as read differently here.
: SQUARE-HABU-BODY ( IR-CTX:ctx -- n n n n )
   HIR-MOD
   BUILD-SQUARE
   4 1 1 EMITTED-HABU
   A64EMIT:INSNS
   0 A64EMIT:WORD@                   \ ldur x0, [x19, #-8] - the argument's cell
   2 A64EMIT:WORD@                   \ stur x0, [x19, #-8] - the result into it
   7 PUBLISH NRUN:ENTER1 ;

: SQUARE-HABU-CASE ( -- )
   s" a cell under the pointer is written in the unscaled signed form" T-LABEL
   WBND [: SQUARE-HABU-BODY ;] IR-CTX:WITH-CONTEXT
   49 T= $F81F8260 T= $F85F8260 T= 4 T= ;

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
   c nb A64RA:BIND-DIALECT
   c nb A64RAV:BIND-DIALECT
   c nb A64EMIT:BIND-DIALECT
   c m0 nb TXT TXT-N A64SPILL:REWRITE {: m1:IR-BUILD:module :}
   c m1 3 16 NFIX:LEAF-FRAMED A64RA:ALLOCATE
   m1 3 16 NFIX:LEAF-FRAMED A64RAV:ACCEPT
   c m1 A64EMIT:EMIT ;

: SPILL-BODY ( IR-CTX:ctx -- n n n n n )
   SPILL-EMITTED
   A64EMIT:INSNS
   0 A64EMIT:WORD@                   \ sub sp, sp, #16 - the routine takes its frame
   7 A64EMIT:WORD@                   \ str x2, [sp, #0] - the third value is put away
   14 A64EMIT:WORD@                  \ ldr x1, [sp, #0] - and comes back for the sum
   NFIX:RESULT-REG ;

: SPILL-CASE ( -- )
   s" a block that does not fit reserves a frame and spills into it" T-LABEL
   WBND [: SPILL-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= $F94003E1 T= $F90003E2 T= $D10043FF T= 21 T= ;

: RUN-SPILL-BODY ( IR-CTX:ctx -- n n )
   SPILL-EMITTED
   NFIX:RESULT-REG
   PUBLISH EXEC0 ;

: RUN-SPILL-CASE ( -- )
   s" the emitted spilled program computes what its values add up to" T-LABEL
   WBND [: RUN-SPILL-BODY ;] IR-CTX:WITH-CONTEXT
   $11 $22 + $33 + $44 + $55 + 2 * T= 0 T= ;

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
   c nb A64RA:BIND-DIALECT
   c nb A64RAV:BIND-DIALECT
   c nb A64EMIT:BIND-DIALECT
   c m0 nb TXT TXT-N A64SPILL:REWRITE {: m1:IR-BUILD:module :}
   c m1 3 0 NFIX:LEAF-FRAMED A64RA:ALLOCATE
   m1 3 0 NFIX:LEAF-FRAMED A64RAV:ACCEPT
   c m1 A64EMIT:EMIT ;

: REMAT-EMIT-BODY ( IR-CTX:ctx -- n n n )
   REMAT-EMITTED
   A64EMIT:INSNS
   0 A64EMIT:WORD@                   \ movz x0, #$11 - no stack adjustment at all
   NFIX:RESULT-REG ;

: REMAT-EMIT-CASE ( -- )
   s" a block that does not fit writes its constants again and takes no frame"
   T-LABEL
   WBND [: REMAT-EMIT-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= $D2800220 T= 12 T= ;

: RUN-REMAT-BODY ( IR-CTX:ctx -- n n )
   REMAT-EMITTED
   NFIX:RESULT-REG
   PUBLISH EXEC0 ;

: RUN-REMAT-CASE ( -- )
   s" the emitted re-emitting program computes what its constants add up to"
   T-LABEL
   WBND [: RUN-REMAT-BODY ;] IR-CTX:WITH-CONTEXT
   $11 $22 + $33 + $44 + $55 + T= 0 T= ;

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

: SECOND-ABI ( -- A64EFF:routine )
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
   c nb A64RA:BIND-DIALECT
   c nb A64RAV:BIND-DIALECT
   c nb A64EMIT:BIND-DIALECT
   c m0 nb TXT TXT-N A64SPILL:REWRITE {: m1:IR-BUILD:module :}
   c m1 SECOND-ABI A64RA:ALLOCATE
   m1 SECOND-ABI A64RAV:ACCEPT
   c m1 A64EMIT:EMIT ;

: SECOND-BODY ( IR-CTX:ctx -- n n n )
   SECOND-EMITTED
   A64EMIT:INSNS
   0 A64EMIT:WORD@                   \ mov x0, x1 - orr x0, xzr, x1
   1 A64EMIT:WORD@ ;

: SECOND-CASE ( -- )
   s" a returned value in the wrong register is copied into the right one" T-LABEL
   WBND [: SECOND-BODY ;] IR-CTX:WITH-CONTEXT
   $D65F03C0 T= $AA0103E0 T= 2 T= ;

: RUN-SECOND-BODY ( IR-CTX:ctx -- n n )
   SECOND-EMITTED
   NFIX:RESULT-REG
   7 9 PUBLISH EXEC2 ;

: RUN-SECOND-CASE ( -- )
   s" the emitted copy really returns the second argument" T-LABEL
   WBND [: RUN-SECOND-BODY ;] IR-CTX:WITH-CONTEXT
   9 T= 0 T= ;

\ ---- refusals ----------------------------------------------------------------
\ Nobody has accepted anything yet. This case runs FIRST in the suite, because an
\ acceptance is package state no later run takes back: once one allocation has
\ been accepted, the ways to be refused for it are that it is about another
\ module or that a later walk replaced it, and both have cases of their own.
: UNACCEPTED-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c A64-NEW
   BIND-EMIT
   BUILD-PLAIN
   M-FREEZE {: m:IR-BUILD:module :}
   c m A64EMIT:EMIT ;

\ A module the binding was not taken over: the binding is taken over the first
\ module and the second is the one presented.
: WRONG-MODULE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c A64-NEW
   BIND-EMIT
   BUILD-PLAIN
   M-FREEZE drop
   c A64-NEW
   BUILD-PLAIN
   M-FREEZE {: m2:IR-BUILD:module :}
   c m2 A64EMIT:EMIT ;

: NO-BIND-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c A64-NEW
   BUILD-PLAIN
   M-FREEZE {: m:IR-BUILD:module :}
   c m A64EMIT:EMIT ;

: TWICE-BIND-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   IR-BUILD:PLAN-BEGIN
   IR-BUILD:PLAN-DEFAULT
   c A64IR:NEW-BUILDER {: b:IR-BUILD:builder :}
   c b A64EMIT:BIND-DIALECT
   c b A64EMIT:BIND-DIALECT ;

: WRONG-DIALECT-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   IR-BUILD:PLAN-BEGIN
   IR-BUILD:PLAN-DEFAULT
   c HIR:NEW-BUILDER {: b:IR-BUILD:builder :}
   c b A64EMIT:BIND-DIALECT ;

\ BOTH FUNCTIONS' INSTRUCTIONS, IN ONE EMISSION, read back as the words they are.
\ The four instructions are the two functions end to end - each one's move-wide
\ and its return - and the literals say which is which, so an emitter that laid
\ the second function over the first, or wrote the first one twice, answers
\ different words here rather than a different count.
: TWO-FUNS-BODY ( IR-CTX:ctx -- n n n n n bool )
   {: c:IR-CTX:ctx :}
   c A64-NEW
   BIND-RA
   BIND-RAV
   BIND-EMIT
   BUILD-TWO-FUNS
   M-FREEZE {: m:IR-BUILD:module :}
   c m 0 4 NFIX:FINISH
   A64EMIT:INSNS
   0 A64EMIT:WORD@   1 A64EMIT:WORD@
   2 A64EMIT:WORD@   3 A64EMIT:WORD@
   A64EMIT:SEALED? ;

: TWO-FUNS-CASE ( -- )
   s" a module of two functions emits both of them, one after the other" T-LABEL
   WBND [: TWO-FUNS-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE
   $D65F03C0 T=  $D2800120 T=
   $D65F03C0 T=  $D28000E0 T=
   4 T= ;

: EXTRA-OPCODE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c A64-NEW
   BIND-EMIT
   BUILD-EXTRA
   M-FREEZE {: m:IR-BUILD:module :}
   c m A64EMIT:EMIT ;

\ These instructions belong to one architecture. The module is built under the
\ machine this dialect is for and presented under one it is not.
: WRONG-TARGET-INNER ( IR-BUILD:module IR-CTX:ctx -- )
   {: m:IR-BUILD:module c:IR-CTX:ctx :}
   c m A64EMIT:EMIT ;

: WRONG-TARGET-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c A64-NEW
   BIND-EMIT
   BUILD-PLAIN
   M-FREEZE {: m:IR-BUILD:module :}
   m PBND [: WRONG-TARGET-INNER ;] IR-CTX:WITH-CONTEXT ;

\ An acceptance about one module is not an answer about another: the first module
\ is allocated and accepted, the second is the one presented for emission.
: OTHER-ALLOC-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c A64-NEW
   BIND-RA
   BIND-RAV
   BUILD-PLAIN
   M-FREEZE {: m1:IR-BUILD:module :}
   c m1 4 NFIX:LEAF-N A64RA:ALLOCATE
   m1 4 NFIX:LEAF-N A64RAV:ACCEPT
   c A64-NEW
   BIND-EMIT
   BUILD-PLAIN
   M-FREEZE {: m2:IR-BUILD:module :}
   c m2 A64EMIT:EMIT ;

\ An accepted answer stops being one when a later walk replaces the allocation it
\ was about, and the emitter finds that out before it writes a byte.
: STALE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c A64-NEW
   BIND-RA
   BIND-RAV
   BIND-EMIT
   BUILD-PLAIN
   M-FREEZE {: m1:IR-BUILD:module :}
   c m1 4 NFIX:LEAF-N A64RA:ALLOCATE
   m1 4 NFIX:LEAF-N A64RAV:ACCEPT
   c A64-NEW
   BIND-RA
   BUILD-PLAIN
   M-FREEZE {: m2:IR-BUILD:module :}
   c m2 4 NFIX:LEAF-N A64RA:ALLOCATE
   c m1 A64EMIT:EMIT ;

\ An index outside a sealed emission.
: PAST-END-BODY ( IR-CTX:ctx -- )
   PLAIN-EMITTED
   A64EMIT:INSNS A64EMIT:WORD@ drop ;

: PAST-MAP-BODY ( IR-CTX:ctx -- )
   PLAIN-EMITTED
   -1 A64EMIT:MAP-OFFSET@ drop ;

\ ---- the three address-chain shapes the emitter refuses ----------------------
\ A MOVN CLAIMING AN ADDRESS. The relocation pass writes four plain immediates
\ over a chain; a movn builds its value out of ones, so a lane that was one would
\ have to be complemented and the pass does not look. Refused where the movn word
\ is encoded.
\
\ IT IS A FULL FOUR-LANE RUN IN ONE REGISTER, and that is the point of the
\ fixture rather than an incidental detail. A bare movn carrying the kind is a
\ ONE-lane run, which the run-length check below refuses on its own - so a case
\ built that way passes with the movn guard deleted and proves nothing about it.
\ Leading a genuine carrier with a movn is the only shape that reaches the movn
\ guard with every other check satisfied: deleting the guard then reds THIS case
\ and leaves the other two green.
: BUILD-MOVN-ADDR ( -- )
   s" MOVNADDR" 0 1 OPEN-FUN
   A64IR-OPCODE:MOVN 1 0 A64IR:ADDR-DATA M-WIDE
   2 16 A64IR:ADDR-DATA M-WIDE-K
   3 32 A64IR:ADDR-DATA M-WIDE-K
   4 48 A64IR:ADDR-DATA M-WIDE-K
   M-RET
   CLOSE-FUN ;

\ A RUN THAT IS NOT THE CARRIER'S WIDTH. Three lanes leave no room for a fourth
\ half that rebasing can make non-zero, so a three-lane run is not a site and is
\ not silently treated as one either.
: BUILD-SHORT-RUN ( -- )
   s" SHORTRUN" 0 1 OPEN-FUN
   A64IR-OPCODE:MOVZ 1 0 A64IR:ADDR-DATA M-WIDE
   2 16 A64IR:ADDR-DATA M-WIDE-K
   3 32 A64IR:ADDR-DATA M-WIDE-K
   M-RET
   CLOSE-FUN ;

\ FOUR LANES THAT DO NOT NAME ONE REGISTER. Four move-wides into two registers
\ spell out no address any site pushed, and the loader refuses exactly this shape
\ from the other end (src/habu/habu2.f EMIT-ADDRS). Two independent two-lane
\ chains are the way to build it: neither takes the other as an operand, so the
\ allocator has no reason to give them the same register.
: BUILD-SPLIT-RUN ( -- )
   s" SPLITRUN" 0 1 OPEN-FUN
   A64IR-OPCODE:MOVZ 1 0 A64IR:ADDR-DATA M-WIDE
   2 16 A64IR:ADDR-DATA M-WIDE-K {: a:IR-ID:ir-value-id :}
   A64IR-OPCODE:MOVZ 3 0 A64IR:ADDR-DATA M-WIDE
   4 16 A64IR:ADDR-DATA M-WIDE-K {: b:IR-ID:ir-value-id :}
   a b M-ADD M-RET
   CLOSE-FUN ;

: EMIT-BUILT ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   M-FREEZE {: m:IR-BUILD:module :}
   c m 0 4 NFIX:FINISH ;

: MOVN-ADDR-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c A64-NEW BIND-RA BIND-RAV BIND-EMIT BUILD-MOVN-ADDR c EMIT-BUILT ;

: SHORT-RUN-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c A64-NEW BIND-RA BIND-RAV BIND-EMIT BUILD-SHORT-RUN c EMIT-BUILT ;

: SPLIT-RUN-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c A64-NEW BIND-RA BIND-RAV BIND-EMIT BUILD-SPLIT-RUN c EMIT-BUILT ;

\ ---- refusal cases -----------------------------------------------------------
: UNACCEPTED ( -- )      WBND [: UNACCEPTED-BODY ;] IR-CTX:WITH-CONTEXT ;
: WRONG-MODULE ( -- )    WBND [: WRONG-MODULE-BODY ;] IR-CTX:WITH-CONTEXT ;
: NO-BIND ( -- )         WBND [: NO-BIND-BODY ;] IR-CTX:WITH-CONTEXT ;
: TWICE-BIND ( -- )      WBND [: TWICE-BIND-BODY ;] IR-CTX:WITH-CONTEXT ;
: WRONG-DIALECT ( -- )   WBND [: WRONG-DIALECT-BODY ;] IR-CTX:WITH-CONTEXT ;
: EXTRA-OPCODE ( -- )    WBND [: EXTRA-OPCODE-BODY ;] IR-CTX:WITH-CONTEXT ;
: WRONG-TARGET ( -- )    WBND [: WRONG-TARGET-BODY ;] IR-CTX:WITH-CONTEXT ;
: OTHER-ALLOC ( -- )     WBND [: OTHER-ALLOC-BODY ;] IR-CTX:WITH-CONTEXT ;
: STALE ( -- )           WBND [: STALE-BODY ;] IR-CTX:WITH-CONTEXT ;
: PAST-END ( -- )        WBND [: PAST-END-BODY ;] IR-CTX:WITH-CONTEXT ;
: PAST-MAP ( -- )        WBND [: PAST-MAP-BODY ;] IR-CTX:WITH-CONTEXT ;
: MOVN-ADDR ( -- )       WBND [: MOVN-ADDR-BODY ;] IR-CTX:WITH-CONTEXT ;
: SHORT-RUN ( -- )       WBND [: SHORT-RUN-BODY ;] IR-CTX:WITH-CONTEXT ;
: SPLIT-RUN ( -- )       WBND [: SPLIT-RUN-BODY ;] IR-CTX:WITH-CONTEXT ;

: DROP-BINDING ( -- )
   A64EMIT:RELEASE ;

\ Each of the three names its own shape, so a guard that stopped refusing one of
\ them leaves the other two green and the case that reds says which.
: ADDR-REFUSE-CASES ( -- )
   s" a movn that claims to carry an address is refused" T-LABEL
   [: MOVN-ADDR ;] E-A64EMIT-ADDR TTHROWSQ
   s" an address run shorter than the carrier is refused" T-LABEL
   [: SHORT-RUN ;] E-A64EMIT-ADDR TTHROWSQ
   s" four address lanes that do not name one register are refused" T-LABEL
   [: SPLIT-RUN ;] E-A64EMIT-ADDR TTHROWSQ ;

: ALLOC-REFUSE-CASES ( -- )
   s" emitting from a register assignment nobody accepted is refused" T-LABEL
   [: UNACCEPTED ;] E-A64EMIT-ALLOC TTHROWSQ ;

: BIND-REFUSE-CASES ( -- )
   s" emitting without a binding is refused" T-LABEL
   [: NO-BIND ;] E-A64EMIT-BIND TTHROWSQ
   s" a second binding over a live one is refused" T-LABEL
   [: TWICE-BIND ;] E-A64EMIT-BIND TTHROWSQ
   DROP-BINDING ;

: MODULE-REFUSE-CASES ( -- )
   s" a frozen module the binding was not taken over is refused" T-LABEL
   [: WRONG-MODULE ;] E-A64EMIT-MODULE TTHROWSQ
   s" binding a builder of another dialect is refused" T-LABEL
   [: WRONG-DIALECT ;] E-A64EMIT-MODULE TTHROWSQ ;

: SHAPE-REFUSE-CASES ( -- )
   s" an operation of a form outside the dialect's family is refused" T-LABEL
   [: EXTRA-OPCODE ;] E-A64EMIT-OPCODE TTHROWSQ ;

: TARGET-REFUSE-CASES ( -- )
   s" emitting under a context bound to another machine is refused" T-LABEL
   [: WRONG-TARGET ;] E-A64EMIT-TARGET TTHROWSQ ;

: OTHER-ALLOC-REFUSE-CASE ( -- )
   s" an acceptance made from another module is refused" T-LABEL
   [: OTHER-ALLOC ;] E-A64EMIT-ALLOC TTHROWSQ ;

\ Its own group: this fixture and the one above each abandon a context holding
\ two modules, and two of those at once run the arena registry dry.
: STALE-REFUSE-CASE ( -- )
   s" an acceptance a later allocation replaced stops answering" T-LABEL
   [: STALE ;] E-A64RAV-STATE TTHROWSQ ;

: BOUND-REFUSE-CASES ( -- )
   s" an instruction index past the emission is refused" T-LABEL
   [: PAST-END ;] E-A64EMIT-BOUND TTHROWSQ
   s" a source-map index below the emission is refused" T-LABEL
   [: PAST-MAP ;] E-A64EMIT-BOUND TTHROWSQ ;

\ ---- groups ------------------------------------------------------------------
: GROUP-ALLOC ( IR-CTX:ctx -- )   drop ALLOC-REFUSE-CASES ;
: GROUP-BIND ( IR-CTX:ctx -- )    drop BIND-REFUSE-CASES ;
: GROUP-MODULE ( IR-CTX:ctx -- )  drop MODULE-REFUSE-CASES ;
: GROUP-SHAPE ( IR-CTX:ctx -- )   drop SHAPE-REFUSE-CASES ;
: GROUP-TARGET ( IR-CTX:ctx -- )  drop TARGET-REFUSE-CASES ;
: GROUP-ACCEPT ( IR-CTX:ctx -- )  drop OTHER-ALLOC-REFUSE-CASE ;
: GROUP-STALE ( IR-CTX:ctx -- )   drop STALE-REFUSE-CASE ;
: GROUP-BOUND ( IR-CTX:ctx -- )   drop BOUND-REFUSE-CASES ;
: GROUP-ADDR ( IR-CTX:ctx -- )    drop ADDR-REFUSE-CASES ;

public

: RUN ( -- )
   T-RESET
   WBND [: GROUP-ALLOC ;] IR-CTX:WITH-CONTEXT
   SQUARE-CASE
   BYTES-CASE
   DIFF-CASE
   DIV-CASE
   SUM3-CASE
   REUSE-CASE
   SUM3-HIGH-CASE
   WIDE-CASE
   BUMP-CASE
   SQUARE-HABU-CASE
   MAP-CASE
   RUN-SQUARE-CASE
   RUN-DIFF-CASE
   RUN-DIV-CASE
   RUN-SUM3-CASE
   RUN-REUSE-CASE
   RUN-WIDE-CASE
   PLAIN-CASE
   TWO-FUNS-CASE
   SPILL-CASE
   RUN-SPILL-CASE
   REMAT-EMIT-CASE
   RUN-REMAT-CASE
   SECOND-CASE
   RUN-SECOND-CASE
   WBND [: GROUP-ADDR ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-BIND ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-MODULE ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-SHAPE ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-TARGET ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-ACCEPT ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-STALE ;] IR-CTX:WITH-CONTEXT
   WBND [: GROUP-BOUND ;] IR-CTX:WITH-CONTEXT
   T-REPORT ;

;package

A64EMIT-TEST:RUN

\ x64-emit.f - the x86-64 bytes: src/compiler/native/emit-x64.f (X64EMIT) over
\ the routines src/compiler/native/select-x64.f selects and the allocator and its
\ validator accept, and over two routines written straight in the machine
\ dialect. THE EMITTER OWNS ITS SINK: a case declares where the routine goes with
\ `X64EMIT:PLACE-AT`, emits, reads the sealed image back and retires it, which is
\ what gives the placement back as well as the bytes. A SHADOW case emits with no
\ placement at all: every field that depends on one is left to the rows.
\
\ HOW THE BYTES ARE PINNED. The WHOLE byte string of the routine is compared
\ against a fixed expectation, the way test/compiler/x86-64-asm.f pins one
\ encoder at a time. The `mc:` comment above each pinned line is the instruction
\ that byte group is, one mnemonic a line, as
\ `llvm-mc -triple=x86_64 -disassemble` read the pinned string back (LLVM 22.1.8,
\ 2026-09-22); every line was then re-assembled with
\ `llvm-mc -triple=x86_64 -show-encoding` and answered exactly the bytes pinned
\ here, WITH THE TWO EXCEPTIONS NAMED BELOW. That agreement is not free:
\ src/arch/x86-64/asm.f implements exactly one encoding per operation and none
\ of llvm-mc's preferred short forms (05 id for `add rax`, D1 /n for a shift by
\ one, 83 /n ib for an ALU immediate that fits a signed byte), so the fixtures
\ below fold their immediates onto the SECOND argument, shift by a count other
\ than one, name rax as the destination of no immediate form, and hold every
\ folded literal outside a signed byte.
\
\ THE FIRST EXCEPTION IS THE DATA-STACK ADJUSTMENT, which no fixture can dodge: a
\ pointer move is a small multiple of eight and therefore always fits the byte
\ form llvm-mc prefers, while `x64.dtake` and `x64.dpublish` are the imm32 add
\ and subtract of this dialect whatever the distance (emit-x64.f PUT-DMOVE).
\ llvm-mc DISASSEMBLES the pinned 49 81 ec 10 00 00 00 as the `subq $16, %r12`
\ its `mc:` line gives, and re-ASSEMBLES that line as the four-byte
\ 49 83 ec 10: on those lines the instruction agrees and the encoding is the
\ longer one this encoder has. The frame's `x64.reserve` and `x64.release` are
\ the same imm32 subtract and add on rsp, and disagree the same way. So does
\ the comparison with minus one every `x64.idiv` carries: llvm-mc re-assembles
\ `cmpq $-1` as 48 83 /7 ff.
\
\ THE SECOND IS THE BRANCH, and it is the same kind of disagreement. A `mc:` line
\ for a branch or a call carries the rel32 FIELD as llvm-mc printed it - `je 11`,
\ `jmp -26`, `callq 980` - and re-assembling that line answers a fixup holding
\ the very same number. For `callq` the encoding agrees outright, e8 having no
\ short form; for `jmp` and `jcc` llvm-mc assembles the two-byte relaxable form
\ (EB cb, 7x cb) where this emitter always writes the wide one, because rel8
\ relaxation is not a condition of correctness here (docs/x86-64.md). The
\ displacement itself is measured FROM THE END of its instruction, so the target
\ of a pinned `jmp -26` is 26 bytes before the byte after it.
\
\ THE FIXTURES ARE THE CONTRACTS x64-regalloc.f ALLOCATES UNDER, one per shape.
\ Most are the REGISTER-convention leaf: the contract names no place, so the
\ module is the body alone and every value in it is one the allocator is free to
\ place. Then the DATA-STACK convention (X64ABI:LEAF), where the interface is
\ caller cells and the selector writes the boundary itself - the only shape the
\ four data-stack crossings and the four addressed forms appear in, because the
\ order an addressed form needs is the one the entry's `x64.dtake` minted and the
\ exit's `x64.dpublish` consumes. Then the two contracts a call site needs:
\ X64ABI:CALL for a routine that calls and comes back, X64ABI:TAIL for one that
\ leaves through its callee. A call site cannot be staged in the dialect instead
\ - the memory order it mints would be read by nobody and the validator refuses
\ that by name - so `x64.call`, `x64.wordcall` and `x64.tailcall` reach this
\ emitter only through the selector. Last X64ABI:NORET-LEAF-FRAMED, the
\ contract of a routine that ends in `die`. ONE FIXTURE PER CONTEXT, and the refusing
\ cases run inside an enclosing one, for the reason that suite gives.
\
\ WHAT THIS SUITE DOES NOT PIN. Of the forms this emitter renders, `x64.cmpbri`
\ has no byte string here: the selector is deliberately unfused and mints no
\ compare-and-branch at all, and the diamond below is the one staged in the
\ dialect. Nor have the scalar double forms: their encoders are pinned one at a
\ time against llvm-mc in test/compiler/x86-64-asm.f, and the routines the
\ double fixtures below select run as images in test/x86-64-peer-routines.f,
\ whose answers hold every render to what this engine's own float words answer.
\ What else is left unmeasured is a byte no module of this slice can produce,
\ and each of them is a later slice's:
\
\ - THE REGISTERS ABOVE rdx. The pool is rax, rcx, rdx, rsi, rdi and r8..r11
\   (x64ir.f RESERVED-MASK), and no fixture here is wide enough for the
\   allocator to reach past rdx, so no pinned byte sets REX.R or REX.B for an
\   operand and none names the spl/bpl/sil/dil byte registers.
\ - A DATA-STACK DISPLACEMENT OUTSIDE disp8, or a negative one. `x64.dslot` is
\   signed and reaches disp32 (x64ir.f DSLOT), which wants a contract of more
\   than sixteen cells.
\ - A FRAME SLOT OUTSIDE disp8. The validator bounds a slot by the module's own
\   value count (regalloc-verify.f FLOW-SLOT refuses E-A64RAV-SLOT) and the spill
\   pass reuses a slot once its value is dead, so slot 128 wants seventeen values
\   put away at once.
\ - THE FORMS STILL REFUSED BY NAME, one case standing for all of them below:
\   `x64.neg` and the selects `x64.cmpsel` and `x64.selz`. Each needs a
\   register the machine names or a lowering, and neither is here. An
\   `x64.fcmpset` whose condition and result count are not `gt` with one or
\   `equal` with two is refused the same way, a case for each.
\ - THE FIELD OF THE CALL TO `die`. Its entry is the host engine's, so the trap
\   case pins every other byte and holds that field to the entry (DIE-ENTRY).
\   The call to `throw` in the selected divide is held the same way
\   (THROW-TARGET). Each case is placed at the slot at or below its entry
\   (SLOT-BELOW), so the field reaches the entry wherever the engine loaded.
\ - E-X64EMIT-LAYOUT, the disagreement between the two passes. It is the check
\   that holds the writer to the measurer's numbers, and no module reaches it
\   without a defect in one of them, so no case here can pin it.
\ - E-X64EMIT-SHAPE, one step earlier: the block shapes it names are ones the IR
\   builder will not hand out. A module whose return is followed by another
\   operation is refused by `IR-BUILD:END-BLOCK`, with E-IR-FUN-TERM (-8049,
\   measured; src/compiler/ir/fun.f TERM-CK -
\   "a block that does not end in exactly one terminator operation", which is
\   the empty block as well), so that branch of the emitter's shape check stands
\   behind an invariant the builder already holds.

require lib/test.f
require lib/byte-buffer.f
require lib/fmath.f                       \ FMATH:E-DOMAIN, borrowed for a malformed expected string
require src/compiler/native/select-x64.f
require src/compiler/native/regalloc.f
require src/compiler/native/regalloc-verify.f
require src/compiler/native/emit-x64.f
require src/compiler/native/emission.f
require src/arch/x86-64/abi.f
require src/arch/x86-64/machine.f

package X64EMIT-TEST
private

\ ---- reading one emission back -----------------------------------------------
\ The emitter owns its sink, so a case PLACES, emits, compares the whole sealed
\ image and retires it. Retiring is what gives the placement back as well as the
\ bytes, which is why every case here ends in one - the refusing cases too, the
\ way src/compiler/native/compiler.f ends a refused definition.
: HEX-DIGIT ( n -- n ) {: c:n :}
   c 48 >= c 57 <= and if c 48 - exit then
   c 97 >= c 102 <= and 0= if FMATH:E-DOMAIN throw then
   c 97 - 10 + ;

: HEX-BYTE ( ptr u8 n -- n ) {: a:ptr i:n :}
   a i 2 * + c@ HEX-DIGIT 4 lshift
   a i 2 * 1 + + c@ HEX-DIGIT or ;

: SPAN=HEX? ( ptr u8 n ptr u8 n -- bool ) {: da:ptr dlen:n ea:ptr eu:n :}
   eu 2 mod 0<> if FMATH:E-DOMAIN throw then
   dlen eu 2 / <> if false exit then
   dlen 0 ?do
      ea i HEX-BYTE da i + c@ <> if false unloop exit then
   loop
   true ;

\ Compare the whole routine the last case emitted against the expected string,
\ then retire it so the next case can place its own.
: X= ( ptr u8 n -- ) {: ea:ptr eu:n :}
   X64EMIT:BYTES X64EMIT:SIZE {: da:ptr dlen:n :}
   da dlen ea eu SPAN=HEX? TTRUE
   X64EMIT:RETIRE ;

\ Compare without retiring, for the cases that read the sealed emission's
\ layout, sites or block table after its bytes.
: XB= ( ptr u8 n -- ) {: ea:ptr eu:n :}
   X64EMIT:BYTES X64EMIT:SIZE {: da:ptr dlen:n :}
   da dlen ea eu SPAN=HEX? TTRUE ;

\ ---- bindings ----------------------------------------------------------------
\ The linux x86-64 contract x64-regalloc.f selects and allocates under.
: WBND ( -- CBIND:binding )
   CTARGET-ARCH:X86-64 CTARGET-ABI:SYSV-AMD64 CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64
   CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH CTARGET:CONTRACT
   CNUM-OVERFLOW:WRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY
   CBIND:BIND ;

\ ---- the fixture's source text -----------------------------------------------
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

\ The mem-typed argument the addressed fixture takes: a routine under the
\ register convention has no `hir.mem` to mint an order with - that operation is
\ E-X64SEL-MEM without a data-stack place - so the order this one reads arrives
\ as an ARGUMENT, which is what every addressed form needs and all it needs.
: MEMT ( -- IR-ID:ir-type-id )
   CC BB HIR:MEM-TYPE ;

: MEM-SIGN ( -- IR-ID:ir-type-id )
   CELLT {: t:IR-ID:ir-type-id :}
   IR-TYPE:FN-BEGIN
   t IR-TYPE:FN-PARAM
   MEMT IR-TYPE:FN-PARAM
   t IR-TYPE:FN-RESULT
   CC BB IR-BUILD:INTERN-CODE-REF ;

\ A module's function table admits one function per symbol (E-IR-FUN-DUP), so a
\ module of two names them apart.
: OPEN-FUN-NAMED ( IR-ID:ir-type-id ptr u8 n -- )
   {: sig:IR-ID:ir-type-id a:ptr u:n :}
   CC BB  CC BB a u IR-BUILD:INTERN-SYMBOL  IR-BUILD:BEGIN-FUN
   CC BB  sig  IR-BUILD:SET-SIGNATURE
   CC BB IR--FUN-LINKAGE:DEFINED IR-BUILD:SET-LINKAGE
   CC BB IR--FUN-VISIBILITY:EXPORTED IR-BUILD:SET-VISIBILITY
   CC BB IR--FUN-CONVENTION:HABU IR-BUILD:SET-CONVENTION
   CC BB  NAME-ST NAME-LN SPN  IR-BUILD:SET-FUN-SPAN
   CC BB IR-BUILD:BEGIN-BLOCK
   CC BB  OPEN-ST OPEN-LN SPN  IR-BUILD:SET-BLOCK-SPAN ;

: OPEN-FUN-SIG ( IR-ID:ir-type-id -- )
   s" LEAF" OPEN-FUN-NAMED ;

: OPEN-FUN ( n n -- )
   SIGN OPEN-FUN-SIG ;

: ARG+ ( -- IR-ID:ir-value-id )
   CC BB CELLT IR-BUILD:ADD-BLOCK-ARG ;

: ARG-MEM+ ( -- IR-ID:ir-value-id )
   CC BB MEMT IR-BUILD:ADD-BLOCK-ARG ;

\ The order a routine mints for ITSELF. `hir.mem` is the source operation that
\ has one, and under the data-stack convention the selector answers it with the
\ token the entry's `x64.dtake` already made (select-x64.f EMIT-MEM), so a
\ data-stack routine needs no mem-typed argument to address memory.
: MEM0 ( -- IR-ID:ir-value-id )
   HIR-OPCODE:MEM BODY-ST BODY-LN OPEN-OP
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

: STORE1 ( HIR:opcode IR-ID:ir-value-id IR-ID:ir-value-id IR-ID:ir-value-id -- IR-ID:ir-value-id )
   {: o:HIR:opcode v:IR-ID:ir-value-id a:IR-ID:ir-value-id k:IR-ID:ir-value-id :}
   o BODY-ST BODY-LN OPEN-OP
   CC BB v IR-BUILD:ADD-OPERAND
   CC BB a IR-BUILD:ADD-OPERAND
   CC BB k IR-BUILD:ADD-OPERAND
   CC BB MEMT IR-BUILD:ADD-RESULT
   CLOSE-VALUE ;

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

\ ---- the fixtures ------------------------------------------------------------
\ `: LEAF ( a b -- n ) - ;` - the two-address subtraction that destroys its first
\ operand, with nothing copied.
: BUILD-DIFF ( -- )
   2 1 OPEN-FUN
   ARG+ {: x:IR-ID:ir-value-id :}
   ARG+ {: y:IR-ID:ir-value-id :}
   HIR-OPCODE:SUB x y BINOP RET1
   CLOSE-FUN ;

\ `: LEAF ( a -- n ) dup + ;` - the copy a two-address form needs, which is the
\ only way `x64.mov` reaches this emitter.
: BUILD-SQUARE ( -- )
   1 1 OPEN-FUN
   ARG+ {: a:IR-ID:ir-value-id :}
   HIR-OPCODE:ADD a a BINOP RET1
   CLOSE-FUN ;

\ `: LEAF ( a b -- n ) and over or over xor over * ;` - the four remaining tied
\ binaries in one routine, each destroying the result of the one before it.
: BUILD-CHAIN ( -- )
   2 1 OPEN-FUN
   ARG+ {: a:IR-ID:ir-value-id :}
   ARG+ {: b:IR-ID:ir-value-id :}
   HIR-OPCODE:AND a b BINOP {: x:IR-ID:ir-value-id :}
   HIR-OPCODE:OR x b BINOP {: y:IR-ID:ir-value-id :}
   HIR-OPCODE:XOR y b BINOP {: z:IR-ID:ir-value-id :}
   HIR-OPCODE:MUL z b BINOP RET1
   CLOSE-FUN ;

\ The five immediate forms, folded from literals that fit the dialect's signed
\ thirty-two bits. The chain runs on the SECOND argument so that no instruction
\ names rax, whose accumulator short forms llvm-mc would answer with instead.
: BUILD-IMMS ( -- )
   2 1 OPEN-FUN
   ARG+ {: a:IR-ID:ir-value-id :}
   ARG+ {: b:IR-ID:ir-value-id :}
   HIR-OPCODE:AND b a BINOP {: x:IR-ID:ir-value-id :}
   HIR-OPCODE:ADD x 1000 CONSTOP BINOP {: p:IR-ID:ir-value-id :}
   HIR-OPCODE:SUB p 2000 CONSTOP BINOP {: q:IR-ID:ir-value-id :}
   HIR-OPCODE:AND q 4095 CONSTOP BINOP {: r:IR-ID:ir-value-id :}
   HIR-OPCODE:OR r 61440 CONSTOP BINOP {: s:IR-ID:ir-value-id :}
   HIR-OPCODE:XOR s 255 CONSTOP BINOP RET1
   CLOSE-FUN ;

\ `: LEAF ( a -- n ) 3 lshift 5 rshift ;` - both immediate shifts. A count other
\ than one, so llvm-mc answers with the same C1 /n ib form the encoder writes.
: BUILD-SHIFTS ( -- )
   1 1 OPEN-FUN
   ARG+ {: a:IR-ID:ir-value-id :}
   HIR-OPCODE:LSHIFT a 3 CONSTOP BINOP {: x:IR-ID:ir-value-id :}
   HIR-OPCODE:RSHIFT x 5 CONSTOP BINOP RET1
   CLOSE-FUN ;

\ `: LEAF ( a n -- x ) over swap lshift xor ;` - a computed count, and the value
\ shifted read AFTER the shift: the value's copy is what the two-address form
\ destroys, and the count's copy is the operand fixed to rcx, coalesced back
\ into the count nothing else reads.
: BUILD-SHL ( -- )
   2 1 OPEN-FUN
   ARG+ {: a:IR-ID:ir-value-id :}
   ARG+ {: n:IR-ID:ir-value-id :}
   HIR-OPCODE:LSHIFT a n BINOP {: s:IR-ID:ir-value-id :}
   HIR-OPCODE:XOR a s BINOP RET1
   CLOSE-FUN ;

\ `: LEAF ( a n -- x ) over swap rshift xor ;` - the same shape, shifted right.
: BUILD-SHR ( -- )
   2 1 OPEN-FUN
   ARG+ {: a:IR-ID:ir-value-id :}
   ARG+ {: n:IR-ID:ir-value-id :}
   HIR-OPCODE:RSHIFT a n BINOP {: s:IR-ID:ir-value-id :}
   HIR-OPCODE:XOR a s BINOP RET1
   CLOSE-FUN ;

\ `: LEAF ( a b n -- x ) lshift + ;` - the value shifted dies at the shift, so it
\ is the tied destination itself, and the shift reads it at the instant it reads
\ the count's copy from rcx: it is kept out of rcx, which it would otherwise
\ have taken as the lowest register free where it is written.
: BUILD-SHL-ADD ( -- )
   3 1 OPEN-FUN
   ARG+ {: a:IR-ID:ir-value-id :}
   ARG+ {: b:IR-ID:ir-value-id :}
   ARG+ {: n:IR-ID:ir-value-id :}
   HIR-OPCODE:LSHIFT b n BINOP {: s:IR-ID:ir-value-id :}
   HIR-OPCODE:ADD a s BINOP RET1
   CLOSE-FUN ;

\ `: LEAF ( a n -- x ) 2dup lshift rot xor + ;` - the value shifted and the count
\ are both read after the shift, so both are copied and both originals live
\ across it: neither may hold rcx, where the count's copy is when the shift
\ reads it.
: BUILD-SHL-CROSS ( -- )
   2 1 OPEN-FUN
   ARG+ {: a:IR-ID:ir-value-id :}
   ARG+ {: n:IR-ID:ir-value-id :}
   HIR-OPCODE:LSHIFT a n BINOP {: s:IR-ID:ir-value-id :}
   HIR-OPCODE:XOR s a BINOP {: t:IR-ID:ir-value-id :}
   HIR-OPCODE:ADD n t BINOP RET1
   CLOSE-FUN ;

\ `: LEAF ( a b n -- x ) -rot + swap lshift ;`, or its `rshift` twin - nothing
\ but its copy reads the count, so the copy is coalesced back into it and the
\ class pinned to rcx opens where the count is defined. Under the data-stack
\ convention that is its load, which b crosses on its way to the add: b is kept
\ out of rcx and the count is loaded straight into it.
: BUILD-SUM-SHIFT ( HIR:opcode -- )
   {: o:HIR:opcode :}
   3 1 OPEN-FUN
   ARG+ {: a:IR-ID:ir-value-id :}
   ARG+ {: b:IR-ID:ir-value-id :}
   ARG+ {: n:IR-ID:ir-value-id :}
   HIR-OPCODE:ADD a b BINOP {: s:IR-ID:ir-value-id :}
   o s n BINOP RET1
   CLOSE-FUN ;

\ `: LEAF ( a -- n ) invert ;` - the one unary form of this slice.
: BUILD-NOT ( -- )
   1 1 OPEN-FUN
   ARG+ {: a:IR-ID:ir-value-id :}
   HIR-OPCODE:INVERT a UNOP RET1
   CLOSE-FUN ;

\ `: LEAF ( a b -- n ) / ;` - the divide: the dividend's copy is the operand
\ fixed to rax, and the remainder is a result nothing reads (select-x64.f
\ EMIT-DIV).
: BUILD-DIV ( -- )
   2 1 OPEN-FUN
   ARG+ {: x:IR-ID:ir-value-id :}
   ARG+ {: y:IR-ID:ir-value-id :}
   HIR-OPCODE:DIV x y BINOP RET1
   CLOSE-FUN ;

\ `: LEAF ( a b -- flag ) rel ;` for any of the six relations the selector
\ compares with (select-x64.f COMPARE-COND).
: BUILD-RELATION ( HIR:opcode -- )
   {: o:HIR:opcode :}
   2 1 OPEN-FUN
   ARG+ {: a:IR-ID:ir-value-id :}
   ARG+ {: b:IR-ID:ir-value-id :}
   o a b BINOP RET1
   CLOSE-FUN ;

\ `: LEAF ( a b -- flag ) swap - 1000 rel ;` - the relation against a literal
\ the selector folds into the compare.
: BUILD-RELATIONI ( HIR:opcode -- )
   {: o:HIR:opcode :}
   2 1 OPEN-FUN
   ARG+ {: a:IR-ID:ir-value-id :}
   ARG+ {: b:IR-ID:ir-value-id :}
   HIR-OPCODE:SUB b a BINOP {: x:IR-ID:ir-value-id :}
   o x 1000 CONSTOP BINOP RET1
   CLOSE-FUN ;

\ `: LEAF ( a b -- n ) < ;` - compare, set a byte, widen it, negate it to the
\ all-ones flag: one dialect operation and four instructions, because the flags
\ between them are a single architectural resource no value stands for.
: BUILD-CMPSET ( -- )    HIR-OPCODE:LT BUILD-RELATION ;

\ The same four instructions against a folded literal, again on the second
\ argument so no instruction names rax.
: BUILD-CMPSETI ( -- )   HIR-OPCODE:LT BUILD-RELATIONI ;

\ A literal too wide for the immediate an ALU form carries is materialised, and
\ on this machine that is ONE instruction whatever the cell holds.
: BUILD-MOVI ( -- )
   1 1 OPEN-FUN
   ARG+ {: a:IR-ID:ir-value-id :}
   HIR-OPCODE:ADD a 4294967296 CONSTOP BINOP RET1
   CLOSE-FUN ;

\ `( a tok -- n )`: the cell and byte loads at one address and the cell and byte
\ stores back to it, each taking the memory order the one before it answered.
\ Every one of them encodes at displacement zero, because the dialect gives the
\ addressed forms no offset.
: BUILD-ADDRESSED ( -- )
   MEM-SIGN OPEN-FUN-SIG
   ARG+ {: a:IR-ID:ir-value-id :}
   ARG-MEM+ {: k0:IR-ID:ir-value-id :}
   HIR-OPCODE:LOAD a k0 LOAD1 {: v:IR-ID:ir-value-id k1:IR-ID:ir-value-id :}
   HIR-OPCODE:BLOAD a k1 LOAD1 {: w:IR-ID:ir-value-id k2:IR-ID:ir-value-id :}
   HIR-OPCODE:STORE v a k2 STORE1 {: k3:IR-ID:ir-value-id :}
   HIR-OPCODE:BSTORE w a k3 STORE1 drop
   w RET1
   CLOSE-FUN ;

\ `( a -- n )` under the data-stack convention: the same four addressed forms
\ over an address the routine took off the data stack, with the memory order
\ minted inside the body instead of arriving as an argument.
: BUILD-DADDRESSED ( -- )
   1 1 OPEN-FUN
   ARG+ {: a:IR-ID:ir-value-id :}
   MEM0 {: k0:IR-ID:ir-value-id :}
   HIR-OPCODE:LOAD a k0 LOAD1 {: v:IR-ID:ir-value-id k1:IR-ID:ir-value-id :}
   HIR-OPCODE:BLOAD a k1 LOAD1 {: w:IR-ID:ir-value-id k2:IR-ID:ir-value-id :}
   HIR-OPCODE:STORE v a k2 STORE1 {: k3:IR-ID:ir-value-id :}
   HIR-OPCODE:BSTORE w a k3 STORE1 drop
   w RET1
   CLOSE-FUN ;

: BR1 ( IR-ID:ir-value-id n -- )
   {: v:IR-ID:ir-value-id t:n :}
   HIR-OPCODE:BR CLOSE-ST CLOSE-LN OPEN-OP
   CC BB v IR-BUILD:ADD-OPERAND
   CC BB t BLOCK-ID IR-BUILD:ADD-SUCCESSOR
   CC BB IR-BUILD:END-OP drop ;

\ `B0(x, c0): br B1(c0); B1(c): t = x + c; brz t -> B2 / B3; B2: ret t;
\ B3: br B1(t)` - the loop test/compiler/x64-regalloc.f allocates under the
\ data-stack convention. FOUR blocks and only three of them are written: B3 does
\ nothing but pass control back to the header, so the order chases every branch
\ to it through to B1 and leaves it out. The header is entered by falling in and
\ left by falling out, and the backedge is the one branch that goes backwards.
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

\ ---- the routine that leaves through its callee ------------------------------
\ The same shape test/compiler/x64-regalloc.f allocates under X64ABI:TAIL: one
\ cell in and one out, the call site passing on the very cell this routine was
\ entered with, so the selector writes no store and the whole body is the take
\ and the branch.
$400 constant CALLEE-ENTRY           \ the address the tail case leaves through

: WCALL-ATTRS ( n n n -- )
   {: e:n in:n out:n :}
   CC BB  CC BB HIR:KEY-ENTRY  CC BB e IR-BUILD:INTERN-INT-ATTR IR-BUILD:ADD-ATTR
   CC BB  CC BB HIR:KEY-IN     CC BB in IR-BUILD:INTERN-INT-ATTR IR-BUILD:ADD-ATTR
   CC BB  CC BB HIR:KEY-OUT    CC BB out IR-BUILD:INTERN-INT-ATTR IR-BUILD:ADD-ATTR ;

\ The three operands are the memory order, one value carried across the site and
\ the argument, and the three results are the order, that value again and the
\ callee's answer. `hir.call` is the SELF-call and carries no attributes at all:
\ RECURSE names the definition, so what it takes and leaves is the CONTRACT's
\ own declaration (select-x64.f SELF-SHAPE).
: CALL-OPERANDS ( IR-ID:ir-value-id IR-ID:ir-value-id IR-ID:ir-value-id -- )
   {: tok:IR-ID:ir-value-id live:IR-ID:ir-value-id arg:IR-ID:ir-value-id :}
   CC BB tok IR-BUILD:ADD-OPERAND
   CC BB live IR-BUILD:ADD-OPERAND
   CC BB arg IR-BUILD:ADD-OPERAND
   CC BB MEMT IR-BUILD:ADD-RESULT
   CC BB CELLT IR-BUILD:ADD-RESULT
   CC BB CELLT IR-BUILD:ADD-RESULT ;

: WCALLN ( IR-ID:ir-value-id IR-ID:ir-value-id IR-ID:ir-value-id n -- IR-ID:ir-op-id )
   {: tok:IR-ID:ir-value-id live:IR-ID:ir-value-id arg:IR-ID:ir-value-id e:n :}
   HIR-OPCODE:WORDCALL BODY-ST BODY-LN OPEN-OP
   tok live arg CALL-OPERANDS
   e 1 1 WCALL-ATTRS
   CC BB IR-BUILD:END-OP ;

: CALL1 ( IR-ID:ir-value-id IR-ID:ir-value-id IR-ID:ir-value-id -- IR-ID:ir-op-id )
   {: tok:IR-ID:ir-value-id live:IR-ID:ir-value-id arg:IR-ID:ir-value-id :}
   HIR-OPCODE:CALL BODY-ST BODY-LN OPEN-OP
   tok live arg CALL-OPERANDS
   CC BB IR-BUILD:END-OP ;

\ `: LEAF ( n -- n ) CALLEE ;` - one call site, its answer returned.
: BUILD-WORDCALLER ( n -- )
   {: e:n :}
   1 1 OPEN-FUN
   ARG+ {: a:IR-ID:ir-value-id :}
   MEM0 {: tok:IR-ID:ir-value-id :}
   tok a a e WCALLN {: id:IR-ID:ir-op-id :}
   CC BB id 2 IR-BUILD:OP-RESULT@ RET1
   CLOSE-FUN ;

\ `: LEAF ( n -- n ) RECURSE ;` - the same shape through the definition's own
\ label instead of another word's address.
: BUILD-SELFCALLER ( -- )
   1 1 OPEN-FUN
   ARG+ {: a:IR-ID:ir-value-id :}
   MEM0 {: tok:IR-ID:ir-value-id :}
   tok a a CALL1 {: id:IR-ID:ir-op-id :}
   CC BB id 2 IR-BUILD:OP-RESULT@ RET1
   CLOSE-FUN ;

\ ---- the routine that ends the process ----------------------------------------
\ `: LEAF ( -- ) s" hello" 70 die ;` in the shape the source compiler hands
\ over (test/compiler/native-trap.f TRAP1): one `hir.trap` whose three operands
\ are the cells `die` reads, the address, the length and the exit code. The
\ address is a plain literal here: a literal's site is the relocatable literal
\ case's. Nothing follows the trap and no block returns.
: BUILD-TRAP ( -- )
   0 0 OPEN-FUN
   $DA7A0000 CONSTOP {: a:IR-ID:ir-value-id :}
   5 CONSTOP {: u:IR-ID:ir-value-id :}
   70 CONSTOP {: rc:IR-ID:ir-value-id :}
   HIR-OPCODE:TRAP CLOSE-ST CLOSE-LN OPEN-OP
   CC BB a IR-BUILD:ADD-OPERAND
   CC BB u IR-BUILD:ADD-OPERAND
   CC BB rc IR-BUILD:ADD-OPERAND
   CC BB IR-BUILD:END-OP drop
   CLOSE-FUN ;

\ ---- the routine that answers another function's address ---------------------
\ `: LEAF ( -- xt ) [: 3000 ;] ;`: a module of two functions, the first
\ answering the address of the second, which `hir.quot` names by its ordinal.
\ Both answer one cell, because the contract is the whole module's.
: QUOT1 ( n -- IR-ID:ir-value-id )
   {: k:n :}
   HIR-OPCODE:QUOT BODY-ST BODY-LN OPEN-OP
   CC BB CELLT IR-BUILD:ADD-RESULT
   CC BB  CC BB HIR:KEY-FUN  CC BB k IR-BUILD:INTERN-INT-ATTR
   IR-BUILD:ADD-ATTR
   CLOSE-VALUE ;

: BUILD-QUOTER ( -- )
   0 1 SIGN OPEN-FUN-SIG
   1 QUOT1 RET1
   CLOSE-FUN
   0 1 SIGN s" SECOND" OPEN-FUN-NAMED
   3000 CONSTOP RET1
   CLOSE-FUN ;

\ ---- the doubles -------------------------------------------------------------
\ A double never crosses a routine's boundary: its arguments and its answer are
\ cells, read as doubles with `bitsreal` and handed back with `realbits` the way
\ the front end reads them, so a case hands a routine bit patterns and checks the
\ bits it answers. A fixture shared by several source operations stages the one
\ F-OP names.
TYPED-VARIABLE F-OP HIR:opcode

: REALT ( -- IR-ID:ir-type-id )
   CC BB HIR:REAL-TYPE ;

: ROP1 ( HIR:opcode IR-ID:ir-value-id IR-ID:ir-type-id -- IR-ID:ir-value-id )
   {: o:HIR:opcode x:IR-ID:ir-value-id t:IR-ID:ir-type-id :}
   o BODY-ST BODY-LN OPEN-OP
   CC BB x IR-BUILD:ADD-OPERAND
   CC BB t IR-BUILD:ADD-RESULT
   CLOSE-VALUE ;

: ROP2 ( HIR:opcode IR-ID:ir-value-id IR-ID:ir-value-id IR-ID:ir-type-id -- IR-ID:ir-value-id )
   {: o:HIR:opcode x:IR-ID:ir-value-id y:IR-ID:ir-value-id t:IR-ID:ir-type-id :}
   o BODY-ST BODY-LN OPEN-OP
   CC BB x IR-BUILD:ADD-OPERAND
   CC BB y IR-BUILD:ADD-OPERAND
   CC BB t IR-BUILD:ADD-RESULT
   CLOSE-VALUE ;

: DOUBLE ( IR-ID:ir-value-id -- IR-ID:ir-value-id )
   {: x:IR-ID:ir-value-id :}
   HIR-OPCODE:BITSREAL x REALT ROP1 ;

: CELL-OF ( IR-ID:ir-value-id -- IR-ID:ir-value-id )
   {: d:IR-ID:ir-value-id :}
   HIR-OPCODE:REALBITS d CELLT ROP1 ;

\ `( a b -- n )` with the two-double operation F-OP names between the readings.
: BUILD-FBIN ( -- )
   2 1 OPEN-FUN
   ARG+ {: a:IR-ID:ir-value-id :}
   ARG+ {: b:IR-ID:ir-value-id :}
   a DOUBLE {: x:IR-ID:ir-value-id :}
   b DOUBLE {: y:IR-ID:ir-value-id :}
   F-OP @ x y REALT ROP2 CELL-OF RET1
   CLOSE-FUN ;

\ `( a b -- n )`, the sum less its left operand. That operand is read after the
\ add that destroys its register, so selection copies it first with the double
\ copy, `x64.movsd` (select-x64.f TIED-OPERAND); no other fixture has the shape.
: BUILD-FKEEP ( -- )
   2 1 OPEN-FUN
   ARG+ {: a:IR-ID:ir-value-id :}
   ARG+ {: b:IR-ID:ir-value-id :}
   a DOUBLE {: x:IR-ID:ir-value-id :}
   b DOUBLE {: y:IR-ID:ir-value-id :}
   HIR-OPCODE:FADD x y REALT ROP2 {: s:IR-ID:ir-value-id :}
   HIR-OPCODE:FSUB s x REALT ROP2 CELL-OF RET1
   CLOSE-FUN ;

\ `( a -- n )` with the one-double operation F-OP names.
: BUILD-FUN1 ( -- )
   1 1 OPEN-FUN
   ARG+ DOUBLE {: x:IR-ID:ir-value-id :}
   F-OP @ x REALT ROP1 CELL-OF RET1
   CLOSE-FUN ;

\ `( a b -- flag )` with the comparison F-OP names.
: BUILD-FCMP ( -- )
   2 1 OPEN-FUN
   ARG+ {: a:IR-ID:ir-value-id :}
   ARG+ {: b:IR-ID:ir-value-id :}
   a DOUBLE {: x:IR-ID:ir-value-id :}
   b DOUBLE {: y:IR-ID:ir-value-id :}
   F-OP @ x y CELLT ROP2 RET1
   CLOSE-FUN ;

\ `( a -- flag )` with the comparison against zero F-OP names.
: BUILD-FCMP0 ( -- )
   1 1 OPEN-FUN
   ARG+ DOUBLE {: x:IR-ID:ir-value-id :}
   F-OP @ x CELLT ROP1 RET1
   CLOSE-FUN ;

\ `: LEAF ( n -- n ) s>f realbits ;`
: BUILD-INTREAL ( -- )
   1 1 OPEN-FUN
   ARG+ {: a:IR-ID:ir-value-id :}
   HIR-OPCODE:INTREAL a REALT ROP1 CELL-OF RET1
   CLOSE-FUN ;

\ `: LEAF ( a -- n ) bitsreal f>s ;`
: BUILD-REALINT ( -- )
   1 1 OPEN-FUN
   ARG+ DOUBLE {: x:IR-ID:ir-value-id :}
   HIR-OPCODE:REALINT x CELLT ROP1 RET1
   CLOSE-FUN ;

\ `: LEAF ( a -- n ) bitsreal realbits ;` - the eight bytes there and back.
: BUILD-BITS ( -- )
   1 1 OPEN-FUN
   ARG+ DOUBLE CELL-OF RET1
   CLOSE-FUN ;

\ -pi: the sign and both halves of the cell set, so a literal moved through
\ fewer than eight bytes answers other bits.
public
$C00921FB54442D18 constant FCONST-BITS
private

: FCONSTOP ( n -- IR-ID:ir-value-id )
   {: bits:n :}
   HIR-OPCODE:FCONST BODY-ST BODY-LN OPEN-OP
   CC BB REALT IR-BUILD:ADD-RESULT
   CC BB  CC BB HIR:KEY-VALUE  CC BB bits IR-BUILD:INTERN-INT-ATTR
   IR-BUILD:ADD-ATTR
   CLOSE-VALUE ;

\ `: LEAF ( a -- n ) -pi realbits + ;`
: BUILD-FCONST ( -- )
   1 1 OPEN-FUN
   ARG+ {: a:IR-ID:ir-value-id :}
   FCONST-BITS FCONSTOP CELL-OF {: k:IR-ID:ir-value-id :}
   HIR-OPCODE:ADD k a BINOP RET1
   CLOSE-FUN ;

\ One call to a one-in one-out callee with nothing carried past it.
: WCALL0 ( IR-ID:ir-value-id IR-ID:ir-value-id n -- IR-ID:ir-value-id )
   {: tok:IR-ID:ir-value-id arg:IR-ID:ir-value-id e:n :}
   HIR-OPCODE:WORDCALL BODY-ST BODY-LN OPEN-OP
   CC BB tok IR-BUILD:ADD-OPERAND
   CC BB arg IR-BUILD:ADD-OPERAND
   CC BB MEMT IR-BUILD:ADD-RESULT
   CC BB CELLT IR-BUILD:ADD-RESULT
   e 1 1 WCALL-ATTRS
   CC BB IR-BUILD:END-OP {: id:IR-ID:ir-op-id :}
   CC BB id 1 IR-BUILD:OP-RESULT@ ;

\ `: LEAF ( a b -- n ) s>f over s>f rot CALLEE -rot swap f- realbits + ;` - two
\ doubles made before the call and subtracted after it. It extends the
\ one-double shape test/compiler/x64-regalloc.f BUILD-FCALL plans into the
\ frame with a second double. Both are live at once, so they take two slots and
\ one of them sits away from rsp; the difference reads each from its own.
: BUILD-FCALL ( n -- )
   {: e:n :}
   2 1 OPEN-FUN
   ARG+ {: a:IR-ID:ir-value-id :}
   ARG+ {: b:IR-ID:ir-value-id :}
   HIR-OPCODE:INTREAL a REALT ROP1 {: x:IR-ID:ir-value-id :}
   HIR-OPCODE:INTREAL b REALT ROP1 {: y:IR-ID:ir-value-id :}
   MEM0 a e WCALL0 {: c:IR-ID:ir-value-id :}
   HIR-OPCODE:FSUB x y REALT ROP2 CELL-OF {: d:IR-ID:ir-value-id :}
   HIR-OPCODE:ADD c d BINOP RET1
   CLOSE-FUN ;

\ ---- the module the machine dialect is written into --------------------------
: X64-BUILDER ( -- IR-BUILD:builder )
   IR-BUILD:PLAN-BEGIN
   IR-BUILD:PLAN-DEFAULT
   CC X64IR:NEW-BUILDER ;

\ ---- two routines built straight in the machine dialect ----------------------
\ The selector produces neither under a contract this machine lowers: a call
\ site is E-X64SEL-CALL under the register convention, because a call hands its
\ arguments over through the data-stack pointer such a routine never took, and
\ no source operation lowers to `x64.neg` at all. Both are therefore staged in
\ X64IR itself - the emitter's input is an X64IR module, and where the module
\ came from is not its question.
1 TYPED-BUFFER M-BLD IR-BUILD:builder
1 TYPED-BUFFER M-SRC IR-ID:ir-source-id

: MB ( -- IR-BUILD:builder )        0 M-BLD @ ;
: MSPN ( -- IR-SOURCE:span )        MB 0 M-SRC @ BODY-ST BODY-LN IR-BUILD:ADD-SPAN ;
: MCELLT ( -- IR-ID:ir-type-id )    CC MB X64IR:GPR-TYPE ;

: M-OPEN ( X64IR:opcode -- )
   {: o:X64IR:opcode :}
   CC MB  CC MB o X64IR:OPCODE  IR-BUILD:BEGIN-OP
   CC MB  MSPN  IR-BUILD:SET-OP-SPAN ;

: M-ATTR+ ( IR-ID:ir-symbol-id IR-ID:ir-attr-id -- )
   {: k:IR-ID:ir-symbol-id v:IR-ID:ir-attr-id :}
   CC MB k v IR-BUILD:ADD-ATTR ;

\ A module's function table admits one function per symbol (E-IR-FUN-DUP,
\ -8051, measured on a second `LEAF` here), so a module of two names them apart.
: M-FUN-NAMED ( IR-ID:ir-type-id ptr u8 n -- )
   {: sig:IR-ID:ir-type-id a:ptr u:n :}
   CC MB  CC MB a u IR-BUILD:INTERN-SYMBOL  IR-BUILD:BEGIN-FUN
   CC MB  sig  IR-BUILD:SET-SIGNATURE
   CC MB IR--FUN-LINKAGE:DEFINED IR-BUILD:SET-LINKAGE
   CC MB IR--FUN-VISIBILITY:EXPORTED IR-BUILD:SET-VISIBILITY
   CC MB IR--FUN-CONVENTION:HABU IR-BUILD:SET-CONVENTION
   CC MB  MSPN  IR-BUILD:SET-FUN-SPAN
   CC MB IR-BUILD:BEGIN-BLOCK
   CC MB  MSPN  IR-BUILD:SET-BLOCK-SPAN ;

: M-FUN ( IR-ID:ir-type-id -- )
   s" LEAF" M-FUN-NAMED ;

\ Close a function that is not the module's last, so another `M-FUN` can open the
\ next one; `M-CLOSE` is this and the freeze.
: M-FUN-END ( -- )
   CC MB IR-BUILD:END-BLOCK drop
   CC MB IR-BUILD:END-FUN drop ;

: M-CLOSE ( -- IR-BUILD:module )
   M-FUN-END
   CC MB IR-BUILD:FREEZE ;

: M-MOD ( -- )
   X64-BUILDER {: b:IR-BUILD:builder :}
   b 0 M-BLD !
   CC b TXT TXT-N IR-BUILD:ADD-SOURCE 0 M-SRC !
   CC b X64IR:REGISTER
   CC MB X64EMIT:BIND-DIALECT ;

\ `( a -- n )` negated: a tied unary form of this dialect that this slice does
\ not write, in a routine whose shape it accepts.
: NEG-SIGN ( -- IR-ID:ir-type-id )
   IR-TYPE:FN-BEGIN
   MCELLT IR-TYPE:FN-PARAM
   MCELLT IR-TYPE:FN-RESULT
   CC MB IR-BUILD:INTERN-CODE-REF ;

: BUILD-NEGATOR ( -- IR-BUILD:module )
   M-MOD
   CC MB X64M:MACHINE  CC MB X64IR:VOCABULARY  A64RA:BIND-DIALECT
   CC MB  CC MB X64IR:VOCABULARY  A64RAV:BIND-DIALECT
   NEG-SIGN M-FUN
   CC MB MCELLT IR-BUILD:ADD-BLOCK-ARG {: a:IR-ID:ir-value-id :}
   X64IR-OPCODE:NEG M-OPEN
   CC MB a IR-BUILD:ADD-OPERAND
   CC MB MCELLT IR-BUILD:ADD-RESULT
   CC MB IR-BUILD:END-OP {: id:IR-ID:ir-op-id :}
   CC MB id 0 IR-BUILD:OP-RESULT@ {: v:IR-ID:ir-value-id :}
   X64IR-OPCODE:RET M-OPEN
   CC MB v IR-BUILD:ADD-OPERAND
   CC MB IR-BUILD:END-OP drop
   M-CLOSE ;

\ ---- the machine-dialect shapes the selector does not produce ----------------
\ The selector is CORRECT AND UNFUSED (select-x64.f's own header): a comparison
\ feeding a branch becomes `x64.cmpset` and then `x64.brz`, never `x64.cmpbr`,
\ and it lowers a self-call from HIR `call` only inside a definition that
\ recurses. Both forms are the dialect's and this emitter writes both, so they
\ are staged here the way the negator above is - the emitter's input is an X64IR
\ module and where it came from is not its question.
: M-BIND-MACHINE ( -- )
   CC MB X64M:MACHINE  CC MB X64IR:VOCABULARY  A64RA:BIND-DIALECT
   CC MB  CC MB X64IR:VOCABULARY  A64RAV:BIND-DIALECT ;

: M-RESULT+ ( -- )       CC MB MCELLT IR-BUILD:ADD-RESULT ;
: M-ARG+ ( -- IR-ID:ir-value-id )      CC MB MCELLT IR-BUILD:ADD-BLOCK-ARG ;

: M-OPERAND+ ( IR-ID:ir-value-id -- )
   {: v:IR-ID:ir-value-id :}
   CC MB v IR-BUILD:ADD-OPERAND ;

: M-BLOCK-ID ( n -- IR-ID:ir-block-id )
   {: k:n :}
   MB IR-BUILD:MODULE-KEY k IR-ID:PACK-BLOCK ;

: M-SUCC+ ( n -- )
   {: k:n :}
   CC MB k M-BLOCK-ID IR-BUILD:ADD-SUCCESSOR ;

: M-BLOCK+ ( -- )
   CC MB IR-BUILD:END-BLOCK drop
   CC MB IR-BUILD:BEGIN-BLOCK
   CC MB  MSPN  IR-BUILD:SET-BLOCK-SPAN ;

: M-MOVI ( n n -- IR-ID:ir-value-id )
   {: imm:n kind:n :}
   X64IR-OPCODE:MOVI M-OPEN
   M-RESULT+
   CC MB X64IR:KEY-IMM   CC MB imm X64IR:IMM-ATTR   M-ATTR+
   CC MB X64IR:KEY-ADDR  CC MB kind X64IR:ADDR-ATTR M-ATTR+
   CC MB X64IR:KEY-REMAT-MARK
   CC MB 0 IR-BUILD:INTERN-INT-ATTR M-ATTR+
   CC MB IR-BUILD:END-OP {: id:IR-ID:ir-op-id :}
   CC MB id 0 IR-BUILD:OP-RESULT@ ;

: M-RET1 ( IR-ID:ir-value-id -- )
   X64IR-OPCODE:RET M-OPEN
   M-OPERAND+
   CC MB IR-BUILD:END-OP drop ;

: BINARY-SIGN ( -- IR-ID:ir-type-id )
   IR-TYPE:FN-BEGIN
   MCELLT IR-TYPE:FN-PARAM
   MCELLT IR-TYPE:FN-PARAM
   MCELLT IR-TYPE:FN-RESULT
   CC MB IR-BUILD:INTERN-CODE-REF ;

: NULLARY-SIGN ( -- IR-ID:ir-type-id )
   IR-TYPE:FN-BEGIN
   MCELLT IR-TYPE:FN-RESULT
   CC MB IR-BUILD:INTERN-CODE-REF ;

\ `( a b -- n )` as a diamond with a join: the comparison and the branch are one
\ operation, each arm materialises its own literal, and the block they both
\ branch to takes the answer as its argument. FOUR blocks, and every one of them
\ is written: neither arm is a block control only passes through.
\
\ THE BODY IS THE MODULE'S FIRST FUNCTION WHEREVER IT IS USED: `M-SUCC+` names a
\ successor by the block's ordinal IN THE MODULE, so the 1, 2 and 3 below are
\ this function's own blocks only while its blocks are the module's first four.
: DIAMOND-BODY ( -- )
   M-ARG+ {: a:IR-ID:ir-value-id :}
   M-ARG+ {: b:IR-ID:ir-value-id :}
   X64IR-OPCODE:CMPBR M-OPEN
   a M-OPERAND+
   b M-OPERAND+
   CC MB X64IR:KEY-COND  CC MB X64IR-COND:LT X64IR:COND-ATTR  M-ATTR+
   1 M-SUCC+
   2 M-SUCC+
   CC MB IR-BUILD:END-OP drop
   M-BLOCK+
   1000 X64IR:ADDR-NONE M-MOVI {: lo:IR-ID:ir-value-id :}
   X64IR-OPCODE:BR M-OPEN
   lo M-OPERAND+
   3 M-SUCC+
   CC MB IR-BUILD:END-OP drop
   M-BLOCK+
   2000 X64IR:ADDR-NONE M-MOVI {: hi:IR-ID:ir-value-id :}
   X64IR-OPCODE:BR M-OPEN
   hi M-OPERAND+
   3 M-SUCC+
   CC MB IR-BUILD:END-OP drop
   M-BLOCK+
   M-ARG+ {: v:IR-ID:ir-value-id :}
   v M-RET1 ;

: BUILD-DIAMOND ( -- IR-BUILD:module )
   M-MOD
   M-BIND-MACHINE
   BINARY-SIGN M-FUN
   DIAMOND-BODY
   M-CLOSE ;

\ TWO FUNCTIONS IN ONE MODULE, which is what says the writer lays each function
\ out again before it writes it. `B-START` is indexed by a block's ordinal IN ITS
\ FUNCTION, so the numbers the measuring pass leaves there are the LAST
\ function's; a writer that walked function zero against them would be holding it
\ to another routine's starts (emit-x64.f WRITE-ALL).
: BUILD-PAIR ( -- IR-BUILD:module )
   M-MOD
   M-BIND-MACHINE
   BINARY-SIGN M-FUN
   DIAMOND-BODY
   M-FUN-END
   NULLARY-SIGN s" SECOND" M-FUN-NAMED
   3000 X64IR:ADDR-NONE M-MOVI M-RET1
   M-CLOSE ;

\ `( -- n )` materialising an address the relocation pass has to find again: one
\ `mov r64, imm64` whose `x64.addr` says which kind of address its immediate is.
: BUILD-RELOC ( n -- IR-BUILD:module )
   {: kind:n :}
   M-MOD
   M-BIND-MACHINE
   NULLARY-SIGN M-FUN
   $DA7A0000 kind M-MOVI M-RET1
   M-CLOSE ;

\ ---- the four frame forms ----------------------------------------------------
\ `( a -- n )` in a frame of its own: `a` put away in two slots, brought back
\ from both and summed, so the answer is `2 a *`. A slot is a displacement from
\ rsp, and rsp as a base always takes a SIB byte: slot 0 has no displacement
\ field and slot 8 a disp8. The spill pass's own frames are
\ test/compiler/x64-chain.f's pressure fixtures, run natively by
\ test/x86-64-peer-routines.f.
16 constant FRAME-N                  \ two slots, a whole multiple of X64IR:SP-ALIGN

: MMEMT ( -- IR-ID:ir-type-id )     CC MB X64IR:MEM-TYPE ;

: M-CLOSE-VALUE ( -- IR-ID:ir-value-id )
   CC MB IR-BUILD:END-OP {: id:IR-ID:ir-op-id :}
   CC MB id 0 IR-BUILD:OP-RESULT@ ;

: M-RESERVE ( n -- IR-ID:ir-value-id )
   {: size:n :}
   X64IR-OPCODE:RESERVE M-OPEN
   CC MB MMEMT IR-BUILD:ADD-RESULT
   CC MB X64IR:KEY-FRAME  CC MB size X64IR:FRAME-ATTR  M-ATTR+
   M-CLOSE-VALUE ;

: M-STORE ( IR-ID:ir-value-id IR-ID:ir-value-id n -- IR-ID:ir-value-id )
   {: v:IR-ID:ir-value-id k:IR-ID:ir-value-id slot:n :}
   X64IR-OPCODE:STORE M-OPEN
   v M-OPERAND+
   k M-OPERAND+
   CC MB MMEMT IR-BUILD:ADD-RESULT
   CC MB X64IR:KEY-SLOT  CC MB slot X64IR:SLOT-ATTR  M-ATTR+
   M-CLOSE-VALUE ;

: M-LOAD ( IR-ID:ir-value-id n -- IR-ID:ir-value-id IR-ID:ir-value-id )
   {: k:IR-ID:ir-value-id slot:n :}
   X64IR-OPCODE:LOAD M-OPEN
   k M-OPERAND+
   M-RESULT+
   CC MB MMEMT IR-BUILD:ADD-RESULT
   CC MB X64IR:KEY-SLOT  CC MB slot X64IR:SLOT-ATTR  M-ATTR+
   CC MB IR-BUILD:END-OP {: id:IR-ID:ir-op-id :}
   CC MB id 0 IR-BUILD:OP-RESULT@
   CC MB id 1 IR-BUILD:OP-RESULT@ ;

: M-RELEASE ( IR-ID:ir-value-id n -- )
   {: k:IR-ID:ir-value-id size:n :}
   X64IR-OPCODE:RELEASE M-OPEN
   k M-OPERAND+
   CC MB X64IR:KEY-FRAME  CC MB size X64IR:FRAME-ATTR  M-ATTR+
   CC MB IR-BUILD:END-OP drop ;

: M-ADD ( IR-ID:ir-value-id IR-ID:ir-value-id -- IR-ID:ir-value-id )
   {: x:IR-ID:ir-value-id y:IR-ID:ir-value-id :}
   X64IR-OPCODE:ADD M-OPEN
   x M-OPERAND+
   y M-OPERAND+
   M-RESULT+
   M-CLOSE-VALUE ;

: BUILD-FRAMED ( -- IR-BUILD:module )
   M-MOD
   M-BIND-MACHINE
   NEG-SIGN M-FUN
   M-ARG+ {: a:IR-ID:ir-value-id :}
   FRAME-N M-RESERVE {: k0:IR-ID:ir-value-id :}
   a k0 0 M-STORE {: k1:IR-ID:ir-value-id :}
   a k1 8 M-STORE {: k2:IR-ID:ir-value-id :}
   k2 0 M-LOAD {: x:IR-ID:ir-value-id k3:IR-ID:ir-value-id :}
   k3 8 M-LOAD {: y:IR-ID:ir-value-id k4:IR-ID:ir-value-id :}
   x y M-ADD {: u:IR-ID:ir-value-id :}
   k4 FRAME-N M-RELEASE
   u M-RET1
   M-CLOSE ;

\ ---- the divide, staged in the dialect ---------------------------------------
\ The selector reads only the quotient and names THIS engine's `throw` as the
\ cold side's entry (select-x64.f EMIT-DIV), so the remainder, and a divide
\ whose cold side calls an entry an image carries, are staged here. Every
\ routine keeps the data-stack boundary the selector writes: the pointer taken
\ over the cells, each one loaded, the answer stored and published, and a
\ return with no operand.
: TERNARY-SIGN ( -- IR-ID:ir-type-id )
   IR-TYPE:FN-BEGIN
   MCELLT IR-TYPE:FN-PARAM
   MCELLT IR-TYPE:FN-PARAM
   MCELLT IR-TYPE:FN-PARAM
   MCELLT IR-TYPE:FN-RESULT
   CC MB IR-BUILD:INTERN-CODE-REF ;

: M-DTAKE ( n -- IR-ID:ir-value-id )
   {: d:n :}
   X64IR-OPCODE:DTAKE M-OPEN
   CC MB MMEMT IR-BUILD:ADD-RESULT
   CC MB X64IR:KEY-DBYTES  CC MB d X64IR:DBYTES-ATTR  M-ATTR+
   M-CLOSE-VALUE ;

: M-DLOAD ( IR-ID:ir-value-id n -- IR-ID:ir-value-id IR-ID:ir-value-id )
   {: k:IR-ID:ir-value-id off:n :}
   X64IR-OPCODE:DLOAD M-OPEN
   k M-OPERAND+
   M-RESULT+
   CC MB MMEMT IR-BUILD:ADD-RESULT
   CC MB X64IR:KEY-DSLOT  CC MB off X64IR:DSLOT-ATTR  M-ATTR+
   CC MB IR-BUILD:END-OP {: id:IR-ID:ir-op-id :}
   CC MB id 0 IR-BUILD:OP-RESULT@
   CC MB id 1 IR-BUILD:OP-RESULT@ ;

: M-DSTORE ( IR-ID:ir-value-id IR-ID:ir-value-id n -- IR-ID:ir-value-id )
   {: v:IR-ID:ir-value-id k:IR-ID:ir-value-id off:n :}
   X64IR-OPCODE:DSTORE M-OPEN
   v M-OPERAND+
   k M-OPERAND+
   CC MB MMEMT IR-BUILD:ADD-RESULT
   CC MB X64IR:KEY-DSLOT  CC MB off X64IR:DSLOT-ATTR  M-ATTR+
   M-CLOSE-VALUE ;

: M-DPUBLISH ( IR-ID:ir-value-id n -- )
   {: k:IR-ID:ir-value-id d:n :}
   X64IR-OPCODE:DPUBLISH M-OPEN
   k M-OPERAND+
   CC MB X64IR:KEY-DBYTES  CC MB d X64IR:DBYTES-ATTR  M-ATTR+
   CC MB IR-BUILD:END-OP drop ;

: M-RET0 ( -- )
   X64IR-OPCODE:RET M-OPEN
   CC MB IR-BUILD:END-OP drop ;

\ The divide of `x` by `y`, its cold side calling `entry`: the quotient and the
\ remainder, in that order.
: M-IDIV ( IR-ID:ir-value-id IR-ID:ir-value-id n -- IR-ID:ir-value-id IR-ID:ir-value-id )
   {: x:IR-ID:ir-value-id y:IR-ID:ir-value-id entry:n :}
   X64IR-OPCODE:IDIV M-OPEN
   x M-OPERAND+
   y M-OPERAND+
   M-RESULT+
   M-RESULT+
   CC MB X64IR:KEY-THROW-ENTRY  CC MB entry X64IR:ENTRY-ATTR  M-ATTR+
   CC MB IR-BUILD:END-OP {: id:IR-ID:ir-op-id :}
   CC MB id 0 IR-BUILD:OP-RESULT@
   CC MB id 1 IR-BUILD:OP-RESULT@ ;

\ `( k a b -- n )` answering `a b mod k +`, with k live across the divide: the
\ shape of test/compiler/x64-regalloc.f EARLY-CLOBBER-BODY. k is loaded first
\ and kept out of rax and rdx, which the form writes, so it takes rcx; rdx is
\ then the lowest register left for the divisor b, and b is not given it,
\ because `cqo` writes rdx before `idiv` reads its divisor. Divided by an rdx
\ holding b's sign, a positive a would be divided by zero and a negative one by
\ minus one.
: BUILD-REMAINDER ( n -- IR-BUILD:module )
   {: entry:n :}
   M-MOD
   M-BIND-MACHINE
   TERNARY-SIGN M-FUN
   24 M-DTAKE {: t0:IR-ID:ir-value-id :}
   t0 0 M-DLOAD {: k:IR-ID:ir-value-id t1:IR-ID:ir-value-id :}
   t1 8 M-DLOAD {: a:IR-ID:ir-value-id t2:IR-ID:ir-value-id :}
   t2 16 M-DLOAD {: b:IR-ID:ir-value-id t3:IR-ID:ir-value-id :}
   a b entry M-IDIV nip {: r:IR-ID:ir-value-id :}
   r k M-ADD {: u:IR-ID:ir-value-id :}
   u t3 0 M-DSTORE {: t4:IR-ID:ir-value-id :}
   t4 8 M-DPUBLISH
   M-RET0
   M-CLOSE ;

\ `( a b -- n )` answering `a b /`: the selected divide less the dividend's
\ copy, which the selector makes because its source may be read afterwards and
\ nothing reads `a` here.
: BUILD-QUOTIENT ( n -- IR-BUILD:module )
   {: entry:n :}
   M-MOD
   M-BIND-MACHINE
   BINARY-SIGN M-FUN
   16 M-DTAKE {: t0:IR-ID:ir-value-id :}
   t0 0 M-DLOAD {: a:IR-ID:ir-value-id t1:IR-ID:ir-value-id :}
   t1 8 M-DLOAD {: b:IR-ID:ir-value-id t2:IR-ID:ir-value-id :}
   a b entry M-IDIV drop {: q:IR-ID:ir-value-id :}
   q t2 0 M-DSTORE {: t3:IR-ID:ir-value-id :}
   t3 8 M-DPUBLISH
   M-RET0
   M-CLOSE ;

\ `( -- n ) 7 0 mod`: control leaves through the cold side, and no cell was
\ taken, so the code lands in the cell above the caller's.
: BUILD-DIVZERO ( n -- IR-BUILD:module )
   {: entry:n :}
   M-MOD
   M-BIND-MACHINE
   NULLARY-SIGN M-FUN
   0 M-DTAKE {: t0:IR-ID:ir-value-id :}
   7 X64IR:ADDR-NONE M-MOVI {: a:IR-ID:ir-value-id :}
   0 X64IR:ADDR-NONE M-MOVI {: b:IR-ID:ir-value-id :}
   a b entry M-IDIV nip {: r:IR-ID:ir-value-id :}
   r t0 0 M-DSTORE {: t1:IR-ID:ir-value-id :}
   t1 8 M-DPUBLISH
   M-RET0
   M-CLOSE ;

\ ---- the two selects, staged in the dialect ----------------------------------
\ Nothing selects `x64.cmpsel` or `x64.selz` yet (select-x64.f's header), so each
\ is staged here under the data-stack boundary the divide keeps. The allocator
\ picks the registers, so an aliasing case is an IR identity: one value standing
\ in two operands is one register in both. SEL-TAKE leaves the memory order under
\ the loaded cells, and each shape picks its operands off the top with a stack
\ word.
: SEL-TAKE2 ( -- IR-ID:ir-value-id IR-ID:ir-value-id IR-ID:ir-value-id )
   M-MOD
   M-BIND-MACHINE
   BINARY-SIGN M-FUN
   16 M-DTAKE {: t0:IR-ID:ir-value-id :}
   t0 0 M-DLOAD {: a:IR-ID:ir-value-id t1:IR-ID:ir-value-id :}
   t1 8 M-DLOAD {: b:IR-ID:ir-value-id t2:IR-ID:ir-value-id :}
   t2 a b ;

: SEL-TAKE3 ( -- IR-ID:ir-value-id IR-ID:ir-value-id IR-ID:ir-value-id IR-ID:ir-value-id )
   M-MOD
   M-BIND-MACHINE
   TERNARY-SIGN M-FUN
   24 M-DTAKE {: t0:IR-ID:ir-value-id :}
   t0 0 M-DLOAD {: a:IR-ID:ir-value-id t1:IR-ID:ir-value-id :}
   t1 8 M-DLOAD {: b:IR-ID:ir-value-id t2:IR-ID:ir-value-id :}
   t2 16 M-DLOAD {: c:IR-ID:ir-value-id t3:IR-ID:ir-value-id :}
   t3 a b c ;

\ The answer stored and published, under the order the cells were taken with.
: SEL-ANSWER ( IR-ID:ir-value-id IR-ID:ir-value-id -- IR-BUILD:module )
   {: t:IR-ID:ir-value-id r:IR-ID:ir-value-id :}
   r t 0 M-DSTORE {: t1:IR-ID:ir-value-id :}
   t1 8 M-DPUBLISH
   M-RET0
   M-CLOSE ;

\ Operands 0 and 1 compared signed less-than; operand 2 the answer when that
\ fails and operand 3 when it holds (x64ir.f DEF-CMPSEL).
: M-CMPSEL ( IR-ID:ir-value-id IR-ID:ir-value-id IR-ID:ir-value-id IR-ID:ir-value-id -- IR-ID:ir-value-id )
   {: a:IR-ID:ir-value-id b:IR-ID:ir-value-id
      f:IR-ID:ir-value-id h:IR-ID:ir-value-id :}
   X64IR-OPCODE:CMPSEL M-OPEN
   a M-OPERAND+
   b M-OPERAND+
   f M-OPERAND+
   h M-OPERAND+
   M-RESULT+
   CC MB X64IR:KEY-COND  CC MB X64IR-COND:LT X64IR:COND-ATTR  M-ATTR+
   M-CLOSE-VALUE ;

\ Operand 0 tested against zero; operand 1 the answer when it is not zero and
\ operand 2 when it is (x64ir.f DEF-SELZ).
: M-SELZ ( IR-ID:ir-value-id IR-ID:ir-value-id IR-ID:ir-value-id -- IR-ID:ir-value-id )
   {: v:IR-ID:ir-value-id nz:IR-ID:ir-value-id z:IR-ID:ir-value-id :}
   X64IR-OPCODE:SELZ M-OPEN
   v M-OPERAND+
   nz M-OPERAND+
   z M-OPERAND+
   M-RESULT+
   M-CLOSE-VALUE ;

\ A fourth value for the general cmpsel, which three cells cannot hold apart.
public
1000 constant SEL-LIT
private

\ `( a b x -- n )`: `a b < if 1000 else x then`, four values in four registers.
: BUILD-CMPSEL ( -- IR-BUILD:module )
   SEL-TAKE3  SEL-LIT X64IR:ADDR-NONE M-MOVI  M-CMPSEL SEL-ANSWER ;

\ `( a b y -- n )`, operand 2 = operand 0: the result is the compared register,
\ and `a` must survive the compare to be the answer when it fails.
: BUILD-CMPSEL-FA ( -- IR-BUILD:module )
   SEL-TAKE3  >r over r>  M-CMPSEL SEL-ANSWER ;

\ `( a b x -- n )`, operand 3 = operand 1: `b` must survive the compare to be
\ moved in.
: BUILD-CMPSEL-HB ( -- IR-BUILD:module )
   SEL-TAKE3  over  M-CMPSEL SEL-ANSWER ;

\ `( a b x -- n )`, operand 3 = operand 0: `a` must survive the compare to be
\ moved in.
: BUILD-CMPSEL-HA ( -- IR-BUILD:module )
   SEL-TAKE3  >r over r> swap  M-CMPSEL SEL-ANSWER ;

\ `( a b x -- n )`, operand 2 = operand 3: both sources one register, so the
\ answer is `x` whichever way the compare goes.
: BUILD-CMPSEL-SAME ( -- IR-BUILD:module )
   SEL-TAKE3  dup  M-CMPSEL SEL-ANSWER ;

\ `( v x y -- n )`: `v if x else y then`, three values in three registers.
: BUILD-SELZ ( -- IR-BUILD:module )
   SEL-TAKE3  M-SELZ SEL-ANSWER ;

\ `( v y -- n )`, operand 1 = operand 0: the result is the tested register, and
\ `v` must survive the test to be the answer when it is not zero.
: BUILD-SELZ-NV ( -- IR-BUILD:module )
   SEL-TAKE2  >r dup r>  M-SELZ SEL-ANSWER ;

\ `( v x -- n )`, operand 2 = operand 0: `v` must survive the test to be moved
\ in, which is zero.
: BUILD-SELZ-ZV ( -- IR-BUILD:module )
   SEL-TAKE2  over  M-SELZ SEL-ANSWER ;

\ `( v x -- n )`, operand 1 = operand 2: both sources one register.
: BUILD-SELZ-SAME ( -- IR-BUILD:module )
   SEL-TAKE2  dup  M-SELZ SEL-ANSWER ;

\ ---- the double comparison the selector never mints --------------------------
\ `x64.fcmpset` carries its scratch as a variadic result tail (x64ir.f
\ DEF-FCMPSET), so nothing before the emitter ties the count to the condition: a
\ module whose count is wrong for its condition, or whose condition is not one
\ of the two ucomisd's flags answer, freezes, allocates and is accepted. Each is
\ staged here as `( a b -- flag )` under the data-stack boundary, with `k`
\ results.
: M-XMM ( IR-ID:ir-value-id -- IR-ID:ir-value-id )
   {: g:IR-ID:ir-value-id :}
   X64IR-OPCODE:MOVQ-XR M-OPEN
   g M-OPERAND+
   CC MB  CC MB X64IR:FPR-TYPE  IR-BUILD:ADD-RESULT
   M-CLOSE-VALUE ;

: BUILD-BADFCMP ( X64IR:cond n -- IR-BUILD:module )
   {: c:X64IR:cond k:n :}
   M-MOD
   M-BIND-MACHINE
   BINARY-SIGN M-FUN
   16 M-DTAKE {: t0:IR-ID:ir-value-id :}
   t0 0 M-DLOAD {: a:IR-ID:ir-value-id t1:IR-ID:ir-value-id :}
   t1 8 M-DLOAD {: b:IR-ID:ir-value-id t2:IR-ID:ir-value-id :}
   a M-XMM {: x:IR-ID:ir-value-id :}
   b M-XMM {: y:IR-ID:ir-value-id :}
   X64IR-OPCODE:FCMPSET M-OPEN
   x M-OPERAND+
   y M-OPERAND+
   k 0 ?do M-RESULT+ loop
   CC MB X64IR:KEY-COND  CC MB c X64IR:COND-ATTR  M-ATTR+
   M-CLOSE-VALUE {: f:IR-ID:ir-value-id :}
   f t2 0 M-DSTORE {: t3:IR-ID:ir-value-id :}
   t3 8 M-DPUBLISH
   M-RET0
   M-CLOSE ;

\ ---- running selection, allocation, validation and emission ------------------
\ The contract every case emits under: a leaf computing in this machine's nine
\ allocatable general registers, returning to its caller, reserving the frame it
\ is given - none, but for the frame forms' case - and calling nothing.
: LEAF-FRAMED ( n -- NEFF:routine )
   {: frame:n :}
   NEFF-CONV:REGISTER NEFF:SEQ-NONE NEFF:SEQ-NONE X64ABI:SCRATCH
   NEFF:FPR-NONE NEFF:FPR-NONE NEFF:FPR-NONE
   NEFF-NZCV:CLOBBERED NEFF-LINK:ABSENT NEFF-CONTROL:RETURNS
   NEFF:TRAITS-NONE frame 0 X64M:MACHINE NEFF:ROUTINE ;

: LEAF ( -- NEFF:routine )
   0 LEAF-FRAMED ;

\ The emitter is bound to the module about to be written at the same moment the
\ allocator and the validator are: a module's opcode and key identities are its
\ own ordinals, so all three passes take them from it once.
: SELECTED ( -- IR-BUILD:module )
   CC BB HIR:ENSURE-VOCABULARY
   CC BB IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m X64SEL:BIND-SOURCE
   X64-BUILDER {: xb:IR-BUILD:builder :}
   CC xb X64M:MACHINE  CC xb X64IR:VOCABULARY  A64RA:BIND-DIALECT
   CC xb  CC xb X64IR:VOCABULARY  A64RAV:BIND-DIALECT
   CC xb X64EMIT:BIND-DIALECT
   CC m xb LEAF X64SEL:SELECT ;

: ALLOCATED ( -- IR-BUILD:module )
   SELECTED {: m:IR-BUILD:module :}
   CC m LEAF A64RA:ALLOCATE
   m LEAF A64RAV:ACCEPT
   m ;

\ AT SLOT ZERO, which is the placement every case of this file emits at unless
\ it is about the placement itself or calls one of this engine's entries
\ (SLOT-BELOW): a displacement to another word's entry is measured from it, and
\ zero is the one that leaves the entry's own number in the bytes.
: PLACED ( IR-BUILD:module n -- )
   {: m:IR-BUILD:module at:n :}
   at X64EMIT:PLACE-AT
   CC m X64EMIT:EMIT ;

: EMITTED ( -- )
   ALLOCATED 0 PLACED ;

\ ---- and the same passes under the data-stack convention ---------------------
\ The other contract of this machine, the one test/compiler/x64-regalloc.f
\ allocates and the validator accepts: the interface is caller cells in and out,
\ so the selector writes the boundary itself. Nothing arrives in a register -
\ the entry takes the pointer and loads every cell the contract lists, and the
\ exit stores every result and publishes. The first case through here is the
\ subtraction the first case of the file pins under the other contract, so what
\ its longer byte string adds is the boundary and nothing else.
: DLEAF ( n n -- NEFF:routine )
   {: in:n out:n :}
   X64ABI:SCRATCH in out X64ABI:LEAF ;

: DSTACK-SELECTED ( n n -- IR-BUILD:module )
   {: in:n out:n :}
   CC BB HIR:ENSURE-VOCABULARY
   CC BB IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m X64SEL:BIND-SOURCE
   X64-BUILDER {: xb:IR-BUILD:builder :}
   CC xb X64M:MACHINE  CC xb X64IR:VOCABULARY  A64RA:BIND-DIALECT
   CC xb  CC xb X64IR:VOCABULARY  A64RAV:BIND-DIALECT
   CC xb X64EMIT:BIND-DIALECT
   CC m xb in out DLEAF X64SEL:SELECT ;

: DSTACK-ALLOCATED ( n n -- IR-BUILD:module )
   {: in:n out:n :}
   in out DSTACK-SELECTED {: m:IR-BUILD:module :}
   CC m in out DLEAF A64RA:ALLOCATE
   m in out DLEAF A64RAV:ACCEPT
   m ;

: DSTACK-EMITTED ( n n -- )
   DSTACK-ALLOCATED 0 PLACED ;

\ ---- and the contract a routine that CALLS is compiled under -----------------
\ One cell in and one out, reached by a call and coming back from one: the
\ selector writes the boundary and the call site's own save and restore, which
\ is the only way a `x64.call` or `x64.wordcall` reaches this emitter at all -
\ a register-convention leaf has no data stack to hand a callee its arguments on
\ (E-X64SEL-CALL), and a module built straight in the dialect leaves the memory
\ order its call site mints unread (E-A64RAV-ORDER).
: DCALL ( -- NEFF:routine )
   X64ABI:SCRATCH 1 1 X64ABI:CALL ;

\ Selected, allocated and accepted under the one contract given.
: UNDER ( NEFF:routine -- IR-BUILD:module )
   {: r :}
   CC BB HIR:ENSURE-VOCABULARY
   CC BB IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m X64SEL:BIND-SOURCE
   X64-BUILDER {: xb:IR-BUILD:builder :}
   CC xb X64M:MACHINE  CC xb X64IR:VOCABULARY  A64RA:BIND-DIALECT
   CC xb  CC xb X64IR:VOCABULARY  A64RAV:BIND-DIALECT
   CC xb X64EMIT:BIND-DIALECT
   CC m xb r X64SEL:SELECT {: sel:IR-BUILD:module :}
   CC sel r A64RA:ALLOCATE
   sel r A64RAV:ACCEPT
   sel ;

: CALL-ALLOCATED ( -- IR-BUILD:module )
   DCALL UNDER ;

\ ---- and the contract a routine leaves through its callee under --------------
\ One cell in and one out, control leaving through the callee: the pointer never
\ moves, so every cell the callee reads is a cell this routine was entered with.
\ test/compiler/x64-regalloc.f allocates the same shape under it.
: DTAIL ( -- NEFF:routine )
   X64ABI:SCRATCH 1 1 X64ABI:TAIL ;

: TAIL-ALLOCATED ( -- IR-BUILD:module )
   DTAIL UNDER ;

\ ---- and the contract of a routine that ends the process ---------------------
\ Nothing in or out, no return anywhere and no call it comes back from: the
\ contract src/arch/x86-64/passes.f ROUTINE declares for a definition that ends
\ in `die` and calls nothing else.
: DTRAP ( -- NEFF:routine )
   X64ABI:SCRATCH 0 0 0 X64ABI:NORET-LEAF-FRAMED ;

\ A form outside this emitter's renders is refused only once the shape and the
\ assignment have been agreed, so this module is allocated and accepted like any
\ other. The allocator and the validator are bound inside BUILD-NEGATOR, because
\ a binding reads the BUILDER and a frozen builder handle answers nothing.
: NEGATOR-EMIT ( -- )
   BUILD-NEGATOR {: m:IR-BUILD:module :}
   CC m LEAF A64RA:ALLOCATE
   m LEAF A64RAV:ACCEPT
   m 0 PLACED ;

\ ---- the cases ---------------------------------------------------------------
: DIFF-BYTES ( IR-CTX:ctx -- )     HIR-MOD BUILD-DIFF EMITTED ;
: SQUARE-BYTES ( IR-CTX:ctx -- )   HIR-MOD BUILD-SQUARE EMITTED ;
: CHAIN-BYTES ( IR-CTX:ctx -- )    HIR-MOD BUILD-CHAIN EMITTED ;
: IMMS-BYTES ( IR-CTX:ctx -- )     HIR-MOD BUILD-IMMS EMITTED ;
: SHIFTS-BYTES ( IR-CTX:ctx -- )   HIR-MOD BUILD-SHIFTS EMITTED ;
: SHL-BYTES ( IR-CTX:ctx -- )      HIR-MOD BUILD-SHL EMITTED ;
: SHR-BYTES ( IR-CTX:ctx -- )      HIR-MOD BUILD-SHR EMITTED ;
: SHL-ADD-BYTES ( IR-CTX:ctx -- )  HIR-MOD BUILD-SHL-ADD EMITTED ;
: SHL-CROSS-BYTES ( IR-CTX:ctx -- ) HIR-MOD BUILD-SHL-CROSS EMITTED ;
: NOT-BYTES ( IR-CTX:ctx -- )      HIR-MOD BUILD-NOT EMITTED ;
: CMPSET-BYTES ( IR-CTX:ctx -- )   HIR-MOD BUILD-CMPSET EMITTED ;
: CMPSETI-BYTES ( IR-CTX:ctx -- )  HIR-MOD BUILD-CMPSETI EMITTED ;
: MOVI-BYTES ( IR-CTX:ctx -- )     HIR-MOD BUILD-MOVI EMITTED ;

: DDIFF-BYTES ( IR-CTX:ctx -- )    HIR-MOD BUILD-DIFF 2 1 DSTACK-EMITTED ;
: DADDR-BYTES ( IR-CTX:ctx -- )    HIR-MOD BUILD-DADDRESSED 1 1 DSTACK-EMITTED ;
: DLOOP-BYTES ( IR-CTX:ctx -- )    HIR-MOD BUILD-LOOP 2 1 DSTACK-EMITTED ;
: DSUM-SHL-BYTES ( IR-CTX:ctx -- )
   HIR-MOD HIR-OPCODE:LSHIFT BUILD-SUM-SHIFT 3 1 DSTACK-EMITTED ;
: DSUM-SHR-BYTES ( IR-CTX:ctx -- )
   HIR-MOD HIR-OPCODE:RSHIFT BUILD-SUM-SHIFT 3 1 DSTACK-EMITTED ;

: TAIL-BYTES ( IR-CTX:ctx -- )
   HIR-MOD CALLEE-ENTRY BUILD-WORDCALLER TAIL-ALLOCATED 0 PLACED ;

\ ---- the machine-dialect cases -----------------------------------------------
\ Each is allocated and accepted like any other module: the walk TAKES the
\ allocator's binding, which is why every fixture here allocates and none has to
\ hand one back.
: M-ALLOCATED ( IR-BUILD:module -- IR-BUILD:module )
   {: m:IR-BUILD:module :}
   CC m LEAF A64RA:ALLOCATE
   m LEAF A64RAV:ACCEPT
   m ;

\ Allocated and accepted under the data-stack contract its boundary is written
\ for: `in` cells taken and `out` published.
: M-DALLOCATED ( IR-BUILD:module n n -- IR-BUILD:module )
   {: m:IR-BUILD:module in:n out:n :}
   CC m in out DLEAF A64RA:ALLOCATE
   m in out DLEAF A64RAV:ACCEPT
   m ;

: DIAMOND-BYTES ( IR-CTX:ctx -- )
   0 W-CTX ! BUILD-DIAMOND M-ALLOCATED 0 PLACED ;

: PAIR-BYTES ( IR-CTX:ctx -- )
   0 W-CTX ! BUILD-PAIR M-ALLOCATED 0 PLACED ;

: SELFCALL-BYTES ( IR-CTX:ctx -- )
   HIR-MOD BUILD-SELFCALLER CALL-ALLOCATED 0 PLACED ;

\ AT A SLOT OF ITS OWN, because what a wordcall's displacement is made of is the
\ callee's absolute entry LESS the placement: the same module at another slot is
\ other bytes, and pinning it anywhere but zero is what says so.
16 constant CALL-SLOT

: WORDCALL-BYTES ( IR-CTX:ctx -- )
   HIR-MOD CALLEE-ENTRY BUILD-WORDCALLER CALL-ALLOCATED CALL-SLOT PLACED ;

: RELOC-BYTES ( IR-CTX:ctx -- )
   0 W-CTX ! X64IR:ADDR-DATA BUILD-RELOC M-ALLOCATED 0 PLACED ;

\ Allocated and accepted under a leaf whose frame is the one it reserves.
: FRAMED-BYTES ( IR-CTX:ctx -- )
   0 W-CTX ! BUILD-FRAMED {: m:IR-BUILD:module :}
   CC m FRAME-N LEAF-FRAMED A64RA:ALLOCATE
   m FRAME-N LEAF-FRAMED A64RAV:ACCEPT
   m 0 PLACED ;

\ The selector calls two of this engine's entries: `die` for a trap and `throw`
\ for a divide's cold side. A case that calls one is placed at the aligned slot
\ at or below it, so its rel32 reaches the entry wherever the engine is loaded;
\ on Darwin arm64 that is above 4 GB, where no slot near zero reaches it.
: SLOT-BELOW ( n -- n )
   X64IR:SP-ALIGN / X64IR:SP-ALIGN * ;

: DIE-ENTRY ( -- n )
   NTRAP:ROUTINE ;

: TRAP-SLOT ( -- n )
   DIE-ENTRY SLOT-BELOW ;

: TRAP-BYTES ( IR-CTX:ctx -- )
   HIR-MOD BUILD-TRAP DTRAP UNDER TRAP-SLOT PLACED ;

\ Where the divide's cold side goes: `throw` in THIS engine's dictionary, which
\ is what the selector names (select-x64.f THROW-ENTRY). It moves with every
\ engine build as `die` does, so its call's field is read back and held to it
\ less the slot.
: THROW-TARGET ( -- n )
   s" throw" NDICT:CALL-TARGET ;

: THROW-SLOT ( -- n )
   THROW-TARGET SLOT-BELOW ;

: DIV-BYTES ( IR-CTX:ctx -- )
   HIR-MOD BUILD-DIV 2 1 DSTACK-ALLOCATED THROW-SLOT PLACED ;

\ At a slot of its own too, because the answer is the placement PLUS where the
\ second function starts.
: QUOTER-BYTES ( IR-CTX:ctx -- )
   HIR-MOD BUILD-QUOTER ALLOCATED CALL-SLOT PLACED ;

\ ---- the shadow cases: EMIT with no PLACE-AT ---------------------------------
\ Nothing to measure an absolute entry from, so every call and tail-branch field
\ is zero and its row alone says where it goes, and a function's address is its
\ offset in the emission.
: SHADOW ( IR-BUILD:module -- )
   {: m:IR-BUILD:module :}
   CC m X64EMIT:EMIT ;

\ A callee four gigabytes away, which no rel32 reaches from anywhere near: the
\ placed twin is FAR-REFUSE-CASES's refusal, and here there is no reach to ask.
$100000000 constant FAR-ENTRY

: SHADOW-CALL-BYTES ( IR-CTX:ctx -- )
   HIR-MOD FAR-ENTRY BUILD-WORDCALLER CALL-ALLOCATED SHADOW ;

: SHADOW-TAIL-BYTES ( IR-CTX:ctx -- )
   HIR-MOD CALLEE-ENTRY BUILD-WORDCALLER TAIL-ALLOCATED SHADOW ;

: SHADOW-QUOTER-BYTES ( IR-CTX:ctx -- )
   HIR-MOD BUILD-QUOTER ALLOCATED SHADOW ;

\ At slot zero with the cold side's entry at CALLEE-ENTRY, so every byte is the
\ module's own and none is the host's.
: REMAINDER-BYTES ( IR-CTX:ctx -- )
   0 W-CTX ! CALLEE-ENTRY BUILD-REMAINDER 3 1 M-DALLOCATED 0 PLACED ;

\ Where `hir.trap` goes: `die` in THIS engine's dictionary, which is what the
\ selector names (select-x64.f TRAP-ENTRY). That address is the host's and moves
\ with every engine build, so the call's field is not pinned as bytes: it is read
\ back and held to the entry less the slot and the routine's own length, the
\ call being the routine's last instruction.
4 constant REL32-N                   \ the bytes of a rel32 field

\ The rel32 that ends the sealed emission, little-endian and signed.
: LAST-REL32 ( -- n )
   X64EMIT:BYTES X64EMIT:SIZE REL32-N - + {: f:ptr :}
   0
   REL32-N 0 ?do  f i + c@  i 8 * lshift or  loop
   dup $80000000 and 0<> if $100000000 - then ;

\ Compare every byte but the field that ends the emission, without retiring.
: XH= ( ptr u8 n -- ) {: ea:ptr eu:n :}
   X64EMIT:BYTES X64EMIT:SIZE REL32-N - {: da:ptr dlen:n :}
   da dlen ea eu SPAN=HEX? TTRUE ;

: TRAP-FIELD ( -- n )
   DIE-ENTRY TRAP-SLOT - X64EMIT:SIZE - ;

\ The rel32 `at` bytes into the sealed emission, little-endian and signed.
: REL32@ ( n -- n )
   {: at:n :}
   X64EMIT:BYTES at + {: f:ptr :}
   0
   REL32-N 0 ?do  f i + c@  i 8 * lshift or  loop
   dup $80000000 and 0<> if $100000000 - then ;

\ Compare every byte but the rel32 field `at` bytes in, without retiring: the
\ expected string leaves the field out.
: XF= ( ptr u8 n n -- ) {: ea:ptr eu:n at:n :}
   X64EMIT:BYTES {: da:ptr :}
   da at  ea at 2 *  SPAN=HEX? TTRUE
   da at + REL32-N +  X64EMIT:SIZE at - REL32-N -
   ea at 2 * +  eu at 2 * -  SPAN=HEX? TTRUE ;

\ The quoting routine's own readers: two functions, one site.
: QUOTER-FACTS ( -- n n n n n )
   X64EMIT:ADDR-SITES  0 X64EMIT:ADDR-SITE@  0 X64EMIT:ADDR-SITE-KIND@
   0 X64EMIT:FUNCTION-OFFSET@  1 X64EMIT:FUNCTION-OFFSET@ ;

\ The sealed emission's own readers, taken on the routine that has one site.
\ Every number here is a BYTE count or a BYTE offset, which is what separates
\ these readers from the ARM64 emitter's: there an instruction is four bytes and
\ the answers are instruction indices.
: RELOC-FACTS ( -- n n n n n n )
   X64EMIT:SIZE  X64EMIT:BLOCKS  0 X64EMIT:FUNCTION-OFFSET@
   X64EMIT:ADDR-SITES  0 X64EMIT:ADDR-SITE@  0 X64EMIT:ADDR-SITE-KIND@ ;

\ The one row a routine that leaves for another word's entry files: where the
\ `call` or `jmp` starts, which of NEMIT's kinds it is, and the absolute entry.
: CALL-ROW ( -- n n n n )
   X64EMIT:CALL-SITES
   0 X64EMIT:CALL-SITE@  0 X64EMIT:CALL-KIND@  0 X64EMIT:CALL-TARGET@ ;

: CALL-PAST-END ( -- )
   1 X64EMIT:CALL-SITE@ drop ;

\ ---- why the addressed pins are taken under the data-stack convention --------
\ The same four forms in a REGISTER-convention leaf are not accepted, so their
\ bytes could not be pinned there. The order such a routine is handed arrives as
\ an argument and the one its last store answers is read by nothing - a register
\ convention has no publish to consume it - and the validator refuses exactly
\ that (regalloc-verify.f ORDER-VALUE-CK). Under the data-stack convention the
\ exit's `x64.dpublish` reads the order the body ends with, which is why
\ DADDR-BYTES above is accepted and this is not.
: ADDRESSED-ACCEPT ( -- )
   ALLOCATED drop ;

\ ---- the refusals ------------------------------------------------------------
\ A well-shaped module nobody allocated: the accepted assignment is another
\ module's and the registers of this one were never handed out. It hands the
\ allocator's dialect binding back itself, because the walk is what takes a
\ binding and a second binding over a live one is E-A64RA-BIND.
: UNALLOCATED-EMIT ( -- )
   SELECTED {: m:IR-BUILD:module :}
   A64RA:RELEASE
   m 0 PLACED ;

: ADDRESSED-CASES ( IR-CTX:ctx -- )
   HIR-MOD
   BUILD-ADDRESSED
   s" the same four forms under the REGISTER convention are refused: nothing reads the order the last store answers, there being no publish" T-LABEL
   [: ADDRESSED-ACCEPT ;] E-A64RAV-ORDER TTHROWSQ ;

\ ---- the state machine around one emission -----------------------------------
\ Bound, placed or not, sealed, retired. Each of these asks for a step out of
\ turn.
: READ-UNSEALED ( -- )
   X64EMIT:SIZE drop ;

: PLACE-TWICE ( -- )
   0 X64EMIT:PLACE-AT
   0 X64EMIT:PLACE-AT ;

: PLACE-NEGATIVE ( -- )
   X64IR:SP-ALIGN negate X64EMIT:PLACE-AT ;

: PLACE-UNALIGNED ( -- )
   X64IR:SP-ALIGN 1+ X64EMIT:PLACE-AT ;

\ One site is what the routine below has, so the second is past its count.
: SITE-PAST-END ( -- )
   1 X64EMIT:ADDR-SITE@ drop ;

: STATE-CASES ( -- )
   s" a reader of an emission that has not been sealed is refused rather than answering the last one's bytes" T-LABEL
   [: READ-UNSEALED ;] E-X64EMIT-STATE TTHROWSQ
   s" a second placement over a live one is refused: a routine is written at one slot" T-LABEL
   [: PLACE-TWICE ;] E-X64EMIT-PLACE TTHROWSQ
   X64EMIT:RETIRE
   s" a negative slot is no address this machine's code region hands out" T-LABEL
   [: PLACE-NEGATIVE ;] E-X64EMIT-PLACE TTHROWSQ
   s" a slot off the machine's own alignment is refused: X64IR:SP-ALIGN is the unit a code region hands slots out in" T-LABEL
   [: PLACE-UNALIGNED ;] E-X64EMIT-PLACE TTHROWSQ
   X64EMIT:RETIRE ;

\ A form this emitter does not render, in the machine-dialect module it was
\ built in.
: MACHINE-REFUSE-CASES ( IR-CTX:ctx -- )
   0 W-CTX !
   s" a form this emitter does not render is refused by name in a routine whose shape and assignment are both agreed" T-LABEL
   [: NEGATOR-EMIT ;] E-X64EMIT-FORM TTHROWSQ
   X64EMIT:RETIRE ;

: BADFCMP-EMIT ( X64IR:cond n -- )
   BUILD-BADFCMP 2 1 M-DALLOCATED 0 PLACED ;

\ `gt` answers with one flag and `equal` with the flag and the `setnp` scratch;
\ every other pairing of condition and count is refused by name.
: FCMP-REFUSE-CASES ( IR-CTX:ctx -- )
   0 W-CTX !
   s" a gt fcmpset carrying a scratch result is refused: seta writes one byte" T-LABEL
   [: X64IR-COND:GT 2 BADFCMP-EMIT ;] E-X64EMIT-FORM TTHROWSQ
   X64EMIT:RETIRE
   s" an equal fcmpset without its scratch result is refused: the setnp byte has no register" T-LABEL
   [: X64IR-COND:EQUAL 1 BADFCMP-EMIT ;] E-X64EMIT-FORM TTHROWSQ
   X64EMIT:RETIRE
   s" an fcmpset under a condition other than gt and equal is refused: ucomisd writes the unsigned flags" T-LABEL
   [: X64IR-COND:LT 1 BADFCMP-EMIT ;] E-X64EMIT-FORM TTHROWSQ
   X64EMIT:RETIRE ;

\ WHAT A REFUSED EMISSION LEAVES BEHIND, which is nothing: the refusal above is
\ thrown from inside a MEASUREMENT - the pass that writes a zero into every
\ displacement field, the width of one not depending on the number in it - so an
\ emitter that let its measuring flag stand would write a zero here too, and this
\ branch is the shortest displacement the suite has.
: AFTER-REFUSE-CASES ( IR-CTX:ctx -- )
   HIR-MOD
   CALLEE-ENTRY BUILD-WORDCALLER TAIL-ALLOCATED 0 PLACED
   s" the emission after a refused one writes its displacement: the refusal walked out of a measurement and left no measuring behind" T-LABEL
   \ mc: jmp 1019
   s" e9fb030000" X= ;

\ THE FAR CALL IS REFUSED BY THE WRITER AND NOT BY THE MEASURER: a measurement
\ writes zero into every displacement field, because the field's width does not
\ depend on the number in it, so the reach of this one is only asked once both
\ ends are known.
: FARCALL-EMIT ( -- )
   CALL-ALLOCATED 0 PLACED ;

: FAR-REFUSE-CASES ( IR-CTX:ctx -- )
   HIR-MOD
   $100000000 BUILD-WORDCALLER
   s" a callee more than two gigabytes from the placement is refused: no rel32 field holds that displacement" T-LABEL
   [: FARCALL-EMIT ;] E-X64EMIT-REACH TTHROWSQ
   X64EMIT:RETIRE ;

: UNACCEPTED-CASES ( IR-CTX:ctx -- )
   HIR-MOD
   BUILD-DIFF
   s" a module the accepted allocation is not about is refused before a byte is written" T-LABEL
   [: UNALLOCATED-EMIT ;] E-X64EMIT-ACCEPT TTHROWSQ
   X64EMIT:RETIRE ;

\ The divide, its call row to `throw`, and the remainder read.
: DIVIDE-CASES ( -- )
   s" the divide under the data-stack convention: a zero divisor's code pushed and handed to throw, minus one a negation, any other divisor the divide" T-LABEL
   WBND [: DIV-BYTES ;] IR-CTX:WITH-CONTEXT
   \ mc: subq $16, %r12
   \ mc: movq (%r12), %rax
   \ mc: movq 8(%r12), %rcx
   \ mc: movq %rax, %rax
   \ mc: testq %rcx, %rcx
   \ mc: jne 23
   \ mc: movq $-6400, %rdx
   \ mc: movq %rdx, (%r12)
   \ mc: addq $8, %r12
   \ mc: callq THROW-TARGET-(THROW-SLOT+51)
   \ mc: cmpq $-1, %rcx
   \ mc: jne 10
   \ mc: negq %rax
   \ mc: xorl %edx, %edx
   \ mc: jmp 5
   \ mc: cqto
   \ mc: idivq %rcx
   \ mc: movq %rax, (%r12)
   \ mc: addq $8, %r12
   \ mc: retq
   s" 4981ec10000000498b0424498b4c24084889c04885c90f851700000048c7c200e7ffff498914244981c408000000e84881f9ffffffff0f850a00000048f7d831d2e905000000489948f7f9498904244981c408000000c3" 47 XF=
   47 REL32@  THROW-TARGET THROW-SLOT - 51 -  T=
   s" the divide's call files a row naming throw's entry, the cold side's last instruction" T-LABEL
   CALL-ROW
   THROW-TARGET T=
   NEMIT:CALL T=
   46 T=                                 \ the e8 before the rel32 at 47
   1 T=
   X64EMIT:RETIRE

   s" the remainder read, with a value live across the divide in rcx and the divisor kept out of rdx, which cqo writes first" T-LABEL
   WBND [: REMAINDER-BYTES ;] IR-CTX:WITH-CONTEXT
   \ mc: subq $24, %r12
   \ mc: movq (%r12), %rcx
   \ mc: movq 8(%r12), %rax
   \ mc: movq 16(%r12), %rsi
   \ mc: testq %rsi, %rsi
   \ mc: jne 23
   \ mc: movq $-6400, %rdx
   \ mc: movq %rdx, (%r12)
   \ mc: addq $8, %r12
   \ mc: callq 971
   \ mc: cmpq $-1, %rsi
   \ mc: jne 10
   \ mc: negq %rax
   \ mc: xorl %edx, %edx
   \ mc: jmp 5
   \ mc: cqto
   \ mc: idivq %rsi
   \ mc: addq %rcx, %rdx
   \ mc: movq %rdx, (%r12)
   \ mc: addq $8, %r12
   \ mc: retq
   s" 4981ec18000000498b0c24498b442408498b7424104885f60f851700000048c7c200e7ffff498914244981c408000000e8cb0300004881feffffffff0f850a00000048f7d831d2e905000000489948f7fe4801ca498914244981c408000000c3" X= ;

: CMPSEL-BYTES ( IR-CTX:ctx -- )
   0 W-CTX ! BUILD-CMPSEL 3 1 M-DALLOCATED 0 PLACED ;

: SELZ-BYTES ( IR-CTX:ctx -- )
   0 W-CTX ! BUILD-SELZ 3 1 M-DALLOCATED 0 PLACED ;

\ The flags between the compare and the move are no value, so each select is
\ written as the pair; test/x86-64-peer-routines.f runs its aliasing cases.
: SELECT-CASES ( -- )
   s" cmpsel: the compare, then the move of the value the condition holds for over the tied one it fails for" T-LABEL
   WBND [: CMPSEL-BYTES ;] IR-CTX:WITH-CONTEXT
   \ mc: subq $24, %r12
   \ mc: movq (%r12), %rax
   \ mc: movq 8(%r12), %rcx
   \ mc: movq 16(%r12), %rdx
   \ mc: movabsq $1000, %rsi
   \ mc: cmpq %rcx, %rax
   \ mc: cmovlq %rsi, %rdx
   \ mc: movq %rdx, (%r12)
   \ mc: addq $8, %r12
   \ mc: retq
   s" 4981ec18000000498b0424498b4c2408498b54241048bee8030000000000004839c8480f4cd6498914244981c408000000c3" X=

   s" selz: the test against zero, then the move of the zero answer over the tied nonzero one" T-LABEL
   WBND [: SELZ-BYTES ;] IR-CTX:WITH-CONTEXT
   \ mc: subq $24, %r12
   \ mc: movq (%r12), %rax
   \ mc: movq 8(%r12), %rcx
   \ mc: movq 16(%r12), %rdx
   \ mc: testq %rax, %rax
   \ mc: cmoveq %rdx, %rcx
   \ mc: movq %rcx, (%r12)
   \ mc: addq $8, %r12
   \ mc: retq
   s" 4981ec18000000498b0424498b4c2408498b5424104885c0480f44ca49890c244981c408000000c3" X= ;

\ A count coalesced into its load, under the data-stack convention: the class
\ pinned to rcx opens at the load, and b, loaded before it and dead at the add,
\ is kept out of rcx there (regalloc.f MB-FORBID-PINS).
: SUM-SHIFT-CASES ( -- )
   s" a computed left shift whose count is loaded straight into rcx: b, loaded before it and dead at the add, is kept out of rcx" T-LABEL
   WBND [: DSUM-SHL-BYTES ;] IR-CTX:WITH-CONTEXT
   \ mc: subq $24, %r12
   \ mc: movq (%r12), %rax
   \ mc: movq 8(%r12), %rdx
   \ mc: movq 16(%r12), %rcx
   \ mc: addq %rdx, %rax
   \ mc: movq %rcx, %rcx
   \ mc: shlq %cl, %rax
   \ mc: movq %rax, (%r12)
   \ mc: addq $8, %r12
   \ mc: retq
   s" 4981ec18000000498b0424498b542408498b4c24104801d04889c948d3e0498904244981c408000000c3" X=

   s" the same shape shifted right" T-LABEL
   WBND [: DSUM-SHR-BYTES ;] IR-CTX:WITH-CONTEXT
   \ mc: subq $24, %r12
   \ mc: movq (%r12), %rax
   \ mc: movq 8(%r12), %rdx
   \ mc: movq 16(%r12), %rcx
   \ mc: addq %rdx, %rax
   \ mc: movq %rcx, %rcx
   \ mc: shrq %cl, %rax
   \ mc: movq %rax, (%r12)
   \ mc: addq $8, %r12
   \ mc: retq
   s" 4981ec18000000498b0424498b542408498b4c24104801d04889c948d3e8498904244981c408000000c3" X= ;

public

: RUN ( -- )
   T-RESET

   s" the two-address difference and the return" T-LABEL
   WBND [: DIFF-BYTES ;] IR-CTX:WITH-CONTEXT
   \ mc: subq %rcx, %rax
   \ mc: retq
   s" 4829c8c3" X=

   s" the copy a two-address form needs, then the addition" T-LABEL
   WBND [: SQUARE-BYTES ;] IR-CTX:WITH-CONTEXT
   \ mc: movq %rax, %rcx
   \ mc: addq %rax, %rcx
   \ mc: retq
   s" 4889c14801c1c3" X=

   s" the four remaining tied binaries" T-LABEL
   WBND [: CHAIN-BYTES ;] IR-CTX:WITH-CONTEXT
   \ mc: andq %rcx, %rax
   \ mc: orq %rcx, %rax
   \ mc: xorq %rcx, %rax
   \ mc: imulq %rcx, %rax
   \ mc: retq
   s" 4821c84809c84831c8480fafc1c3" X=

   s" the five immediate forms" T-LABEL
   WBND [: IMMS-BYTES ;] IR-CTX:WITH-CONTEXT
   \ mc: andq %rax, %rcx
   \ mc: addq $1000, %rcx
   \ mc: subq $2000, %rcx
   \ mc: andq $4095, %rcx
   \ mc: orq $61440, %rcx
   \ mc: xorq $255, %rcx
   \ mc: retq
   s" 4821c14881c1e80300004881e9d00700004881e1ff0f00004881c900f000004881f1ff000000c3" X=

   s" both immediate shifts" T-LABEL
   WBND [: SHIFTS-BYTES ;] IR-CTX:WITH-CONTEXT
   \ mc: shlq $3, %rax
   \ mc: shrq $5, %rax
   \ mc: retq
   s" 48c1e00348c1e805c3" X=

   s" a computed left shift: the value's copy shifted by cl, the value read after it" T-LABEL
   WBND [: SHL-BYTES ;] IR-CTX:WITH-CONTEXT
   \ mc: movq %rax, %rdx
   \ mc: movq %rcx, %rcx
   \ mc: shlq %cl, %rdx
   \ mc: xorq %rdx, %rax
   \ mc: retq
   s" 4889c24889c948d3e24831d0c3" X=

   s" a computed right shift, logical, the same shape" T-LABEL
   WBND [: SHR-BYTES ;] IR-CTX:WITH-CONTEXT
   \ mc: movq %rax, %rdx
   \ mc: movq %rcx, %rcx
   \ mc: shrq %cl, %rdx
   \ mc: xorq %rdx, %rax
   \ mc: retq
   s" 4889c24889c948d3ea4831d0c3" X=

   s" a computed left shift whose tied destination is kept out of rcx, where the count's copy is" T-LABEL
   WBND [: SHL-ADD-BYTES ;] IR-CTX:WITH-CONTEXT
   \ mc: movq %rcx, %rcx
   \ mc: shlq %cl, %rdx
   \ mc: addq %rdx, %rax
   \ mc: retq
   s" 4889c948d3e24801d0c3" X=

   s" a computed left shift whose value and count both live across it, kept out of rcx" T-LABEL
   WBND [: SHL-CROSS-BYTES ;] IR-CTX:WITH-CONTEXT
   \ mc: movq %rax, %rsi
   \ mc: movq %rdx, %rcx
   \ mc: shlq %cl, %rsi
   \ mc: xorq %rax, %rsi
   \ mc: addq %rsi, %rdx
   \ mc: retq
   s" 4889c64889d148d3e64831c64801f2c3" X=

   s" the complement" T-LABEL
   WBND [: NOT-BYTES ;] IR-CTX:WITH-CONTEXT
   \ mc: notq %rax
   \ mc: retq
   s" 48f7d0c3" X=

   s" compare, set, widen and negate" T-LABEL
   WBND [: CMPSET-BYTES ;] IR-CTX:WITH-CONTEXT
   \ mc: cmpq %rcx, %rax
   \ mc: setl %al
   \ mc: movzbq %al, %rax
   \ mc: negq %rax
   \ mc: retq
   s" 4839c80f9cc0480fb6c048f7d8c3" X=

   s" compare against a folded literal, set, widen and negate" T-LABEL
   WBND [: CMPSETI-BYTES ;] IR-CTX:WITH-CONTEXT
   \ mc: subq %rax, %rcx
   \ mc: cmpq $1000, %rcx
   \ mc: setl %al
   \ mc: movzbq %al, %rax
   \ mc: negq %rax
   \ mc: retq
   s" 4829c14881f9e80300000f9cc0480fb6c048f7d8c3" X=

   s" the literal no immediate holds, materialised" T-LABEL
   WBND [: MOVI-BYTES ;] IR-CTX:WITH-CONTEXT
   \ mc: movabsq $4294967296, %rcx
   \ mc: addq %rcx, %rax
   \ mc: retq
   s" 48b900000000010000004801c8c3" X=

   s" the same difference under the data-stack convention: the pointer taken, both cells loaded, the result stored and published" T-LABEL
   WBND [: DDIFF-BYTES ;] IR-CTX:WITH-CONTEXT
   \ mc: subq $16, %r12
   \ mc: movq (%r12), %rax
   \ mc: movq 8(%r12), %rcx
   \ mc: subq %rcx, %rax
   \ mc: movq %rax, (%r12)
   \ mc: addq $8, %r12
   \ mc: retq
   s" 4981ec10000000498b0424498b4c24084829c8498904244981c408000000c3" X=

   s" the four addressed forms, over an address the routine took off the data stack" T-LABEL
   WBND [: DADDR-BYTES ;] IR-CTX:WITH-CONTEXT
   \ mc: subq $8, %r12
   \ mc: movq (%r12), %rax
   \ mc: movq (%rax), %rcx
   \ mc: movzbq (%rax), %rdx
   \ mc: movq %rcx, (%rax)
   \ mc: movb %dl, (%rax)
   \ mc: movq %rdx, (%r12)
   \ mc: addq $8, %r12
   \ mc: retq
   s" 4981ec08000000498b0424488b08480fb6104889088810498914244981c408000000c3" X=

   s" the loop, placed: the header entered by falling into it, the backedge the one branch that goes backwards, and the return block last" T-LABEL
   WBND [: DLOOP-BYTES ;] IR-CTX:WITH-CONTEXT
   \ mc: subq $16, %r12
   \ mc: movq (%r12), %rax
   \ mc: movq 8(%r12), %rcx
   \ mc: movq %rcx, %rcx
   \ mc: movq %rcx, %rcx
   \ mc: movq %rax, %rdx
   \ mc: addq %rcx, %rdx
   \ mc: testq %rdx, %rdx
   \ mc: je 11
   \ mc: movq %rdx, %rdx
   \ mc: movq %rdx, %rcx
   \ mc: jmp -26
   \ mc: movq %rdx, (%r12)
   \ mc: addq $8, %r12
   \ mc: retq
   s" 4981ec10000000498b0424498b4c24084889c94889c94889c24801ca4885d20f840b0000004889d24889d1e9e6ffffff498914244981c408000000c3" XB=
   \ Where the four blocks begin, in bytes and BY ORDINAL: the entry at zero, the
   \ header the entry falls into and the backedge jumps back to, the return block
   \ the order moved to the end, and the backedge block before it.
   X64EMIT:BLOCKS 4 T=
   0 X64EMIT:BLOCK-START@ 0 T=
   1 X64EMIT:BLOCK-START@ 22 T=
   2 X64EMIT:BLOCK-START@ 48 T=
   3 X64EMIT:BLOCK-START@ 37 T=
   X64EMIT:RETIRE

   SUM-SHIFT-CASES

   s" the compare-and-branch diamond, placed: both arms written and the block they join at taking the answer as its argument" T-LABEL
   WBND [: DIAMOND-BYTES ;] IR-CTX:WITH-CONTEXT
   \ mc: cmpq %rcx, %rax
   \ mc: jl 15
   \ mc: movabsq $2000, %rax
   \ mc: jmp 10
   \ mc: movabsq $1000, %rax
   \ mc: retq
   s" 4839c80f8c0f00000048b8d007000000000000e90a00000048b8e803000000000000c3" XB=
   \ BY ORDINAL again, and the order is not the ordinals': the arm the compare
   \ branches to is laid SECOND, the arm it falls into first, and the join both
   \ end at is last - so the arm laid next to the join reaches it by falling out
   \ and only the other one needs a branch over it.
   X64EMIT:BLOCKS 4 T=
   1 X64EMIT:BLOCK-START@ 24 T=
   2 X64EMIT:BLOCK-START@ 9 T=
   3 X64EMIT:BLOCK-START@ 34 T=
   X64EMIT:RETIRE

   s" two routines in one emission: the diamond, then a literal and a return, each walked against its own block starts" T-LABEL
   WBND [: PAIR-BYTES ;] IR-CTX:WITH-CONTEXT
   \ mc: cmpq %rcx, %rax
   \ mc: jl 15
   \ mc: movabsq $2000, %rax
   \ mc: jmp 10
   \ mc: movabsq $1000, %rax
   \ mc: retq
   \ mc: movabsq $3000, %rax
   \ mc: retq
   s" 4839c80f8c0f00000048b8d007000000000000e90a00000048b8e803000000000000c348b8b80b000000000000c3" XB=
   \ The block table the readers answer from is the LAST function laid: four
   \ positions while the diamond was walked, ONE now, because the routine after
   \ it has one block. A function is asked where it starts in the emission; a
   \ block is asked where it starts within the function being laid.
   X64EMIT:BLOCKS 1 T=
   0 X64EMIT:FUNCTION-OFFSET@ 0 T=
   1 X64EMIT:FUNCTION-OFFSET@ 35 T=
   X64EMIT:RETIRE

   s" the self-call: the pointer over the callee's arguments, the call to function zero of this emission, the pointer back over its results" T-LABEL
   WBND [: SELFCALL-BYTES ;] IR-CTX:WITH-CONTEXT
   \ mc: subq $8, %r12
   \ mc: movq (%r12), %rax
   \ mc: movq %rax, 8(%r12)
   \ mc: addq $16, %r12
   \ mc: callq -28
   \ mc: subq $16, %r12
   \ mc: movq 8(%r12), %rax
   \ mc: movq %rax, (%r12)
   \ mc: addq $8, %r12
   \ mc: retq
   s" 4981ec08000000498b042449894424084981c410000000e8e4ffffff4981ec10000000498b442408498904244981c408000000c3" XB=
   s" a call to function zero of this emission is measured inside it and files no row" T-LABEL
   X64EMIT:CALL-SITES 0 T=
   X64EMIT:RETIRE

   s" a call to another word's entry, from a routine placed at a slot of its own" T-LABEL
   WBND [: WORDCALL-BYTES ;] IR-CTX:WITH-CONTEXT
   \ mc: subq $8, %r12
   \ mc: movq (%r12), %rax
   \ mc: movq %rax, 8(%r12)
   \ mc: addq $16, %r12
   \ mc: callq 980
   \ mc: subq $16, %r12
   \ mc: movq 8(%r12), %rax
   \ mc: movq %rax, (%r12)
   \ mc: addq $8, %r12
   \ mc: retq
   s" 4981ec08000000498b042449894424084981c410000000e8d40300004981ec10000000498b442408498904244981c408000000c3" XB=
   s" the call files a row: where the call starts, a call that comes back, and the callee's absolute entry" T-LABEL
   CALL-ROW
   CALLEE-ENTRY T=                       \ the entry, not the placed displacement
   NEMIT:CALL T=
   23 T=                                 \ the e8 byte, after the pointer moved over the argument
   1 T=
   s" the one row this routine has is the only one a reader answers about" T-LABEL
   [: CALL-PAST-END ;] E-X64EMIT-BOUND TTHROWSQ
   X64EMIT:RETIRE

   s" the routine that leaves through its callee: one tail branch and no return" T-LABEL
   WBND [: TAIL-BYTES ;] IR-CTX:WITH-CONTEXT
   \ mc: jmp 1019
   s" e9fb030000" XB=
   s" the tail branch files a row: at byte zero, a branch that leaves for good, and the callee's entry" T-LABEL
   CALL-ROW
   CALLEE-ENTRY T=
   NEMIT:TAIL T=
   0 T=
   1 T=
   X64EMIT:RETIRE

   s" the relocatable literal, and the site the emission recorded for it" T-LABEL
   WBND [: RELOC-BYTES ;] IR-CTX:WITH-CONTEXT
   \ mc: movabsq $3665428480, %rax
   \ mc: retq
   s" 48b800007ada00000000c3" XB=
   RELOC-FACTS
   X64IR:ADDR-DATA T=                    \ the site is a data address
   0 T=                                  \ at byte zero, the `mov r64, imm64` itself
   1 T=                                  \ one site, because ADDR-LANES is one
   0 T=                                  \ the function starts where the emission does
   1 T=                                  \ one block
   11 T=                                 \ eleven bytes of it
   s" the one site this routine has is the only one a reader answers about" T-LABEL
   [: SITE-PAST-END ;] E-X64EMIT-BOUND TTHROWSQ
   X64EMIT:RETIRE

   s" the four frame forms: the frame taken on rsp, a cell put away and brought back at no displacement and at a disp8, the frame given back" T-LABEL
   WBND [: FRAMED-BYTES ;] IR-CTX:WITH-CONTEXT
   \ mc: subq $16, %rsp
   \ mc: movq %rax, (%rsp)
   \ mc: movq %rax, 8(%rsp)
   \ mc: movq (%rsp), %rax
   \ mc: movq 8(%rsp), %rcx
   \ mc: addq %rcx, %rax
   \ mc: addq $16, %rsp
   \ mc: retq
   s" 4881ec10000000488904244889442408488b0424488b4c24084801c84881c410000000c3" X=

   s" the trap, placed: the three cells die reads stored, the pointer moved over them, and a call to die's entry with nothing after it" T-LABEL
   WBND [: TRAP-BYTES ;] IR-CTX:WITH-CONTEXT
   \ mc: movabsq $3665428480, %rax
   \ mc: movabsq $5, %rcx
   \ mc: movabsq $70, %rdx
   \ mc: movq %rax, (%r12)
   \ mc: movq %rcx, 8(%r12)
   \ mc: movq %rdx, 16(%r12)
   \ mc: addq $24, %r12
   \ mc: callq TRAP-FIELD
   s" 48b800007ada0000000048b9050000000000000048ba46000000000000004989042449894c240849895424104981c418000000e8" XH=
   LAST-REL32 TRAP-FIELD T=
   s" the trap's call files a row naming die's entry, the routine's last instruction" T-LABEL
   CALL-ROW
   DIE-ENTRY T=
   NEMIT:CALL T=
   X64EMIT:SIZE REL32-N - 1- T=          \ the e8 before the rel32 that ends it
   1 T=
   X64EMIT:RETIRE

   s" a function's address, from a routine placed at a slot of its own: the placement plus where the second function starts, in the ten bytes a literal takes" T-LABEL
   WBND [: QUOTER-BYTES ;] IR-CTX:WITH-CONTEXT
   \ mc: movabsq $27, %rax
   \ mc: retq
   \ mc: movabsq $3000, %rax
   \ mc: retq
   s" 48b81b00000000000000c348b8b80b000000000000c3" XB=
   QUOTER-FACTS
   11 T=                                 \ the second function starts 11 bytes in
   0 T=                                  \ and the first where the emission does
   X64IR:ADDR-CODE T=                    \ the site is a code address
   0 T=                                  \ at byte zero, the `mov r64, imm64` itself
   1 T=                                  \ one site: the literal 3000 is no address
   X64EMIT:RETIRE

   s" a shadow call: no placement, so the rel32 is zero and no reach is asked, even of a callee four gigabytes away" T-LABEL
   WBND [: SHADOW-CALL-BYTES ;] IR-CTX:WITH-CONTEXT
   \ mc: subq $8, %r12
   \ mc: movq (%r12), %rax
   \ mc: movq %rax, 8(%r12)
   \ mc: addq $16, %r12
   \ mc: callq 0
   \ mc: subq $16, %r12
   \ mc: movq 8(%r12), %rax
   \ mc: movq %rax, (%r12)
   \ mc: addq $8, %r12
   \ mc: retq
   s" 4981ec08000000498b042449894424084981c410000000e8000000004981ec10000000498b442408498904244981c408000000c3" XB=
   s" the row is the whole statement of where a shadow call goes" T-LABEL
   CALL-ROW
   FAR-ENTRY T=
   NEMIT:CALL T=
   23 T=
   1 T=
   X64EMIT:RETIRE

   s" a shadow tail branch: the rel32 zero, the row naming the entry" T-LABEL
   WBND [: SHADOW-TAIL-BYTES ;] IR-CTX:WITH-CONTEXT
   \ mc: jmp 0
   s" e900000000" XB=
   CALL-ROW
   CALLEE-ENTRY T=
   NEMIT:TAIL T=
   0 T=
   1 T=
   X64EMIT:RETIRE

   s" a shadow function address: the second function's offset in the emission, which the writer adds its placement to" T-LABEL
   WBND [: SHADOW-QUOTER-BYTES ;] IR-CTX:WITH-CONTEXT
   \ mc: movabsq $11, %rax
   \ mc: retq
   \ mc: movabsq $3000, %rax
   \ mc: retq
   s" 48b80b00000000000000c348b8b80b000000000000c3" XB=
   QUOTER-FACTS
   11 T=
   0 T=
   X64IR:ADDR-CODE T=
   0 T=
   1 T=
   X64EMIT:RETIRE

   DIVIDE-CASES
   SELECT-CASES

   WBND [: ADDRESSED-CASES ;] IR-CTX:WITH-CONTEXT
   STATE-CASES
   WBND [: MACHINE-REFUSE-CASES ;] IR-CTX:WITH-CONTEXT
   WBND [: FCMP-REFUSE-CASES ;] IR-CTX:WITH-CONTEXT
   WBND [: AFTER-REFUSE-CASES ;] IR-CTX:WITH-CONTEXT
   WBND [: FAR-REFUSE-CASES ;] IR-CTX:WITH-CONTEXT
   WBND [: UNACCEPTED-CASES ;] IR-CTX:WITH-CONTEXT

   T-REPORT ;

;package

\ x64-emit.f - the x86-64 bytes: src/compiler/native/emit-x64.f (X64EMIT) over
\ the routines src/compiler/native/select-x64.f selects and the allocator and its
\ validator accept, and over two routines written straight in the machine
\ dialect. THE EMITTER OWNS ITS SINK: a case declares where the routine goes with
\ `X64EMIT:PLACE-AT`, emits, reads the sealed image back and retires it, which is
\ what gives the placement back as well as the bytes.
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
\ longer one this encoder has.
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
\ emitter only through the selector. ONE FIXTURE PER CONTEXT, and the refusing
\ cases run inside an enclosing one, for the reason that suite gives.
\
\ WHAT THIS SUITE DOES NOT PIN. Of the forms this emitter renders, only
\ `x64.cmpbri` has no byte string here: the selector is deliberately unfused and
\ mints no compare-and-branch at all, and the diamond below is the one staged in
\ the dialect. What else is left unmeasured is a byte no module of this slice can
\ produce, and each of them is a later slice's:
\
\ - THE REGISTERS ABOVE rdx. The pool is rax, rcx, rdx, rsi, rdi and r8..r11
\   (x64ir.f RESERVED-MASK), and no fixture here is wide enough for the
\   allocator to reach past rdx, so no pinned byte sets REX.R or REX.B for an
\   operand and none names the spl/bpl/sil/dil byte registers.
\ - A DATA-STACK DISPLACEMENT OUTSIDE disp8, or a negative one. `x64.dslot` is
\   signed and reaches disp32 (x64ir.f DSLOT), which wants a contract of more
\   than sixteen cells.
\ - THE FORMS STILL REFUSED BY NAME, one case standing for all of them below:
\   the variable shifts `x64.shl` and `x64.shr`, `x64.idiv`, `x64.neg`, the
\   selects `x64.cmpsel` and `x64.selz`, the frame forms `x64.reserve`,
\   `x64.release`, `x64.store` and `x64.load`, `x64.trap` and `x64.codeaddr`.
\   Each needs a register the machine names, a prologue, or a lowering, and none
\   of those is here.
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
require src/compiler/native/select-x64.f
require src/compiler/native/regalloc.f
require src/compiler/native/regalloc-verify.f
require src/compiler/native/emit-x64.f
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

: OPEN-FUN-SIG ( IR-ID:ir-type-id -- )
   {: sig:IR-ID:ir-type-id :}
   CC BB  CC BB s" LEAF" IR-BUILD:INTERN-SYMBOL  IR-BUILD:BEGIN-FUN
   CC BB  sig  IR-BUILD:SET-SIGNATURE
   CC BB IR--FUN-LINKAGE:DEFINED IR-BUILD:SET-LINKAGE
   CC BB IR--FUN-VISIBILITY:EXPORTED IR-BUILD:SET-VISIBILITY
   CC BB IR--FUN-CONVENTION:HABU IR-BUILD:SET-CONVENTION
   CC BB  NAME-ST NAME-LN SPN  IR-BUILD:SET-FUN-SPAN
   CC BB IR-BUILD:BEGIN-BLOCK
   CC BB  OPEN-ST OPEN-LN SPN  IR-BUILD:SET-BLOCK-SPAN ;

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

\ `: LEAF ( a -- n ) invert ;` - the one unary form of this slice.
: BUILD-NOT ( -- )
   1 1 OPEN-FUN
   ARG+ {: a:IR-ID:ir-value-id :}
   HIR-OPCODE:INVERT a UNOP RET1
   CLOSE-FUN ;

\ `: LEAF ( a b -- n ) < ;` - compare, set a byte, widen it: one dialect
\ operation and three instructions, because the flags between them are a single
\ architectural resource no value stands for.
: BUILD-CMPSET ( -- )
   2 1 OPEN-FUN
   ARG+ {: a:IR-ID:ir-value-id :}
   ARG+ {: b:IR-ID:ir-value-id :}
   HIR-OPCODE:LT a b BINOP RET1
   CLOSE-FUN ;

\ The same three instructions against a folded literal, again on the second
\ argument so no instruction names rax.
: BUILD-CMPSETI ( -- )
   2 1 OPEN-FUN
   ARG+ {: a:IR-ID:ir-value-id :}
   ARG+ {: b:IR-ID:ir-value-id :}
   HIR-OPCODE:SUB b a BINOP {: x:IR-ID:ir-value-id :}
   HIR-OPCODE:LT x 1000 CONSTOP BINOP RET1
   CLOSE-FUN ;

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

\ ---- running selection, allocation, validation and emission ------------------
\ The contract every case emits under: a leaf computing in this machine's nine
\ allocatable general registers, returning to its caller, reserving no frame and
\ calling nothing.
: LEAF ( -- NEFF:routine )
   NEFF-CONV:REGISTER NEFF:SEQ-NONE NEFF:SEQ-NONE X64ABI:SCRATCH
   NEFF:FPR-NONE NEFF:FPR-NONE NEFF:FPR-NONE
   NEFF-NZCV:CLOBBERED NEFF-LINK:ABSENT NEFF-CONTROL:RETURNS
   NEFF:TRAITS-NONE 0 0 X64M:MACHINE NEFF:ROUTINE ;

\ The emitter is bound to the module about to be written at the same moment the
\ allocator and the validator are: a module's opcode and key identities are its
\ own ordinals, so all three passes take them from it once.
: SELECTED ( -- IR-BUILD:module )
   CC BB X64SEL:BIND-SOURCE
   CC BB IR-BUILD:FREEZE {: m:IR-BUILD:module :}
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
\ it is about the placement itself: a displacement to another word's entry is
\ measured from it, and zero is the one that leaves the entry's own number in
\ the bytes.
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
   CC BB X64SEL:BIND-SOURCE
   CC BB IR-BUILD:FREEZE {: m:IR-BUILD:module :}
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

: CALL-ALLOCATED ( -- IR-BUILD:module )
   CC BB X64SEL:BIND-SOURCE
   CC BB IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   X64-BUILDER {: xb:IR-BUILD:builder :}
   CC xb X64M:MACHINE  CC xb X64IR:VOCABULARY  A64RA:BIND-DIALECT
   CC xb  CC xb X64IR:VOCABULARY  A64RAV:BIND-DIALECT
   CC xb X64EMIT:BIND-DIALECT
   CC m xb DCALL X64SEL:SELECT {: sel:IR-BUILD:module :}
   CC sel DCALL A64RA:ALLOCATE
   sel DCALL A64RAV:ACCEPT
   sel ;

\ ---- and the contract a routine leaves through its callee under --------------
\ One cell in and one out, control leaving through the callee: the pointer never
\ moves, so every cell the callee reads is a cell this routine was entered with.
\ test/compiler/x64-regalloc.f allocates the same shape under it.
: DTAIL ( -- NEFF:routine )
   X64ABI:SCRATCH 1 1 X64ABI:TAIL ;

: TAIL-ALLOCATED ( -- IR-BUILD:module )
   CC BB X64SEL:BIND-SOURCE
   CC BB IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   X64-BUILDER {: xb:IR-BUILD:builder :}
   CC xb X64M:MACHINE  CC xb X64IR:VOCABULARY  A64RA:BIND-DIALECT
   CC xb  CC xb X64IR:VOCABULARY  A64RAV:BIND-DIALECT
   CC xb X64EMIT:BIND-DIALECT
   CC m xb DTAIL X64SEL:SELECT {: sel:IR-BUILD:module :}
   CC sel DTAIL A64RA:ALLOCATE
   sel DTAIL A64RAV:ACCEPT
   sel ;

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
: NOT-BYTES ( IR-CTX:ctx -- )      HIR-MOD BUILD-NOT EMITTED ;
: CMPSET-BYTES ( IR-CTX:ctx -- )   HIR-MOD BUILD-CMPSET EMITTED ;
: CMPSETI-BYTES ( IR-CTX:ctx -- )  HIR-MOD BUILD-CMPSETI EMITTED ;
: MOVI-BYTES ( IR-CTX:ctx -- )     HIR-MOD BUILD-MOVI EMITTED ;

: DDIFF-BYTES ( IR-CTX:ctx -- )    HIR-MOD BUILD-DIFF 2 1 DSTACK-EMITTED ;
: DADDR-BYTES ( IR-CTX:ctx -- )    HIR-MOD BUILD-DADDRESSED 1 1 DSTACK-EMITTED ;
: DLOOP-BYTES ( IR-CTX:ctx -- )    HIR-MOD BUILD-LOOP 2 1 DSTACK-EMITTED ;

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

\ The sealed emission's own readers, taken on the routine that has one site.
\ Every number here is a BYTE count or a BYTE offset, which is what separates
\ these readers from the ARM64 emitter's: there an instruction is four bytes and
\ the answers are instruction indices.
: RELOC-FACTS ( -- n n n n n n bool )
   X64EMIT:SIZE  X64EMIT:BLOCKS  0 X64EMIT:FUNCTION-OFFSET@
   X64EMIT:ADDR-SITES  0 X64EMIT:ADDR-SITE@  0 X64EMIT:ADDR-SITE-KIND@
   X64EMIT:TRAILING-RETURN? ;

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
\ Bound, placed, sealed, retired. Each of these asks for a step out of turn.
: EMIT-UNPLACED ( -- )
   ALLOCATED {: m:IR-BUILD:module :}
   CC m X64EMIT:EMIT ;

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

: STATE-CASES ( IR-CTX:ctx -- )
   HIR-MOD
   BUILD-DIFF
   s" an emission whose calls would be measured from a placement it was never given is refused before a byte is written" T-LABEL
   [: EMIT-UNPLACED ;] E-X64EMIT-STATE TTHROWSQ
   X64EMIT:RETIRE
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

   s" the complement" T-LABEL
   WBND [: NOT-BYTES ;] IR-CTX:WITH-CONTEXT
   \ mc: notq %rax
   \ mc: retq
   s" 48f7d0c3" X=

   s" compare, set and widen" T-LABEL
   WBND [: CMPSET-BYTES ;] IR-CTX:WITH-CONTEXT
   \ mc: cmpq %rcx, %rax
   \ mc: setl %al
   \ mc: movzbq %al, %rax
   \ mc: retq
   s" 4839c80f9cc0480fb6c0c3" X=

   s" compare against a folded literal, set and widen" T-LABEL
   WBND [: CMPSETI-BYTES ;] IR-CTX:WITH-CONTEXT
   \ mc: subq %rax, %rcx
   \ mc: cmpq $1000, %rcx
   \ mc: setl %al
   \ mc: movzbq %al, %rax
   \ mc: retq
   s" 4829c14881f9e80300000f9cc0480fb6c0c3" X=

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
   X64EMIT:TRAILING-RETURN? TTRUE
   X64EMIT:RETIRE

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
   X64EMIT:TRAILING-RETURN? TTRUE
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
   s" 4981ec08000000498b042449894424084981c410000000e8e4ffffff4981ec10000000498b442408498904244981c408000000c3" X=

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
   s" 4981ec08000000498b042449894424084981c410000000e8d40300004981ec10000000498b442408498904244981c408000000c3" X=

   s" the routine that leaves through its callee: one tail branch and no return" T-LABEL
   WBND [: TAIL-BYTES ;] IR-CTX:WITH-CONTEXT
   \ mc: jmp 1019
   s" e9fb030000" XB=
   X64EMIT:TRAILING-RETURN? TFALSE
   X64EMIT:RETIRE

   s" the relocatable literal, and the site the emission recorded for it" T-LABEL
   WBND [: RELOC-BYTES ;] IR-CTX:WITH-CONTEXT
   \ mc: movabsq $3665428480, %rax
   \ mc: retq
   s" 48b800007ada00000000c3" XB=
   RELOC-FACTS
   TTRUE                                 \ it ends in a return
   X64IR:ADDR-DATA T=                    \ the site is a data address
   0 T=                                  \ at byte zero, the `mov r64, imm64` itself
   1 T=                                  \ one site, because ADDR-LANES is one
   0 T=                                  \ the function starts where the emission does
   1 T=                                  \ one block
   11 T=                                 \ eleven bytes of it
   s" the one site this routine has is the only one a reader answers about" T-LABEL
   [: SITE-PAST-END ;] E-X64EMIT-BOUND TTHROWSQ
   X64EMIT:RETIRE

   WBND [: ADDRESSED-CASES ;] IR-CTX:WITH-CONTEXT
   WBND [: STATE-CASES ;] IR-CTX:WITH-CONTEXT
   WBND [: MACHINE-REFUSE-CASES ;] IR-CTX:WITH-CONTEXT
   WBND [: AFTER-REFUSE-CASES ;] IR-CTX:WITH-CONTEXT
   WBND [: FAR-REFUSE-CASES ;] IR-CTX:WITH-CONTEXT
   WBND [: UNACCEPTED-CASES ;] IR-CTX:WITH-CONTEXT

   T-REPORT ;

;package

X64EMIT-TEST:RUN

\ x64-emit.f - the first x86-64 bytes: src/compiler/native/emit-x64.f (X64EMIT)
\ over the straight-line leaf routines src/compiler/native/select-x64.f selects
\ and the allocator and its validator accept.
\
\ HOW THE BYTES ARE PINNED. Each case emits into one BUF sink and the WHOLE byte
\ string of the routine is compared against a fixed expectation, the way
\ test/compiler/x86-64-asm.f pins one encoder at a time. The `mc:` comment above
\ each pinned line is the instruction that byte group is, one mnemonic a line, as
\ `llvm-mc -triple=x86_64 -disassemble` read the pinned string back (LLVM 22.1.8,
\ 2026-09-21); every line was then re-assembled with
\ `llvm-mc -triple=x86_64 -show-encoding` and answered exactly the bytes pinned
\ here, WITH THE ONE EXCEPTION NAMED BELOW. That agreement is not free:
\ src/arch/x86-64/asm.f implements exactly one encoding per operation and none
\ of llvm-mc's preferred short forms (05 id for `add rax`, D1 /n for a shift by
\ one, 83 /n ib for an ALU immediate that fits a signed byte), so the fixtures
\ below fold their immediates onto the SECOND argument, shift by a count other
\ than one, name rax as the destination of no immediate form, and hold every
\ folded literal outside a signed byte.
\
\ THE EXCEPTION IS THE DATA-STACK ADJUSTMENT, which no fixture can dodge: a
\ pointer move is a small multiple of eight and therefore always fits the byte
\ form llvm-mc prefers, while `x64.dtake` and `x64.dpublish` are the imm32 add
\ and subtract of this dialect whatever the distance (emit-x64.f PUT-DMOVE).
\ llvm-mc DISASSEMBLES the pinned 49 81 ec 10 00 00 00 as the `subq $16, %r12`
\ its `mc:` line gives, and re-ASSEMBLES that line as the four-byte
\ 49 83 ec 10: on those lines - the `subq` and the `addq` on %r12 in the last
\ two cases - the instruction agrees and the encoding is the longer one this
\ encoder has.
\
\ THE FIXTURES ARE THE TWO CONTRACTS x64-regalloc.f ALLOCATES UNDER. Most are
\ the REGISTER-convention leaf: the contract names no place, so the module is
\ the body alone and every value in it is one the allocator is free to place.
\ The last two are the DATA-STACK convention (X64ABI:LEAF), where the interface
\ is caller cells and the selector writes the boundary itself - the only shape
\ the four data-stack crossings and the four addressed forms appear in, because
\ the order an addressed form needs is the one the entry's `x64.dtake` minted
\ and the exit's `x64.dpublish` consumes. ONE FIXTURE PER CONTEXT, and the
\ refusing cases run inside an enclosing one, for the reason that suite gives.
\
\ WHAT THIS SUITE DOES NOT PIN. Every form this emitter writes now has a byte
\ string here. What is left unmeasured is a byte no module of this slice can
\ produce, and each of them is a later slice's:
\
\ - THE REGISTERS ABOVE rdx. The pool is rax, rcx, rdx, rsi, rdi and r8..r11
\   (x64ir.f RESERVED-MASK), and no fixture here is wide enough for the
\   allocator to reach past rdx, so no pinned byte sets REX.R or REX.B for an
\   operand and none names the spl/bpl/sil/dil byte registers.
\ - A DATA-STACK DISPLACEMENT OUTSIDE disp8, or a negative one. `x64.dslot` is
\   signed and reaches disp32 (x64ir.f DSLOT), which wants a contract of more
\   than sixteen cells or a routine that leaves through its callee - and a
\   routine that calls is E-X64EMIT-SHAPE here.
\ - THE FRAME AND THE BRANCH. `x64.reserve`, `release`, `store` and `load` need
\   a prologue and a branch needs a layout, and both are refused by name in the
\   cases below.

require lib/test.f
require lib/byte-buffer.f
require src/compiler/native/select-x64.f
require src/compiler/native/regalloc.f
require src/compiler/native/regalloc-verify.f
require src/compiler/native/emit-x64.f
require src/arch/x86-64/abi.f
require src/arch/x86-64/machine.f

\ The span reader answers a role; a byte comparison takes a raw cell. The same
\ projection test/compiler/x86-64-asm.f takes, for the same reason.
CAST: X64E-BL>RAW ( NUM:byte-len -- n )

package X64EMIT-TEST
private

\ ---- the sink ----------------------------------------------------------------
\ One sink for the whole suite: each case emits into it, is compared, and clears
\ it, so a case can never read a neighbour's bytes.
create SINK BUF:HDR-BYTES allot

: N>BLEN ( n -- NUM:byte-len )
   NUM:BYTE-LEN
   MATCH NUM:numeric-result
      ok OF ENDOF                                negative OF E-BUF-BOUNDS throw ENDOF
      zero OF E-BUF-BOUNDS throw ENDOF           overflow OF E-BUF-BOUNDS throw ENDOF
      underflow OF E-BUF-BOUNDS throw ENDOF      bad-alignment OF E-BUF-BOUNDS throw ENDOF
      misaligned OF E-BUF-BOUNDS throw ENDOF
   ;MATCH ;

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
\ then empty the sink for the next case.
: X= ( ptr u8 n -- ) {: ea:ptr eu:n :}
   SINK BUF:SPAN$ X64E-BL>RAW {: da:ptr dlen:n :}
   da dlen ea eu SPAN=HEX? TTRUE
   SINK BUF:CLEAR ;

: SINK-CLEAR ( -- )  SINK BUF:CLEAR ;

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

\ `: LEAF ( a b -- n ) < if a else b then ;` as the elaborator leaves it: a
\ compare and a two-way branch, then an arm each. Three blocks, which is the
\ shape this slice refuses.
: BUILD-BRANCH ( -- )
   2 1 OPEN-FUN
   ARG+ {: a:IR-ID:ir-value-id :}
   ARG+ {: b:IR-ID:ir-value-id :}
   HIR-OPCODE:LT a b BINOP {: f:IR-ID:ir-value-id :}
   f 1 2 BRZ2
   BLOCK+
   a RET1
   BLOCK+
   b RET1
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
: MMEMT ( -- IR-ID:ir-type-id )     CC MB X64IR:MEM-TYPE ;

: M-OPEN ( X64IR:opcode -- )
   {: o:X64IR:opcode :}
   CC MB  CC MB o X64IR:OPCODE  IR-BUILD:BEGIN-OP
   CC MB  MSPN  IR-BUILD:SET-OP-SPAN ;

: M-ATTR+ ( IR-ID:ir-symbol-id IR-ID:ir-attr-id -- )
   {: k:IR-ID:ir-symbol-id v:IR-ID:ir-attr-id :}
   CC MB k v IR-BUILD:ADD-ATTR ;

: M-FUN ( IR-ID:ir-type-id -- )
   {: sig:IR-ID:ir-type-id :}
   CC MB  CC MB s" LEAF" IR-BUILD:INTERN-SYMBOL  IR-BUILD:BEGIN-FUN
   CC MB  sig  IR-BUILD:SET-SIGNATURE
   CC MB IR--FUN-LINKAGE:DEFINED IR-BUILD:SET-LINKAGE
   CC MB IR--FUN-VISIBILITY:EXPORTED IR-BUILD:SET-VISIBILITY
   CC MB IR--FUN-CONVENTION:HABU IR-BUILD:SET-CONVENTION
   CC MB  MSPN  IR-BUILD:SET-FUN-SPAN
   CC MB IR-BUILD:BEGIN-BLOCK
   CC MB  MSPN  IR-BUILD:SET-BLOCK-SPAN ;

: M-CLOSE ( -- IR-BUILD:module )
   CC MB IR-BUILD:END-BLOCK drop
   CC MB IR-BUILD:END-FUN drop
   CC MB IR-BUILD:FREEZE ;

: M-MOD ( -- )
   X64-BUILDER {: b:IR-BUILD:builder :}
   b 0 M-BLD !
   CC b TXT TXT-N IR-BUILD:ADD-SOURCE 0 M-SRC !
   CC b X64IR:REGISTER
   CC MB X64EMIT:BIND-DIALECT ;

\ `( tok -- )` as a call site: the memory order arrives as the routine's
\ argument, the call passes it on and the return ends the block.
: CALL-SIGN ( -- IR-ID:ir-type-id )
   IR-TYPE:FN-BEGIN
   MMEMT IR-TYPE:FN-PARAM
   CC MB IR-BUILD:INTERN-CODE-REF ;

: BUILD-CALLER ( -- IR-BUILD:module )
   M-MOD
   CALL-SIGN M-FUN
   CC MB MMEMT IR-BUILD:ADD-BLOCK-ARG {: k:IR-ID:ir-value-id :}
   X64IR-OPCODE:WORDCALL M-OPEN
   CC MB k IR-BUILD:ADD-OPERAND
   CC MB MMEMT IR-BUILD:ADD-RESULT
   CC MB X64IR:KEY-DBYTES  CC MB 0 X64IR:DBYTES-ATTR  M-ATTR+
   CC MB X64IR:KEY-DBACK   CC MB 0 X64IR:DBACK-ATTR   M-ATTR+
   CC MB X64IR:KEY-ENTRY   CC MB 64 X64IR:ENTRY-ATTR  M-ATTR+
   CC MB IR-BUILD:END-OP drop
   X64IR-OPCODE:RET M-OPEN
   CC MB IR-BUILD:END-OP drop
   M-CLOSE ;

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

: EMITTED ( -- )
   ALLOCATED SINK X64EMIT:EMIT ;

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
   DSTACK-ALLOCATED SINK X64EMIT:EMIT ;

\ A call site is refused for the module's SHAPE, which is asked before any
\ assignment, so this module needs none.
: CALLER-EMIT ( -- )
   BUILD-CALLER SINK X64EMIT:EMIT ;

\ A form outside the slice is refused only once the shape and the assignment
\ have been agreed, so this module is allocated and accepted like any other. The
\ allocator and the validator are bound inside BUILD-NEGATOR, because a binding
\ reads the BUILDER and a frozen builder handle answers nothing.
: NEGATOR-EMIT ( -- )
   BUILD-NEGATOR {: m:IR-BUILD:module :}
   CC m LEAF A64RA:ALLOCATE
   m LEAF A64RAV:ACCEPT
   m SINK X64EMIT:EMIT ;

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
\ Neither refusing case allocates, so each hands the allocator's dialect binding
\ back itself: the walk is what takes a binding, and a second binding over a
\ live one is E-A64RA-BIND.
\
\ A routine of three blocks is refused for its SHAPE before a byte is written:
\ every displacement between them would be measured against a layout this slice
\ has not got. It is selected and not allocated because the allocator refuses
\ this module too - a leaf whose control leaves through more than one block is
\ E-A64RA-SHAPE - and the shape is a question about the module alone, which is
\ why the emitter asks it before it asks about the assignment at all. The other
\ case is a well-shaped module nobody allocated: the accepted assignment is
\ another module's and the registers of this one were never handed out.
: UNALLOCATED-EMIT ( -- )
   SELECTED {: m:IR-BUILD:module :}
   A64RA:RELEASE
   m SINK X64EMIT:EMIT ;

: ADDRESSED-CASES ( IR-CTX:ctx -- )
   HIR-MOD
   BUILD-ADDRESSED
   s" the same four forms under the REGISTER convention are refused: nothing reads the order the last store answers, there being no publish" T-LABEL
   [: ADDRESSED-ACCEPT ;] E-A64RAV-ORDER TTHROWSQ ;

\ The two machine-dialect modules, in the context they were built in.
: MACHINE-REFUSE-CASES ( IR-CTX:ctx -- )
   0 W-CTX !
   s" a routine that calls is refused for its shape: a call needs an address to measure its displacement from, which a slice with no layout has not got" T-LABEL
   [: CALLER-EMIT ;] E-X64EMIT-SHAPE TTHROWSQ
   s" a form outside this slice is refused by name in a routine whose shape and assignment are both agreed" T-LABEL
   [: NEGATOR-EMIT ;] E-X64EMIT-FORM TTHROWSQ
   SINK-CLEAR ;

: REFUSE-CASES ( IR-CTX:ctx -- )
   HIR-MOD
   BUILD-BRANCH
   s" a routine of more than one block is refused: a branch's displacement needs the layout pass this slice has not got" T-LABEL
   [: UNALLOCATED-EMIT ;] E-X64EMIT-SHAPE TTHROWSQ
   SINK-CLEAR ;

: UNACCEPTED-CASES ( IR-CTX:ctx -- )
   HIR-MOD
   BUILD-DIFF
   s" a module the accepted allocation is not about is refused before a byte is written" T-LABEL
   [: UNALLOCATED-EMIT ;] E-X64EMIT-ACCEPT TTHROWSQ
   SINK-CLEAR ;

public

: RUN ( -- )
   T-RESET
   SINK 64 N>BLEN BUF:INIT

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

   WBND [: ADDRESSED-CASES ;] IR-CTX:WITH-CONTEXT
   WBND [: MACHINE-REFUSE-CASES ;] IR-CTX:WITH-CONTEXT
   WBND [: REFUSE-CASES ;] IR-CTX:WITH-CONTEXT
   WBND [: UNACCEPTED-CASES ;] IR-CTX:WITH-CONTEXT

   SINK BUF:DISPOSE
   T-REPORT ;

;package

X64EMIT-TEST:RUN

\ emit.f - turn one accepted straight-line A64IR module into the ARM64 bytes
\ that are the machine's own reading of it, and a map from every emitted byte
\ back to the source it came from.
\
\ docs/compiler-ir-design.md section 7.11 ("A64ENC consumes physical A64IR and
\ calls the existing encoder words") and section 11.3, which puts the
\ encoder/fixup check among the things the JIT path runs before anything becomes
\ executable. Everything before this pass names values, registers and forms;
\ after it there is nothing left to name, only bytes. This file owns exactly that
\ last step: which four bytes each operation is, where in the buffer they sit,
\ and which source span they answer for.
\
\ WHAT THIS FILE DOES NOT DECIDE. It does not decide what the program computes -
\ that is the selector's - and it does not decide which register holds which
\ value. It asks. The only door to a register is A64RAV:REG@, which answers only
\ after the independent validator has agreed with the module and stops answering
\ the moment a later allocation replaces the one that was agreed with. Nothing
\ here reads A64RA's claims, so there is no route by which an unchecked
\ assignment reaches an instruction. It does not decide the bit layout of an
\ instruction either: every word below comes out of src/arch/arm64/asm.f, which
\ is the one authority on each form, and no encoding constant is written here.
\ It does not lay out labels or fixups, because the straight-line subset has one
\ control transfer and it is the return; a label table with no branch to resolve
\ would be machinery kept warm for a caller that does not exist yet. When
\ branches arrive, so does it (dot habu-lay-out-branches-7e04eab2).
\
\ WHAT IT REFUSES, AND WHY EACH ONE IS ITS OWN JUDGEMENT.
\   - an unbound dialect, or a second binding over a live one. A module's symbols
\     are its own ordinals, so "is this operation a move-wide" has no answer from
\     outside without either the dialect's authority or a second copy of its
\     spellings. This pass asks A64IR while the module is still being built, the
\     same way src/compiler/native/regalloc.f does, and keeps the identities.
\   - a frozen module that is not the one the binding was taken over.
\   - a register assignment nobody accepted, or one accepted for another module.
\     The stale case answers itself: the emitter probes REG@ before it emits a
\     byte, so an acceptance a later allocation invalidated is refused by
\     A64RAV under A64RAV's own name rather than quietly re-read here.
\   - a shape that is not the straight-line subset, re-derived from the module
\     rather than taken on trust: one function, one block, and a block whose only
\     terminator is its last operation. The freeze verifier already forbids a
\     block with two terminators or one that is not last; this pass measures it
\     again because it is about to lay the operations out in that order.
\   - an operation of a form outside the dialect's family. An unmodelled form may
\     mean something this file does not know, and guessing is how the wrong four
\     bytes get published.
\   - a caller reading an emission that never happened, or an index past its end.
\
\ WHAT IT DOES NOT RE-CHECK, DELIBERATELY. It does not bound a register number, a
\ move-wide immediate or a half selector. Every encoder in src/arch/arm64/asm.f
\ refuses an operand that does not fit its field before it packs a single bit,
\ and its bounds are written once there, per field. A second copy of those bounds
\ here would be a second authority that can drift from the fields it describes,
\ so the operands go to the encoder as they are and the encoder's refusal is the
\ refusal. The one arithmetic this file does is turning the dialect's shift - a
\ number of bits - into the half selector the encoding holds, and it divides
\ through the assembler's own SCALE/, which refuses a value the division would
\ round instead of silently encoding the half below it.
\
\ TWO REFUSALS HERE ARE FAIL-CLOSED RATHER THAN REACHABLE, AND SAY SO. A move-wide
\ operation with no attribute under a declared key (E-A64EMIT-ATTR) cannot reach
\ this file: the freeze verifier decides an operation's attribute keys against its
\ schema, so a frozen module always carries exactly one of each. More instructions
\ than the buffers hold (E-A64EMIT-CAP) cannot either: one instruction per
\ operation, one value per non-terminator operation, and the allocator refuses a
\ block with more values than the ceiling above. Both are still written, because a
\ search and a buffer need an answer for the case they cannot serve, and neither
\ is claimed to be tested. What the assembler's own guards catch is a live gap and
\ not a foreclosed one: a module built by hand can carry a raw out-of-field
\ move-wide attribute that freezes and verifies, because a schema cannot yet
\ declare an attribute's value domain (dot habu-declare-an-attr-a14961ae), and the
\ assembler answers it by ending the process rather than by throwing (dot
\ habu-make-the-arm64-fa89e081).
\
\ THE FRAME FORMS ARE FOUR MORE INSTRUCTIONS AND NOTHING ELSE. A store, a load,
\ and the subtraction and addition that take the routine's frame and give it back
\ each encode exactly like every other form here: through the assembler's own
\ encoder, with the register the accepted allocation answers and the slot or size
\ the operation carries. The memory token those operations thread is an ordering
\ dependency and not a machine object, so it reaches no encoder at all - it is
\ read by the passes that have to keep the accesses in order, and it occupies no
\ register and no byte. Nothing here decides where a spill goes either: the slot
\ is the operation's own field, decided by the allocator and checked by the
\ validator before this pass runs.
\
\ THE SOURCE MAP IS THE POINT OF THE BYTE OFFSETS. Every emitted instruction gets
\ one row: the byte offset it was placed at, and the span of the operation it
\ came from. The offset is the cursor at the moment the instruction was appended,
\ not four times its index, so a run that emitted one instruction too few or too
\ many is visible in the map and not only in the length. This is what a located
\ diagnostic about emitted code reads, and it is why the bytes are a byte buffer
\ with a little-endian placement of each word rather than an array of words: the
\ offsets have to index something a caller can actually look at.
\
\ ONE EMISSION AT A TIME. The buffers are fixed package-owned storage rather than
\ heap objects, so this pass emits one block at a time - the single-task
\ compilation discipline the rest of the native chain keeps. A run that refuses
\ leaves no sealed emission behind, so a reader after a refusal answers nothing
\ rather than answering about the run before it.

require lib/prelude.f
require lib/errors.f
require src/compiler/a64-effect.f
require src/compiler/target.f
require src/compiler/binding.f
require src/compiler/ir/id.f
require src/compiler/ir/context.f
require src/compiler/ir/arena.f
require src/compiler/ir/attr.f
require src/compiler/ir/source.f
require src/compiler/ir/op.f
require src/compiler/ir/fun.f
require src/compiler/ir/build.f
require src/compiler/native/a64ir.f
require src/compiler/native/regalloc.f
require src/compiler/native/regalloc-verify.f
require src/arch/arm64/asm.f

package A64EMIT
private

\ ---- the bound dialect -------------------------------------------------------
\ One slot per member of the operation family, so the family stays exhaustive: a
\ member added to A64IR:opcode makes this fail to compile until it has a slot and
\ an encoding.
10 constant OPCODES-N
0 constant O-MOVZ
1 constant O-MOVK
2 constant O-ADD
3 constant O-SUB
4 constant O-MUL
5 constant O-STORE
6 constant O-LOAD
7 constant O-RESERVE
8 constant O-RELEASE
9 constant O-RET

0 constant BOUND-NO
1 constant BOUND-YES

\ ---- how much of one block this pass holds -----------------------------------
\ Values in one block. The selector and the allocator carry the same ceiling, so
\ a module either of them accepted always fits; a block that wants more is a
\ capability to raise in all three, not a ceiling to widen silently.
256 constant VMAX

\ One instruction per operation. Two forms define no value - the return and the
\ release of the frame - and every other operation defines at least one, so a
\ block that fits the ceiling above emits at most this many.
VMAX 2 + constant INSN-MAX

\ Every ARM64 instruction is four bytes.
4 constant INSN-BYTES

\ ---- the frozen tables of the module being read ------------------------------
5 constant VIEWS-N
0 constant V-OPP                     \ operation pool
1 constant V-OPR                     \ operation rows
2 constant V-FUNR                    \ function rows
3 constant V-BLKR                    \ block rows
4 constant V-ATTR                    \ attribute rows

\ ---- emission state ----------------------------------------------------------
0 constant ST-EMPTY
1 constant ST-SEALED

here CELL 1- and CELL swap - CELL 1- and allot
variable BND-MODE
BOUND-NO BND-MODE !
variable ST
ST-EMPTY ST !
variable N-INS
0 N-INS !

1 TYPED-BUFFER BND-MOD IR-ID:ir-module-id
OPCODES-N TYPED-BUFFER BND-OP IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-IMM IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-SH IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-SLOT IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-FRAME IR-ID:ir-symbol-id

1 TYPED-BUFFER S-KEY IR-ID:ir-module-key
VIEWS-N TYPED-BUFFER S-VIEW IR-ARENA:view

\ The emitted bytes, and one source-map row per emitted instruction.
create CODE INSN-MAX INSN-BYTES * allot
create M-OFF INSN-MAX cells allot
create M-ST INSN-MAX cells allot
create M-LN INSN-MAX cells allot
INSN-MAX TYPED-BUFFER M-SRC IR-ID:ir-source-id

\ ---- the slots, read back ----------------------------------------------------
: KEY ( -- IR-ID:ir-module-key )     0 S-KEY @ ;
: VW ( n -- IR-ARENA:view )          S-VIEW @ ;

\ ---- identity ----------------------------------------------------------------
\ Two symbols are the same when they are the same ordinal of the same module.
\ Nothing here compares spellings.
: SAME-SYM? ( IR-ID:ir-symbol-id IR-ID:ir-symbol-id -- bool )
   {: x:IR-ID:ir-symbol-id y:IR-ID:ir-symbol-id :}
   x IR-ID:SYMBOL-LOCAL y IR-ID:SYMBOL-LOCAL <> if false exit then
   x IR-ID:SYMBOL-OWNER y IR-ID:SYMBOL-OWNER IR-ID:MODULE-SAME? ;

\ ---- the dialect's operation family ------------------------------------------
: SLOT-OF ( A64IR:opcode -- n )
   MATCH A64IR:opcode
      movz    OF O-MOVZ    ENDOF
      movk    OF O-MOVK    ENDOF
      add     OF O-ADD     ENDOF
      sub     OF O-SUB     ENDOF
      mul     OF O-MUL     ENDOF
      store   OF O-STORE   ENDOF
      load    OF O-LOAD    ENDOF
      reserve OF O-RESERVE ENDOF
      release OF O-RELEASE ENDOF
      ret     OF O-RET     ENDOF
   ;MATCH ;

: SLOT-OPCODE ( n -- A64IR:opcode )
   case
      O-MOVZ    of A64IR-OPCODE:MOVZ    endof
      O-MOVK    of A64IR-OPCODE:MOVK    endof
      O-ADD     of A64IR-OPCODE:ADD     endof
      O-SUB     of A64IR-OPCODE:SUB     endof
      O-MUL     of A64IR-OPCODE:MUL     endof
      O-STORE   of A64IR-OPCODE:STORE   endof
      O-LOAD    of A64IR-OPCODE:LOAD    endof
      O-RESERVE of A64IR-OPCODE:RESERVE endof
      O-RELEASE of A64IR-OPCODE:RELEASE endof
      O-RET     of A64IR-OPCODE:RET     endof
      E-A64EMIT-OPCODE throw
   endcase ;

\ Which member of the family this symbol names. An operation of a form outside it
\ has no encoding here and is refused rather than skipped or guessed at.
: OPCODE-SLOT ( IR-ID:ir-symbol-id -- n )
   {: sym:IR-ID:ir-symbol-id :}
   -1
   OPCODES-N 0 ?do
      sym i BND-OP @ SAME-SYM? if drop i leave then
   loop
   dup 0 < if E-A64EMIT-OPCODE throw then ;

\ ---- reading the frozen module -----------------------------------------------
: OP-AT ( IR-ID:ir-block-id n -- IR-ID:ir-op-id )
   {: bk:IR-ID:ir-block-id i:n :}
   V-BLKR VW V-OPR VW KEY bk i IR-FUN:FOP@ ;

: SLOT-AT ( IR-ID:ir-op-id -- n )
   V-OPR VW KEY rot IR-OP:FOPCODE@ OPCODE-SLOT ;

: OPERAND-AT ( IR-ID:ir-op-id n -- IR-ID:ir-value-id )
   {: id:IR-ID:ir-op-id i:n :}
   V-OPP VW V-OPR VW KEY id i IR-OP:FOPERAND@ ;

: RESULT-AT ( IR-ID:ir-op-id n -- IR-ID:ir-value-id )
   {: id:IR-ID:ir-op-id i:n :}
   V-OPP VW V-OPR VW KEY id i IR-OP:FRESULT@ ;

: OP-COUNT ( IR-ID:ir-block-id -- n )
   V-BLKR VW swap IR-FUN:FOP-COUNT ;

\ ---- the registers, through the one door that answers ------------------------
\ A64RAV:REG@ is the only checked answer in the chain, and it is the only way a
\ register reaches an instruction here.
: REG-OF ( IR-ID:ir-value-id -- n )
   IR-ID:VALUE-LOCAL A64RAV:REG@ ;

: RESULT-REG ( IR-ID:ir-op-id n -- n )
   RESULT-AT REG-OF ;

: OPERAND-REG ( IR-ID:ir-op-id n -- n )
   OPERAND-AT REG-OF ;

\ ---- the move-wide operands --------------------------------------------------
\ The freeze verifier already proves that a move-wide operation carries exactly
\ one attribute under each key its schema declares, so the search below finds
\ one; it refuses rather than reading a neighbouring attribute if it does not.
: ATTR-SLOT ( IR-ID:ir-op-id IR-ID:ir-symbol-id -- n )
   {: id:IR-ID:ir-op-id want:IR-ID:ir-symbol-id :}
   -1
   V-OPR VW id IR-OP:FATTRS {: n:n :}
   n 0 ?do
      V-OPP VW V-OPR VW KEY id i IR-OP:FATTR-KEY@ want SAME-SYM? if drop i leave then
   loop
   dup 0 < if E-A64EMIT-ATTR throw then ;

: ATTR-INT ( IR-ID:ir-op-id IR-ID:ir-symbol-id -- n )
   {: id:IR-ID:ir-op-id want:IR-ID:ir-symbol-id :}
   id want ATTR-SLOT {: k:n :}
   V-ATTR VW  V-OPP VW V-OPR VW KEY id k IR-OP:FATTR@  IR-ATTR:FINT@ ;

: IMM-OF ( IR-ID:ir-op-id -- n )
   0 BND-IMM @ ATTR-INT ;

\ The dialect records the shift as a number of bits; the encoding holds the half
\ it selects. SCALE/ is the assembler's own refusal for a value its division
\ would round, so a shift that names no whole half is refused there rather than
\ quietly encoding the half below it.
: HALF-OF ( IR-ID:ir-op-id -- n )
   0 BND-SH @ ATTR-INT A64IR:HALF-BITS SCALE/ ;

\ ---- the frame operands ------------------------------------------------------
\ A slot is a byte offset from the stack pointer and a reserved frame is a byte
\ count, and both go to the encoder as the bytes they are: ENC-LDR and ENC-STR
\ divide by their own access scale and refuse a value that division would round,
\ and ENC-SUBI and ENC-ADDI bound their own immediate field. No bound is repeated
\ here for the same reason none of the move-wide ones is.
: SLOT-OFF ( IR-ID:ir-op-id -- n )
   0 BND-SLOT @ ATTR-INT ;

: FRAME-SIZE ( IR-ID:ir-op-id -- n )
   0 BND-FRAME @ ATTR-INT ;

\ ---- one instruction per operation -------------------------------------------
\ Each of these is exactly the encoder call the form names, with the registers
\ the accepted allocation answers and the operands the module carries.
: WORD-MOVZ ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id 0 RESULT-REG  id IMM-OF  id HALF-OF  MOVZHW ;

: WORD-MOVK ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id 0 RESULT-REG  id IMM-OF  id HALF-OF  MOVKHW ;

\ The shifted-register three-operand forms differ only in which encoder they end
\ in, so they share the operand reading.
: TRIPLE ( IR-ID:ir-op-id -- n n n )
   {: id:IR-ID:ir-op-id :}
   id 0 RESULT-REG  id 0 OPERAND-REG  id 1 OPERAND-REG ;

\ The frame accesses. Both name the stack pointer as their base, because the
\ frame is where the stack pointer is: the form has no other base and this
\ dialect has no value that could be one. The register moved is the store's one
\ operand and the load's first result, which is what their schemas declare.
: WORD-STORE ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id 0 OPERAND-REG  A64EFF:SP-GPR  id SLOT-OFF  ENC-STR ;

: WORD-LOAD ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id 0 RESULT-REG  A64EFF:SP-GPR  id SLOT-OFF  ENC-LDR ;

\ Taking the frame and giving it back are one subtraction and one addition on the
\ stack pointer, of exactly the size the operation carries.
: WORD-RESERVE ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   A64EFF:SP-GPR A64EFF:SP-GPR  id FRAME-SIZE  ENC-SUBI ;

: WORD-RELEASE ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   A64EFF:SP-GPR A64EFF:SP-GPR  id FRAME-SIZE  ENC-ADDI ;

: WORD-OF ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id SLOT-AT SLOT-OPCODE
   MATCH A64IR:opcode
      movz    OF id WORD-MOVZ ENDOF
      movk    OF id WORD-MOVK ENDOF
      add     OF id TRIPLE ENC-ADD ENDOF
      sub     OF id TRIPLE ENC-SUB ENDOF
      mul     OF id TRIPLE ENC-MUL ENDOF
      store   OF id WORD-STORE ENDOF
      load    OF id WORD-LOAD ENDOF
      reserve OF id WORD-RESERVE ENDOF
      release OF id WORD-RELEASE ENDOF
      ret     OF ENC-RET ENDOF
   ;MATCH ;

\ ---- the buffer and the map --------------------------------------------------
: BYTE! ( n n -- )
   {: v:n off:n :}
   v $FF and  CODE off +  c! ;

: BYTE@ ( n -- n )
   {: off:n :}
   CODE off + c@ ;

\ Little-endian placement, which is what the machine reads and what the map's
\ offsets index.
: WORD! ( n n -- )
   {: w:n off:n :}
   w off BYTE!
   w 8 rshift off 1+ BYTE!
   w 16 rshift off 2 + BYTE!
   w 24 rshift off 3 + BYTE! ;

: MAP! ( IR-ID:ir-op-id n n -- )
   {: id:IR-ID:ir-op-id off:n k:n :}
   off k cells M-OFF + !
   V-OPR VW KEY id IR-OP:FSPAN@ IR--SOURCE-SPAN:UNMAKE
   {: src:IR-ID:ir-source-id st:n ln:n :}
   src k M-SRC !
   st k cells M-ST + !
   ln k cells M-LN + ! ;

\ Append one instruction at the cursor and record where it landed.
: APPEND ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id w:n :}
   N-INS @ INSN-MAX >= if E-A64EMIT-CAP throw then
   N-INS @ {: k:n :}
   k INSN-BYTES * {: off:n :}
   w off WORD!
   id off k MAP!
   k 1+ N-INS ! ;

\ ---- the shape this leaf emits from ------------------------------------------
: VIEWS! ( IR-BUILD:module -- )
   {: m:IR-BUILD:module :}
   m IR-BUILD:FKEY 0 S-KEY !
   m IR-BUILD:FOP-POOL    V-OPP  S-VIEW !
   m IR-BUILD:FOP-ROWS    V-OPR  S-VIEW !
   m IR-BUILD:FFUN-ROWS   V-FUNR S-VIEW !
   m IR-BUILD:FBLOCK-ROWS V-BLKR S-VIEW !
   m IR-BUILD:FATTR-ROWS  V-ATTR S-VIEW ! ;

\ One function of one block; any other shape means control flow, and control flow
\ has no layout rule here yet.
: BLOCK-OF ( -- IR-ID:ir-block-id )
   V-FUNR VW IR-FUN:FFUNS 1 <> if E-A64EMIT-SHAPE throw then
   KEY 0 IR-ID:PACK-FUN {: f:IR-ID:ir-fun-id :}
   V-FUNR VW f IR-FUN:FBLOCK-COUNT 1 <> if E-A64EMIT-SHAPE throw then
   V-FUNR VW V-BLKR VW KEY f 0 IR-FUN:FBLOCK@ ;

\ The block's operations run in one order and end once. Re-derived rather than
\ taken from the verifier, because this pass is about to lay them out in exactly
\ that order.
: TERMINATOR? ( IR-ID:ir-block-id n -- bool )
   OP-AT SLOT-AT O-RET = ;

: STRAIGHT-CK ( IR-ID:ir-block-id -- )
   {: bk:IR-ID:ir-block-id :}
   bk OP-COUNT {: n:n :}
   n 1 < if E-A64EMIT-SHAPE throw then
   bk n 1- TERMINATOR? 0= if E-A64EMIT-SHAPE throw then
   n 1- 0 ?do
      bk i TERMINATOR? if E-A64EMIT-SHAPE throw then
   loop ;

: WALK ( IR-ID:ir-block-id -- )
   {: bk:IR-ID:ir-block-id :}
   bk OP-COUNT {: n:n :}
   n 0 ?do
      bk i OP-AT {: id:IR-ID:ir-op-id :}
      id  id WORD-OF  APPEND
   loop ;

\ ---- what one emission run is told -------------------------------------------
\ The binding is taken whatever the outcome, so neither an emission without a
\ binding nor a refused emission can leave one behind for the next caller.
: BND-TAKE ( -- )
   BND-MODE @ {: have:n :}
   BOUND-NO BND-MODE !
   have BOUND-YES <> if E-A64EMIT-BIND throw then ;

: BND-MODULE-CK ( IR-BUILD:module -- )
   IR-BUILD:FMODULE  0 BND-MOD @  IR-ID:MODULE-SAME?
   0= if E-A64EMIT-MODULE throw then ;

: BIND1 ( IR-CTX:ctx IR-BUILD:builder A64IR:opcode -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder o:A64IR:opcode :}
   c b o A64IR:OPCODE  o SLOT-OF BND-OP ! ;

\ A module whose schema table was created for another dialect, or for another
\ version of this one, holds operations whose encodings this pass does not know
\ even if some of them happen to be spelled the same.
: DIALECT-CK ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b  c b IR-BUILD:DIALECT@  A64IR:NAME IR-BUILD:SYMBOL-IS?
   0= if E-A64EMIT-MODULE throw then
   c b IR-BUILD:SCHEMA-MAJOR@ A64IR:MAJOR <> if E-A64EMIT-MODULE throw then
   c b IR-BUILD:SCHEMA-MINOR@ A64IR:MINOR <> if E-A64EMIT-MODULE throw then ;

\ These instructions belong to one architecture. A context bound to another
\ machine describes a processor that executes none of them.
: TARGET-CK ( IR-CTX:ctx -- )
   IR-CTX:BINDING@ CBIND:VALIDATE CBIND:TARGET@ CTARGET:ARCH@
   CTARGET-ARCH:AARCH64 CTARGET-ARCH:EQ
   0= if E-A64EMIT-TARGET throw then ;

\ The register assignment this run will read. The probe is what makes staleness
\ a refusal before a byte is written rather than halfway through: an acceptance a
\ later allocation replaced stops answering, under A64RAV's own name. It asks
\ whether the first value is one that lives in a register rather than which
\ register it is in, because the first value of a module that spills is the
\ memory token the frame forms thread and that has no register to answer. What is
\ left for this pass to judge is whether the accepted assignment is about the
\ module it is being asked to emit.
: ALLOC-CK ( IR-BUILD:module -- )
   {: m:IR-BUILD:module :}
   A64RAV:ACCEPTED? 0= if E-A64EMIT-ALLOC throw then
   A64RA:VALUES 0 > if 0 A64RAV:REGISTERED? drop then
   m IR-BUILD:FMODULE A64RA:MODULE@ IR-ID:MODULE-SAME?
   0= if E-A64EMIT-ALLOC throw then ;

: SEAL-CK ( -- )
   ST @ ST-SEALED <> if E-A64EMIT-STATE throw then ;

: ORD-CK ( n -- n )
   dup 0 < over N-INS @ >= or if E-A64EMIT-BOUND throw then ;

public

\ ---- binding the dialect -----------------------------------------------------
\ Learn the operation and attribute-key identities of the module that is about to
\ be emitted, while it is still being built. A module's symbols are its own
\ ordinals, so this is the only moment the dialect can be asked which symbol each
\ of its opcodes and keys is; the answers stay valid after the module freezes
\ because freezing keeps the module's identity. The binding is spent by the next
\ EMIT.
: BIND-DIALECT ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   BND-MODE @ BOUND-YES = if E-A64EMIT-BIND throw then
   c b DIALECT-CK
   b IR-BUILD:MODULE@ 0 BND-MOD !
   c b A64IR-OPCODE:MOVZ    BIND1
   c b A64IR-OPCODE:MOVK    BIND1
   c b A64IR-OPCODE:ADD     BIND1
   c b A64IR-OPCODE:SUB     BIND1
   c b A64IR-OPCODE:MUL     BIND1
   c b A64IR-OPCODE:STORE   BIND1
   c b A64IR-OPCODE:LOAD    BIND1
   c b A64IR-OPCODE:RESERVE BIND1
   c b A64IR-OPCODE:RELEASE BIND1
   c b A64IR-OPCODE:RET     BIND1
   c b A64IR:KEY-IMM   0 BND-IMM !
   c b A64IR:KEY-SHIFT 0 BND-SH !
   c b A64IR:KEY-SLOT  0 BND-SLOT !
   c b A64IR:KEY-FRAME 0 BND-FRAME !
   BOUND-YES BND-MODE ! ;

\ Give up a binding without emitting against it.
: RELEASE ( -- )
   BND-TAKE ;

\ ---- the pass ----------------------------------------------------------------
\ Emit the whole of one frozen machine module, under the register assignment the
\ validator has accepted for it, into this package's buffers. Nothing is readable
\ until this returns; a run that refuses leaves no sealed emission. The shape is
\ decided before the assignment is, because whether these operations are
\ something this leaf can lay out at all is a question about the module alone -
\ and an assignment is never read before both questions have been answered.
: EMIT ( IR-CTX:ctx IR-BUILD:module -- )
   {: c:IR-CTX:ctx m:IR-BUILD:module :}
   BND-TAKE
   ST-EMPTY ST !
   0 N-INS !
   m BND-MODULE-CK
   c TARGET-CK
   m VIEWS!
   BLOCK-OF {: bk:IR-ID:ir-block-id :}
   bk STRAIGHT-CK
   m ALLOC-CK
   bk WALK
   ST-SEALED ST ! ;

\ ---- the sealed emission -----------------------------------------------------
: SEALED? ( -- bool )
   ST @ ST-SEALED = ;

\ How many instructions were emitted, and how many bytes they occupy.
: INSNS ( -- n )
   SEAL-CK N-INS @ ;

: SIZE ( -- n )
   SEAL-CK N-INS @ INSN-BYTES * ;

\ The emitted bytes themselves. The source map's offsets index this buffer.
: BYTES ( -- ptr u8 )
   SEAL-CK CODE ;

\ One instruction, read back out of the bytes it was placed in, so a caller that
\ wants a word and a caller that wants bytes are looking at the same thing.
: WORD@ ( n -- n )
   SEAL-CK ORD-CK INSN-BYTES * {: off:n :}
   off BYTE@
   off 1+ BYTE@ 8 lshift or
   off 2 + BYTE@ 16 lshift or
   off 3 + BYTE@ 24 lshift or ;

\ ---- the source map ----------------------------------------------------------
\ One row per emitted instruction: where its bytes were placed, and the span of
\ the operation that produced it.
: MAP-OFFSET@ ( n -- n )
   SEAL-CK ORD-CK cells M-OFF + @ ;

: MAP-SPAN@ ( n -- IR-SOURCE:span )
   SEAL-CK ORD-CK {: k:n :}
   k M-SRC @  k cells M-ST + @  k cells M-LN + @  IR--SOURCE-SPAN:MAKE ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package

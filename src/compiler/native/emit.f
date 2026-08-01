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
\ It does decide where each block's instructions land, because that is what a
\ branch's displacement is measured from, and nothing before this pass has any
\ business knowing it.
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
\   - a shape this layout cannot serve, re-derived from the module rather than
\     taken on trust: one function, at least one block, more blocks than the
\     layout table holds, and a block whose only terminator is its last
\     operation. The freeze verifier already forbids a block with two terminators
\     or one that is not last; this pass measures it again because it is about to
\     lay the operations out in that order.
\   - a branch whose successor names no block of the function being emitted, and
\     a branch whose displacement does not fit the field its form encodes it in.
\     The second is the reach check: both branch encoders MASK their displacement
\     field instead of bounding it, so a branch out of reach would quietly become
\     a branch somewhere else, and the bound is made here against the width the
\     dialect declares for that form.
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
\ than the buffers hold (E-A64EMIT-CAP) cannot either: at most three instructions
\ per operation, one value per operation that defines one, and the allocator
\ refuses a routine with more values or more blocks than the ceilings above. Both
\ are still written, because a
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
\ THE FOUR DATA-STACK FORMS ARE FOUR MORE INSTRUCTIONS AND NOTHING ELSE, for the
\ same reason: the same load, store, subtraction and addition, against the
\ register the running engine keeps the caller's data stack in. Nothing here
\ decides that a routine reads its arguments off the data stack - the convention
\ is declared on the routine's contract and turned into these operations by
\ src/compiler/native/select.f, so what reaches this pass is a module that
\ already contains its own entry and exit, and this file only encodes them.
\
\ THE TWO ADDRESSED FORMS ARE TWO MORE INSTRUCTIONS AND NOTHING ELSE, and the
\ only difference is where the base comes from. A frame access names the stack
\ pointer and a data-stack access names the engine's data-stack register, both
\ written here because neither is a value; an addressed access takes its base out
\ of the module, as the register the accepted allocation gave the address value.
\ Its offset is zero - `[Xn]` - and that zero is a property of the form rather
\ than a field of the operation, because this dialect has no addressing mode with
\ an offset for a caller to have got wrong.
\
\ THE BLOCK LAYOUT AND THE FIXUPS ARE ONE PASS AND NOT TWO. A branch has to know
\ where it is going before it can be encoded, and where it is going is where the
\ destination block's first instruction lands. So the layout is computed first,
\ from the instruction count of each form - which is a property of the form, the
\ same way its operand count is - and every displacement is then known when its
\ instruction is encoded. That is why there is no relocation list and nothing to
\ patch afterwards: the label table is the block-start table, a block IS a label,
\ and its ordinal is its name. Blocks are laid out in the order the module
\ records them, and no branch is elided because the block it goes to happens to
\ come next - eliding one is an optimisation, and emitting every branch in full
\ is what makes the layout order irrelevant to what the routine computes.
\
\ THE SOURCE MAP IS THE POINT OF THE BYTE OFFSETS. Every emitted instruction gets
\ one row: the byte offset it was placed at, and the span of the operation it
\ came from. An operation that is more than one instruction gets one row per
\ instruction, each carrying the span of the operation they all came from. The offset is the cursor at the moment the instruction was appended,
\ not four times its index, so a run that emitted one instruction too few or too
\ many is visible in the map and not only in the length. This is what a located
\ diagnostic about emitted code reads, and it is why the bytes are a byte buffer
\ with a little-endian placement of each word rather than an array of words: the
\ offsets have to index something a caller can actually look at.
\
\ ONE EMISSION AT A TIME. The buffers are fixed package-owned storage rather than
\ heap objects, so this pass emits one routine at a time - the single-task
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
require src/compiler/ir/source.f
require src/compiler/ir/build.f
require src/compiler/native/a64ir.f
require src/compiler/native/frozen.f
require src/compiler/native/regalloc.f
require src/compiler/native/regalloc-verify.f
require src/arch/arm64/asm.f

package A64EMIT
using NFROZEN
private

\ ---- the bound dialect -------------------------------------------------------
\ One slot per member of the operation family, so the family stays exhaustive: a
\ member added to A64IR:opcode makes this fail to compile until it has a slot and
\ an encoding.
21 constant OPCODES-N
0 constant O-MOVZ
1 constant O-MOVK
2 constant O-MOV
3 constant O-ADD
4 constant O-SUB
5 constant O-MUL
6 constant O-STORE
7 constant O-LOAD
8 constant O-RESERVE
9 constant O-RELEASE
10 constant O-DTAKE
11 constant O-DLOAD
12 constant O-DSTORE
13 constant O-DPUBLISH
14 constant O-FLAG
15 constant O-BR
16 constant O-BRZ
17 constant O-RET
18 constant O-ALOAD
19 constant O-ASTORE
20 constant O-SDIV

0 constant BOUND-NO
1 constant BOUND-YES

\ ---- how much of one routine this pass holds ----------------------------------
\ Instructions in one routine. Three forms of the dialect emit more than one
\ instruction - the comparison and the division are three each and the two-way
\ branch is two - and none emits more than three, so three per operation is the
\ ceiling per operation. Operations are bounded by the values they define: every
\ operation defines at least one value except a block's terminator, the release
\ of the frame and the data-stack publish, of which there is at most one each per
\ block. So a routine that fits the two ceilings NFROZEN commits to emits at most
\ this many.
3 constant INSN-PER-OP
INSN-PER-OP VMAX BMAX 3 * + * constant INSN-MAX

\ Every ARM64 instruction is four bytes.
4 constant INSN-BYTES

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
variable N-BLK
0 N-BLK !
variable LAY-AT
0 LAY-AT !

1 TYPED-BUFFER BND-MOD IR-ID:ir-module-id
OPCODES-N TYPED-BUFFER BND-OP IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-IMM IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-SH IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-SLOT IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-FRAME IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-DSLOT IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-DBYTES IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-COND IR-ID:ir-symbol-id

\ The emitted bytes, and one source-map row per emitted instruction.
create CODE INSN-MAX INSN-BYTES * allot
create M-OFF INSN-MAX cells allot
create M-ST INSN-MAX cells allot
create M-LN INSN-MAX cells allot
INSN-MAX TYPED-BUFFER M-SRC IR-ID:ir-source-id

\ Where each block's first instruction lands, in instructions from the start of
\ the routine. This is the whole label table: a block IS a label, its ordinal is
\ its name, and a branch is resolved by subtracting the branch's own position
\ from the entry here. There is no relocation list, because there is nothing to
\ patch afterwards - the layout is computed before the first byte is written, so
\ every displacement is known when its instruction is encoded.
create B-START BMAX cells allot

\ ---- the dialect's operation family ------------------------------------------
: SLOT-OF ( A64IR:opcode -- n )
   MATCH A64IR:opcode
      movz    OF O-MOVZ    ENDOF
      movk    OF O-MOVK    ENDOF
      mov     OF O-MOV     ENDOF
      add     OF O-ADD     ENDOF
      sub     OF O-SUB     ENDOF
      mul     OF O-MUL     ENDOF
      sdiv    OF O-SDIV    ENDOF
      store    OF O-STORE    ENDOF
      load     OF O-LOAD     ENDOF
      reserve  OF O-RESERVE  ENDOF
      release  OF O-RELEASE  ENDOF
      dtake    OF O-DTAKE    ENDOF
      dload    OF O-DLOAD    ENDOF
      dstore   OF O-DSTORE   ENDOF
      dpublish OF O-DPUBLISH ENDOF
      aload    OF O-ALOAD   ENDOF
      astore   OF O-ASTORE  ENDOF
      flag     OF O-FLAG     ENDOF
      br       OF O-BR       ENDOF
      brz      OF O-BRZ      ENDOF
      ret      OF O-RET      ENDOF
   ;MATCH ;

: SLOT-OPCODE ( n -- A64IR:opcode )
   case
      O-MOVZ    of A64IR-OPCODE:MOVZ    endof
      O-MOVK    of A64IR-OPCODE:MOVK    endof
      O-MOV     of A64IR-OPCODE:MOV     endof
      O-ADD     of A64IR-OPCODE:ADD     endof
      O-SUB     of A64IR-OPCODE:SUB     endof
      O-MUL     of A64IR-OPCODE:MUL     endof
      O-SDIV    of A64IR-OPCODE:SDIV    endof
      O-STORE   of A64IR-OPCODE:STORE   endof
      O-LOAD    of A64IR-OPCODE:LOAD    endof
      O-RESERVE  of A64IR-OPCODE:RESERVE  endof
      O-RELEASE  of A64IR-OPCODE:RELEASE  endof
      O-DTAKE    of A64IR-OPCODE:DTAKE    endof
      O-DLOAD    of A64IR-OPCODE:DLOAD    endof
      O-DSTORE   of A64IR-OPCODE:DSTORE   endof
      O-DPUBLISH of A64IR-OPCODE:DPUBLISH endof
      O-FLAG     of A64IR-OPCODE:FLAG     endof
      O-BR       of A64IR-OPCODE:BR       endof
      O-BRZ      of A64IR-OPCODE:BRZ      endof
      O-RET      of A64IR-OPCODE:RET      endof
      O-ALOAD    of A64IR-OPCODE:ALOAD    endof
      O-ASTORE   of A64IR-OPCODE:ASTORE   endof
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
: SLOT-AT ( IR-ID:ir-op-id -- n )
   OPCODE-AT OPCODE-SLOT ;

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
   id ATTRS-OF {: n:n :}
   n 0 ?do
      id i ATTR-KEY-AT want SAME-SYM? if drop i leave then
   loop
   dup 0 < if E-A64EMIT-ATTR throw then ;

: ATTR-INT ( IR-ID:ir-op-id IR-ID:ir-symbol-id -- n )
   {: id:IR-ID:ir-op-id want:IR-ID:ir-symbol-id :}
   id want ATTR-SLOT {: k:n :}
   id k ATTR-INT-AT ;

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

\ ---- the data-stack operands -------------------------------------------------
\ The same two readings against the other pointer, under the dialect's own keys
\ for them. They are separate keys and separate readers because a frame offset
\ and a data-stack offset are counted from different registers: one key answering
\ both would let a frame access encode as a data-stack access.
: DSLOT-OFF ( IR-ID:ir-op-id -- n )
   0 BND-DSLOT @ ATTR-INT ;

: DBYTES-SIZE ( IR-ID:ir-op-id -- n )
   0 BND-DBYTES @ ATTR-INT ;

\ ---- one instruction per operation -------------------------------------------
\ Each of these is exactly the encoder call the form names, with the registers
\ the accepted allocation answers and the operands the module carries.
: WORD-MOVZ ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id 0 RESULT-REG  id IMM-OF  id HALF-OF  MOVZHW ;

: WORD-MOVK ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id 0 RESULT-REG  id IMM-OF  id HALF-OF  MOVKHW ;

\ The copy that puts a returned value where the routine's contract says it
\ leaves. ENC-MOV is the assembler's own name for the Orr-with-zero-register
\ form ARM64 spells a move as, so no second idiom is written here.
: WORD-MOV ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id 0 RESULT-REG  id 0 OPERAND-REG  ENC-MOV ;

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

\ The four data-stack forms, which are the same four instructions against the
\ other pointer. The base is A64EFF:DSTACK-GPR - the register the running engine
\ keeps the data stack in - asked for rather than written here, for the same
\ reason the frame accesses ask for the stack-pointer operand.
: WORD-DTAKE ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   A64EFF:DSTACK-GPR A64EFF:DSTACK-GPR  id DBYTES-SIZE  ENC-SUBI ;

: WORD-DPUBLISH ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   A64EFF:DSTACK-GPR A64EFF:DSTACK-GPR  id DBYTES-SIZE  ENC-ADDI ;

: WORD-DLOAD ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id 0 RESULT-REG  A64EFF:DSTACK-GPR  id DSLOT-OFF  ENC-LDR ;

: WORD-DSTORE ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id 0 OPERAND-REG  A64EFF:DSTACK-GPR  id DSLOT-OFF  ENC-STR ;

\ ---- the two addressed forms -------------------------------------------------
\ The same Ldr and Str the frame and the data stack use, with the base taken out
\ of the module instead of named by this file: an addressed access reaches
\ wherever the program computed, so its base is the register the accepted
\ allocation gave the address value. The offset is zero, which is what `[Xn]` is,
\ and it is written here as the number the form carries rather than read off an
\ attribute the dialect does not declare: there is no addressing mode with an
\ offset in this dialect, so there is no field to read.
0 constant ADDR-OFF

\ The load's address is its first operand and its loaded value is its first
\ result; the store's value is its first operand and its address is its second,
\ which is the order the dialect declares and the order Forth writes.
: WORD-ALOAD ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id 0 RESULT-REG  id 0 OPERAND-REG  ADDR-OFF  ENC-LDR ;

: WORD-ASTORE ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id 0 OPERAND-REG  id 1 OPERAND-REG  ADDR-OFF  ENC-STR ;

\ ---- the condition a comparison is made under --------------------------------
: COND-OF ( IR-ID:ir-op-id -- n )
   0 BND-COND @ ATTR-INT ;

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
   id SPAN-AT IR--SOURCE-SPAN:UNMAKE
   {: src:IR-ID:ir-source-id st:n ln:n :}
   src k M-SRC !
   st k cells M-ST + !
   ln k cells M-LN + ! ;

\ Append one instruction at the cursor and record where it landed. An operation
\ that is more than one instruction calls this once per instruction, so the map
\ has a row for each of them and each row carries the span of the operation they
\ all came from.
: APPEND ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id w:n :}
   N-INS @ INSN-MAX >= if E-A64EMIT-CAP throw then
   N-INS @ {: k:n :}
   k INSN-BYTES * {: off:n :}
   w off WORD!
   id off k MAP!
   k 1+ N-INS ! ;

\ ---- the block layout --------------------------------------------------------
\ Blocks are laid out in the order the module records them, which is the order
\ they were built in. That is a decision and not an accident: it is the one order
\ every reader of the module already agrees on, so the allocator, the validator
\ and this pass number the same instruction the same way, and a fixture can
\ assert exact offsets. Nothing here reorders blocks to make a branch fall
\ through to the next one - eliding a branch is an optimisation, and every
\ terminator below emits its branches in full, which is what makes the layout
\ order irrelevant to what the routine computes and a wrong successor a wrong
\ answer rather than a lucky one.
\
\ How many instructions a form is, is a property of the form: one for all but the
\ comparison and the division, which are three each, and the two-way branch,
\ which is two. The layout pass and
\ the emission pass read the same answer, so the offsets the fixups are computed
\ against are the offsets the instructions land at.
: INSNS-OF ( n -- n )
   {: k:n :}
   k O-FLAG = if 3 exit then
   k O-SDIV = if 3 exit then
   k O-BRZ = if 2 exit then
   1 ;

: OP-INSNS ( IR-ID:ir-op-id -- n )
   SLOT-AT INSNS-OF ;

: BLOCK-INSNS ( IR-ID:ir-block-id -- n )
   {: bk:IR-ID:ir-block-id :}
   0
   bk OP-COUNT 0 ?do
      bk i OP-AT OP-INSNS +
   loop ;

: BLK-ORD-CK ( n -- n )
   dup 0 < over N-BLK @ >= or if E-A64EMIT-BLOCK throw then ;

: START-AT ( n -- n )
   BLK-ORD-CK cells B-START + @ ;

\ Where each block's first instruction lands, measured in instructions from the
\ start of the routine. It is computed before a single byte is written, because a
\ forward branch has to know where it is going before it can be encoded.
: LAYOUT ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   f BLOCK-COUNT {: n:n :}
   n 1 < if E-A64EMIT-SHAPE throw then
   n BMAX > if E-A64EMIT-CAP throw then
   n N-BLK !
   0 LAY-AT !
   n 0 ?do
      LAY-AT @ i cells B-START + !
      LAY-AT @  f i BLOCK-AT BLOCK-INSNS  +  LAY-AT !
   loop ;

\ ---- the branches ------------------------------------------------------------
\ Which block a successor names. A successor is a block of this module, and this
\ pass laid out every block of the one function it emits, so an ordinal outside
\ that range is a module this layout cannot serve rather than a branch to
\ nowhere.
: SUCC-BLOCK ( IR-ID:ir-op-id n -- n )
   SUCC-AT IR-ID:BLOCK-LOCAL BLK-ORD-CK ;

\ The displacement one branch carries, counted in instructions from the branch
\ itself, which is what the architecture's PC-relative fields hold.
: DELTA ( n -- n )
   START-AT N-INS @ - ;

\ The reach check the branches dot asks for, made before the encoder is called.
\ Both encoders mask their displacement field rather than bounding it - a branch
\ out of reach would silently become a branch somewhere else - so the bound is
\ made here, against the field width the dialect declares for that form.
: B-WORD ( n -- n )
   {: d:n :}
   d A64IR:B-FITS? 0= if E-A64EMIT-REACH throw then
   d ENC-B ;

: BZ-WORD ( n n -- n )
   {: rt:n d:n :}
   d A64IR:BZ-FITS? 0= if E-A64EMIT-REACH throw then
   rt d ENC-CBZ ;

\ Going to one block, handing it its arguments. The arguments are already in the
\ registers the destination's block arguments were given - that is the register
\ allocation's own decision and the validator has agreed with it - so the
\ operands reach no encoder here and the whole instruction is the jump.
: PUT-BR ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id  id 0 SUCC-BLOCK DELTA B-WORD  APPEND ;

\ The two-way branch: go to the first successor when the tested register is
\ zero, and to the second when it is not. Both branches are emitted, in that
\ order, so neither successor depends on where the layout happened to put it.
: PUT-BRZ ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id  id 0 OPERAND-REG  id 0 SUCC-BLOCK DELTA  BZ-WORD  APPEND
   id  id 1 SUCC-BLOCK DELTA B-WORD  APPEND ;

\ One comparison, which is three instructions: compare the two registers, set one
\ into the result on the condition, and negate it, because a Habu flag is all
\ bits set rather than one. This is the sequence the engine's own emitter uses,
\ so a compiled comparison answers what an interpreted one answers.
: PUT-FLAG ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id 0 RESULT-REG {: rd:n :}
   id  id 0 OPERAND-REG id 1 OPERAND-REG ENC-CMP  APPEND
   id  rd id COND-OF ENC-CSET  APPEND
   id  rd rd ENC-NEG  APPEND ;

\ One division, which is three instructions: branch past the trap when the
\ divisor is not zero, the trap, and the divide. It is the sequence the engine's
\ own `/` compiles to - src/habu/habu1.f BDIV0? followed by BDIV - so a compiled
\ division answers what an interpreted one answers on every divisor, and ends
\ the process on the one divisor a bare Sdiv would answer zero for.
\
\ The branch distance is a property of this form and not of the block layout:
\ it skips exactly the one instruction between it and the divide, so it is
\ written here as the two words it is rather than measured off the label table.
2 constant DIV-SKIP                  \ words from the guard to the divide

: PUT-SDIV ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id  id 1 OPERAND-REG DIV-SKIP ENC-CBNZ  APPEND
   id  ENC-BRK  APPEND
   id  id TRIPLE ENC-SDIV  APPEND ;

\ ---- one operation, as the instructions it is --------------------------------
\ The whole encoding table. Every arm names the instructions one machine
\ operation becomes; nothing else in this file decides which bytes an operation
\ is.
: PUT-OP ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id SLOT-AT SLOT-OPCODE
   MATCH A64IR:opcode
      movz     OF id  id WORD-MOVZ  APPEND ENDOF
      movk     OF id  id WORD-MOVK  APPEND ENDOF
      mov      OF id  id WORD-MOV  APPEND ENDOF
      add      OF id  id TRIPLE ENC-ADD  APPEND ENDOF
      sub      OF id  id TRIPLE ENC-SUB  APPEND ENDOF
      mul      OF id  id TRIPLE ENC-MUL  APPEND ENDOF
      sdiv     OF id PUT-SDIV ENDOF
      store    OF id  id WORD-STORE  APPEND ENDOF
      load     OF id  id WORD-LOAD  APPEND ENDOF
      reserve  OF id  id WORD-RESERVE  APPEND ENDOF
      release  OF id  id WORD-RELEASE  APPEND ENDOF
      dtake    OF id  id WORD-DTAKE  APPEND ENDOF
      dload    OF id  id WORD-DLOAD  APPEND ENDOF
      dstore   OF id  id WORD-DSTORE  APPEND ENDOF
      dpublish OF id  id WORD-DPUBLISH  APPEND ENDOF
      aload    OF id  id WORD-ALOAD  APPEND ENDOF
      astore   OF id  id WORD-ASTORE  APPEND ENDOF
      flag     OF id PUT-FLAG ENDOF
      br       OF id PUT-BR ENDOF
      brz      OF id PUT-BRZ ENDOF
      ret      OF id  ENC-RET  APPEND ENDOF
   ;MATCH ;

\ ---- the shape this leaf emits from ------------------------------------------
\ One function, of one or more blocks, each ending in exactly one terminator.
: FUN-OF ( -- IR-ID:ir-fun-id )
   FUN-COUNT 1 <> if E-A64EMIT-SHAPE throw then
   MKEY 0 IR-ID:PACK-FUN ;

\ The block's operations run in one order and end once. Re-derived rather than
\ taken from the verifier, because this pass is about to lay them out in exactly
\ that order.
: TERMINATOR? ( IR-ID:ir-block-id n -- bool )
   OP-AT SLOT-AT {: k:n :}
   k O-RET = k O-BR = or k O-BRZ = or ;

: BLOCK-CK ( IR-ID:ir-block-id -- )
   {: bk:IR-ID:ir-block-id :}
   bk OP-COUNT {: n:n :}
   n 1 < if E-A64EMIT-SHAPE throw then
   bk n 1- TERMINATOR? 0= if E-A64EMIT-SHAPE throw then
   n 1- 0 ?do
      bk i TERMINATOR? if E-A64EMIT-SHAPE throw then
   loop ;

: SHAPE-CK ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   f BLOCK-COUNT 0 ?do
      f i BLOCK-AT BLOCK-CK
   loop ;

: WALK-BLOCK ( IR-ID:ir-block-id -- )
   {: bk:IR-ID:ir-block-id :}
   bk OP-COUNT 0 ?do
      bk i OP-AT PUT-OP
   loop ;

: WALK ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   f BLOCK-COUNT 0 ?do
      f i BLOCK-AT WALK-BLOCK
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
   c b A64IR-OPCODE:MOV     BIND1
   c b A64IR-OPCODE:ADD     BIND1
   c b A64IR-OPCODE:SUB     BIND1
   c b A64IR-OPCODE:MUL     BIND1
   c b A64IR-OPCODE:SDIV    BIND1
   c b A64IR-OPCODE:STORE   BIND1
   c b A64IR-OPCODE:LOAD    BIND1
   c b A64IR-OPCODE:RESERVE  BIND1
   c b A64IR-OPCODE:RELEASE  BIND1
   c b A64IR-OPCODE:DTAKE    BIND1
   c b A64IR-OPCODE:DLOAD    BIND1
   c b A64IR-OPCODE:DSTORE   BIND1
   c b A64IR-OPCODE:DPUBLISH BIND1
   c b A64IR-OPCODE:FLAG     BIND1
   c b A64IR-OPCODE:BR       BIND1
   c b A64IR-OPCODE:BRZ      BIND1
   c b A64IR-OPCODE:RET      BIND1
   c b A64IR-OPCODE:ALOAD    BIND1
   c b A64IR-OPCODE:ASTORE   BIND1
   c b A64IR:KEY-IMM    0 BND-IMM !
   c b A64IR:KEY-SHIFT  0 BND-SH !
   c b A64IR:KEY-SLOT   0 BND-SLOT !
   c b A64IR:KEY-FRAME  0 BND-FRAME !
   c b A64IR:KEY-DSLOT  0 BND-DSLOT !
   c b A64IR:KEY-DBYTES 0 BND-DBYTES !
   c b A64IR:KEY-COND   0 BND-COND !
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
   FUN-OF {: f:IR-ID:ir-fun-id :}
   f SHAPE-CK
   f LAYOUT
   m ALLOC-CK
   f WALK
   ST-SEALED ST ! ;

\ ---- the sealed emission -----------------------------------------------------
: SEALED? ( -- bool )
   ST @ ST-SEALED = ;

\ How many instructions were emitted, and how many bytes they occupy.
: INSNS ( -- n )
   SEAL-CK N-INS @ ;

\ ---- the block layout, read back ---------------------------------------------
\ How many blocks were laid out, and where each one starts. A caller that wants
\ to know whether a branch went where the layout said it would reads these and
\ the instruction at the branch's own position; a fixture asserts both.
: BLOCKS ( -- n )
   SEAL-CK N-BLK @ ;

: BLOCK-START@ ( n -- n )
   SEAL-CK BLK-ORD-CK cells B-START + @ ;

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

;using
;package

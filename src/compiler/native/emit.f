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
\   - a caller asking how many instructions of an emission are its BODY when that
\     emission BRANCHES OUT of itself. A call site publishes its arguments and
\     takes its results back through the very data-stack forms a routine's own
\     crossings use, so for such a routine the emission less its crossings is not
\     its body, and there is no number to give.
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
\ from the instruction count of each operation, and every displacement is then
\ known when its instruction is encoded. That is why there is no relocation list
\ and nothing to patch afterwards: the label table is the block-start table, a
\ block IS a label, and its ordinal is its name.
\
\ AND A CALL TO ANOTHER WORD NEEDS NO RELOCATION EITHER, FOR THE SAME REASON READ
\ FROM THE OTHER END. Its target is not a block of this function but an address,
\ so the label table cannot answer it; what it needs instead is where THIS
\ routine's own bytes will be written, and that is decided by the publication
\ seam before the emission is made. So the pass is told that address, both ends
\ of the subtraction are known when the instruction is encoded, and nothing is
\ patched afterwards here either. The alternative - emit a branch to nowhere and
\ let the seam fix it up through the source map - would put an instruction
\ encoder in the seam, and then two files would decide what a Bl is. The seam
\ instead holds the placement this pass was given against the slot it claims, so
\ the one authority on where a routine lands is asked twice and measured against
\ itself.
\
\ A BRANCH TO THE BLOCK LAID OUT NEXT IS NOT EMITTED, WHICH MAKES A TERMINATOR'S
\ INSTRUCTION COUNT A PROPERTY OF THE LAYOUT AND NOT ONLY OF ITS FORM. A
\ terminator whose trailing unconditional branch names the very next block
\ reaches that block by falling into it, so the branch is left out. That is four
\ bytes and a jump for every one of them, and the fused compare-and-branch is
\ wired to leave its unconditional half pointing at the next block precisely so
\ this can delete it. The price is exact and worth naming: the layout order is
\ load-bearing. Moving a block changes what is emitted and not only where it
\ lands, which is the property the full-branch emission used to buy.
\
\ AND SO THIS PASS CHOOSES THE ORDER RATHER THAN INHERITING IT. Which branches
\ the rule above deletes is decided by which block is written next, and that used
\ to be whatever order the elaborator happened to build the blocks in - so a
\ `begin … while … repeat` loop, whose exit stub is built between the header and
\ the body, kept two branches that nothing about the program required.
\ ORDER-BLOCKS writes block zero first, the block control leaves the routine
\ through last - the publication seam records a length that says the emission
\ ends in the return - and between them follows each block with the successor its
\ trailing branch names, falling back to the lowest block not yet written. The
\ order is one permutation, held in one table, and the layout, the writer and the
\ fall-through rule all read it - so there is one owner of "what comes next" and
\ no second answer to drift from it. The module's own block order is untouched
\ and still names every block: the label table, every branch and every other
\ reader of the module go on speaking in ordinals.
\
\ SO THE RULE IS WRITTEN ONCE AND BOTH PASSES ASK IT. FALL-THRU? answers, from an
\ operation and the ordinal of the block it terminates, whether that trailing
\ branch is reached by falling through. The layout subtracts its instruction from
\ the form's count and the emitter leaves out exactly the instruction the layout
\ did not count - one word, two callers, no second copy to drift from the first.
\ And because one rule asked twice is still a rule asked twice, WALK holds the
\ instruction cursor against the layout at the start of every block and at the
\ end of the routine: a disagreement between what was counted and what was
\ written is E-A64EMIT-LAYOUT before any caller can read a byte of it. The same
\ cursor covers the second elision below.
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
\ A COPY INTO THE REGISTER IT COMES FROM IS NOT EMITTED EITHER, WHICH MAKES A
\ COPY'S INSTRUCTION COUNT A PROPERTY OF THE REGISTER ASSIGNMENT. An a64.mov
\ whose source and destination registers are the same moves nothing, so it is
\ left out. The register allocator is what makes that common rather than
\ accidental: it prefers one register for both ends of a copy wherever the two do
\ not interfere (src/compiler/native/regalloc.f, step five), which is how the
\ copy a value crossing an argument-carrying edge is split with disappears on a
\ loop latch. But the rule here is register equality alone and asks nothing about
\ intent, so a copy whose ends land in one register for any other reason goes the
\ same way.
\
\ SO THE SECOND RULE IS WRITTEN ONCE TOO. SELF-MOV? answers, from an operation,
\ whether it is a copy into its own register; the layout subtracts its
\ instruction and PUT-MOV leaves out exactly the instruction the layout did not
\ count. The same cursor check holds them both. What this rule costs is an
\ ordering: the layout used to be computable from the module alone and now needs
\ the accepted assignment, so EMIT probes the acceptance before it lays the
\ blocks out. That is written where the pass is run.
\
\ AN ELIDED INSTRUCTION GETS NO ROW, because the map has one row per emitted
\ instruction and nothing else. That is what the index of a row MEANS here: row k
\ describes the instruction WORD@ k answers, and the two are read together by
\ every caller that locates a byte. A row standing for an instruction that was
\ not emitted would put every row after it against the wrong instruction, which
\ is a worse answer than the honest one - the terminator's span is on the rows of
\ the instructions it did emit, and a branch or a copy that is not there is not
\ anywhere.
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
63 constant OPCODES-N
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
21 constant O-ABLOAD
22 constant O-ABSTORE
23 constant O-CALL
24 constant O-LINKSAVE
25 constant O-LINKLOAD
26 constant O-CMPBR
27 constant O-WORDCALL
28 constant O-AND
29 constant O-ORR
30 constant O-EOR
31 constant O-LSLV
32 constant O-LSRV
33 constant O-MVN
34 constant O-FADD
35 constant O-FSUB
36 constant O-FMUL
37 constant O-FDIV
38 constant O-FNEG
39 constant O-FABS
40 constant O-FSQRT
41 constant O-SCVTF
42 constant O-FCVTZS
43 constant O-FMOVXD
44 constant O-FMOVDX
45 constant O-FFLAG
46 constant O-FFLAGZ
47 constant O-FCMPBR
48 constant O-FCMPBRZ
49 constant O-FMOVDD
50 constant O-SELZ
51 constant O-CMPSEL
52 constant O-SELZD
53 constant O-CMPSELD
54 constant O-FCMPSEL
55 constant O-FCMPSELZ
56 constant O-FCMPSELD
57 constant O-FCMPSELZD
58 constant O-TAILCALL
59 constant O-MADD
60 constant O-ADDI
61 constant O-SUBI
62 constant O-MOVN

0 constant BOUND-NO
1 constant BOUND-YES

\ ---- how much of one routine this pass holds ----------------------------------
\ Instructions in one routine. Five forms of the dialect emit more than one
\ instruction - the comparison, the division, the call and the fused
\ compare-and-branch are three each and the two-way branch is two - and none
\ emits more than three, so three per operation is the
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

\ The registers this run has put into a destination field, and the registers the
\ accepted allocation says the routine destroys. The first is counted while the
\ instructions are built and the second is taken at the seal; NOTE-WRITE and
\ CLOBBER-SEAL below are where each of them is said.
variable EM-WGPR
0 EM-WGPR !
variable EM-WFPR
0 EM-WFPR !
variable EM-CGPR
0 EM-CGPR !
variable EM-CFPR
0 EM-CFPR !

\ And what the code this run BRANCHES TO destroys. A routine destroys what its
\ own instructions write and everything the routines it calls write, so a caller
\ that read only the first half would be told a register survives a call that the
\ callee's callee writes. It is kept apart from the count above because it is not
\ something these instructions did: the check that the emission wrote nothing the
\ allocation did not claim is about this routine's own instructions, and this is
\ about somebody else's.
variable EM-KGPR
0 EM-KGPR !
variable EM-KFPR
0 EM-KFPR !

\ How many of the instructions this run wrote are the routine's CROSSINGS rather
\ than its work, and how many branches out of the routine it wrote. Both are
\ counted the way the clobber set is - while the instructions are being written,
\ by the word that writes them - and BODY-INSNS at the foot of this file is what
\ they are for: a caller deciding whether to copy a routine's body into itself
\ has to know how much of the emission that body IS.
\
\ THE COUNT IS THE CURSOR'S OWN DIFFERENCE AND NOT A SECOND ARITHMETIC. What an
\ operation costs in instructions is already decided twice over - by the layout,
\ which counts it, and by the writer, which appends it, held together by
\ CURSOR-CK - and a third count here could disagree with both. So the interface
\ total is read off N-INS across each operation the writer is handed, which makes
\ it the same number by construction whatever elisions apply to it.
variable EM-IFACE
0 EM-IFACE !
variable EM-NCALL
0 EM-NCALL !
variable EM-TAIL                     \ tail branches this emission wrote
0 EM-TAIL !

\ ---- where this routine will be written --------------------------------------
\ A branch to a block is measured from the layout, so it is the same displacement
\ wherever the routine lands. A branch to ANOTHER WORD is not: the callee has an
\ address of its own, so the distance between the two depends on where this
\ routine's own bytes go. That address is not this pass's to decide - the
\ publication seam claims the engine's code space and is the one authority on it
\ - so this pass is TOLD it, and the seam refuses to publish an emission whose
\ placement is not the slot it is claiming.
\
\ IT IS DECLARED AND CONSUMED LIKE THE DIALECT BINDING, for the same reason: an
\ emission has to be about one placement, and a run that refused must not leave a
\ placement behind for the next one to measure against. A run that emits no call
\ to another word needs none, and one that does and was told none is refused.
0 constant PLACE-NO
1 constant PLACE-YES
variable PLACE-MODE
PLACE-NO PLACE-MODE !
variable PLACE-AT-N
0 PLACE-AT-N !
variable EM-PLACED                   \ whether the sealed emission has a placement
0 EM-PLACED !
variable EM-PLACE                    \ and what it is
0 EM-PLACE !

1 TYPED-BUFFER BND-MOD IR-ID:ir-module-id
OPCODES-N TYPED-BUFFER BND-OP IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-IMM IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-SH IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-SLOT IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-FRAME IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-DSLOT IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-DBYTES IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-COND IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-DBACK IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-ENTRY IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-OFF IR-ID:ir-symbol-id

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
\
\ IT IS KEYED BY ORDINAL AND NOT BY POSITION, because a branch names an ordinal.
\ Which POSITION a block is laid out at is the other two tables below.
create B-START BMAX cells allot

\ The order the blocks are laid out in, read both ways round. B-ORDER answers
\ which block is written at a position and B-PLACE answers which position a block
\ is written at, and they are one permutation held twice because both directions
\ are asked in the inner loop of a pass: the layout and the writer walk positions
\ and the fall-through rule asks where a branch's TARGET sits. Deriving either
\ from the other by searching would put a scan of every block inside the
\ per-operation question that rule is.
create B-ORDER BMAX cells allot        \ position -> block ordinal
create B-PLACE BMAX cells allot        \ block ordinal -> position

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
      and     OF O-AND     ENDOF
      orr     OF O-ORR     ENDOF
      eor     OF O-EOR     ENDOF
      lslv    OF O-LSLV    ENDOF
      lsrv    OF O-LSRV    ENDOF
      mvn     OF O-MVN     ENDOF
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
      abload   OF O-ABLOAD  ENDOF
      abstore  OF O-ABSTORE ENDOF
      flag     OF O-FLAG     ENDOF
      selz     OF O-SELZ     ENDOF
      cmpsel   OF O-CMPSEL   ENDOF
      br       OF O-BR       ENDOF
      brz      OF O-BRZ      ENDOF
      cmpbr    OF O-CMPBR    ENDOF
      call     OF O-CALL     ENDOF
      wordcall OF O-WORDCALL ENDOF
      linksave OF O-LINKSAVE ENDOF
      linkload OF O-LINKLOAD ENDOF
      ret      OF O-RET      ENDOF
      fadd     OF O-FADD     ENDOF
      fsub     OF O-FSUB     ENDOF
      fmul     OF O-FMUL     ENDOF
      fdiv     OF O-FDIV     ENDOF
      fneg     OF O-FNEG     ENDOF
      fabs     OF O-FABS     ENDOF
      fsqrt    OF O-FSQRT    ENDOF
      scvtf    OF O-SCVTF    ENDOF
      fcvtzs   OF O-FCVTZS   ENDOF
      fmovxd   OF O-FMOVXD   ENDOF
      fmovdx   OF O-FMOVDX   ENDOF
      fmovdd   OF O-FMOVDD   ENDOF
      fflag    OF O-FFLAG    ENDOF
      fflagz   OF O-FFLAGZ   ENDOF
      fcmpbr   OF O-FCMPBR   ENDOF
      fcmpbrz  OF O-FCMPBRZ  ENDOF
      selzd    OF O-SELZD    ENDOF
      cmpseld  OF O-CMPSELD  ENDOF
      fcmpsel   OF O-FCMPSEL   ENDOF
      fcmpselz  OF O-FCMPSELZ  ENDOF
      fcmpseld  OF O-FCMPSELD  ENDOF
      fcmpselzd OF O-FCMPSELZD ENDOF
      tailcall  OF O-TAILCALL  ENDOF
      madd      OF O-MADD      ENDOF
      addi      OF O-ADDI      ENDOF
      subi      OF O-SUBI      ENDOF
      movn      OF O-MOVN      ENDOF
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
      O-AND     of A64IR-OPCODE:AND     endof
      O-ORR     of A64IR-OPCODE:ORR     endof
      O-EOR     of A64IR-OPCODE:EOR     endof
      O-LSLV    of A64IR-OPCODE:LSLV    endof
      O-LSRV    of A64IR-OPCODE:LSRV    endof
      O-MVN     of A64IR-OPCODE:MVN     endof
      O-STORE   of A64IR-OPCODE:STORE   endof
      O-LOAD    of A64IR-OPCODE:LOAD    endof
      O-RESERVE  of A64IR-OPCODE:RESERVE  endof
      O-RELEASE  of A64IR-OPCODE:RELEASE  endof
      O-DTAKE    of A64IR-OPCODE:DTAKE    endof
      O-DLOAD    of A64IR-OPCODE:DLOAD    endof
      O-DSTORE   of A64IR-OPCODE:DSTORE   endof
      O-DPUBLISH of A64IR-OPCODE:DPUBLISH endof
      O-FLAG     of A64IR-OPCODE:FLAG     endof
      O-SELZ     of A64IR-OPCODE:SELZ     endof
      O-CMPSEL   of A64IR-OPCODE:CMPSEL   endof
      O-BR       of A64IR-OPCODE:BR       endof
      O-BRZ      of A64IR-OPCODE:BRZ      endof
      O-CMPBR    of A64IR-OPCODE:CMPBR    endof
      O-RET      of A64IR-OPCODE:RET      endof
      O-ALOAD    of A64IR-OPCODE:ALOAD    endof
      O-ASTORE   of A64IR-OPCODE:ASTORE   endof
      O-ABLOAD   of A64IR-OPCODE:ABLOAD   endof
      O-ABSTORE  of A64IR-OPCODE:ABSTORE  endof
      O-CALL     of A64IR-OPCODE:CALL     endof
      O-WORDCALL of A64IR-OPCODE:WORDCALL endof
      O-LINKSAVE of A64IR-OPCODE:LINKSAVE endof
      O-LINKLOAD of A64IR-OPCODE:LINKLOAD endof
      O-FADD     of A64IR-OPCODE:FADD     endof
      O-FSUB     of A64IR-OPCODE:FSUB     endof
      O-FMUL     of A64IR-OPCODE:FMUL     endof
      O-FDIV     of A64IR-OPCODE:FDIV     endof
      O-FNEG     of A64IR-OPCODE:FNEG     endof
      O-FABS     of A64IR-OPCODE:FABS     endof
      O-FSQRT    of A64IR-OPCODE:FSQRT    endof
      O-SCVTF    of A64IR-OPCODE:SCVTF    endof
      O-FCVTZS   of A64IR-OPCODE:FCVTZS   endof
      O-FMOVXD   of A64IR-OPCODE:FMOVXD   endof
      O-FMOVDX   of A64IR-OPCODE:FMOVDX   endof
      O-FMOVDD   of A64IR-OPCODE:FMOVDD   endof
      O-FFLAG    of A64IR-OPCODE:FFLAG    endof
      O-FFLAGZ   of A64IR-OPCODE:FFLAGZ   endof
      O-FCMPBR   of A64IR-OPCODE:FCMPBR   endof
      O-FCMPBRZ  of A64IR-OPCODE:FCMPBRZ  endof
      O-SELZD    of A64IR-OPCODE:SELZD    endof
      O-CMPSELD  of A64IR-OPCODE:CMPSELD  endof
      O-FCMPSEL   of A64IR-OPCODE:FCMPSEL   endof
      O-FCMPSELZ  of A64IR-OPCODE:FCMPSELZ  endof
      O-FCMPSELD  of A64IR-OPCODE:FCMPSELD  endof
      O-FCMPSELZD of A64IR-OPCODE:FCMPSELZD endof
      O-TAILCALL  of A64IR-OPCODE:TAILCALL  endof
      O-MADD      of A64IR-OPCODE:MADD      endof
      O-ADDI      of A64IR-OPCODE:ADDI      endof
      O-SUBI      of A64IR-OPCODE:SUBI      endof
      O-MOVN      of A64IR-OPCODE:MOVN      endof
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

\ ---- the registers this emission WRITES --------------------------------------
\ A destination register is noted as it is asked for, so what the finished
\ emission destroys is counted from the instructions it really built rather than
\ from the module it read. It is held against the accepted allocation's own
\ answer before the run seals, and the two together are what
\ src/compiler/native/publish.f may record about the routine: an emission that
\ wrote a register no value claimed is refused instead of published as a routine
\ that destroys less than it does.
\
\ THE COUNT IS PER FILE because a register number names a register of ONE file -
\ d3 and x3 are two registers and both are number three - so which file the value
\ lives in is asked of the same accepted allocation the number came from.
: NOTE-WRITE ( IR-ID:ir-value-id n -- )
   {: v:IR-ID:ir-value-id r:n :}
   v IR-ID:VALUE-LOCAL A64RAV:FLOATING? if
      1 r lshift  EM-WFPR @ or  EM-WFPR !
      exit
   then
   1 r lshift  EM-WGPR @ or  EM-WGPR ! ;

: RESULT-REG ( IR-ID:ir-op-id n -- n )
   RESULT-AT {: v:IR-ID:ir-value-id :}
   v REG-OF {: r:n :}
   v r NOTE-WRITE
   r ;

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

\ Whether an operation carries a key at all. ATTR-SLOT above refuses a key that
\ is missing, because every reader that asks for one is reading a field its
\ operation's schema requires; this is the other question, asked by a reader that
\ walks operations of every form and acts on the fields it FINDS.
: ATTR-HAS? ( IR-ID:ir-op-id IR-ID:ir-symbol-id -- bool )
   {: id:IR-ID:ir-op-id want:IR-ID:ir-symbol-id :}
   false
   id ATTRS-OF 0 ?do
      id i ATTR-KEY-AT want SAME-SYM? if drop true leave then
   loop ;

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

\ The arithmetic immediate, under the dialect's own key for it. It goes to the
\ encoder as the number it is: ENC-ADDI and ENC-SUBI bound their own field, so
\ no bound is repeated here for the same reason none of the frame ones is.
: OFF-IMM ( IR-ID:ir-op-id -- n )
   0 BND-OFF @ ATTR-INT ;

\ ---- the data-stack operands -------------------------------------------------
\ The same two readings against the other pointer, under the dialect's own keys
\ for them. They are separate keys and separate readers because a frame offset
\ and a data-stack offset are counted from different registers: one key answering
\ both would let a frame access encode as a data-stack access.
: DSLOT-OFF ( IR-ID:ir-op-id -- n )
   0 BND-DSLOT @ ATTR-INT ;

: DBYTES-SIZE ( IR-ID:ir-op-id -- n )
   0 BND-DBYTES @ ATTR-INT ;

\ The second adjustment, which only the call forms carry: how far the pointer
\ comes back down over what the callee left.
: DBACK-SIZE ( IR-ID:ir-op-id -- n )
   0 BND-DBACK @ ATTR-INT ;

\ The address a call to another word branches to. It is the callee's own entry
\ and not a displacement, so this pass does the one subtraction that turns it
\ into one - which is the whole reason the placement below has to be known here.
: ENTRY-ADDR ( IR-ID:ir-op-id -- n )
   0 BND-ENTRY @ ATTR-INT ;

\ ---- one instruction per operation -------------------------------------------
\ Each of these is exactly the encoder call the form names, with the registers
\ the accepted allocation answers and the operands the module carries.
: WORD-MOVZ ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id 0 RESULT-REG  id IMM-OF  id HALF-OF  MOVZHW ;

: WORD-MOVK ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id 0 RESULT-REG  id IMM-OF  id HALF-OF  MOVKHW ;

\ The move-wide that writes the COMPLEMENT of its shifted immediate. It reads its
\ two fields exactly as the two above do - the dialect holds the same key pair for
\ all three - and differs only in the encoder it hands them to.
: WORD-MOVN ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id 0 RESULT-REG  id IMM-OF  id HALF-OF  MOVNHW ;

\ The copy that puts a returned value where the routine's contract says it
\ leaves. ENC-MOV is the assembler's own name for the Orr-with-zero-register
\ form ARM64 spells a move as, so no second idiom is written here.
: WORD-MOV ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id 0 RESULT-REG  id 0 OPERAND-REG  ENC-MOV ;

\ The bitwise complement, which reads the same one register and writes the same
\ one. ENC-MVN is the assembler's own name for the Orn-with-zero-register form,
\ exactly as ENC-MOV is for the Orr one.
: WORD-MVN ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id 0 RESULT-REG  id 0 OPERAND-REG  ENC-MVN ;

\ The shifted-register three-operand forms differ only in which encoder they end
\ in, so they share the operand reading.
: TRIPLE ( IR-ID:ir-op-id -- n n n )
   {: id:IR-ID:ir-op-id :}
   id 0 RESULT-REG  id 0 OPERAND-REG  id 1 OPERAND-REG ;

\ The add and subtract immediate forms: one register written, one read, and the
\ third field is not a register at all but the number the operation carries. It
\ is the same reading shape TRIPLE has with its last register replaced by that
\ number, which is exactly the difference the form is for.
: PAIRI ( IR-ID:ir-op-id -- n n n )
   {: id:IR-ID:ir-op-id :}
   id 0 RESULT-REG  id 0 OPERAND-REG  id OFF-IMM ;

\ The zero register, which is what register 31 is in this instruction's addend
\ field. No value of the machine dialect stands for it and the allocator never
\ hands it out, so this cannot happen by the ordinary route - which is exactly
\ why it is asked. `madd rd, rn, rm, xzr` and `mul rd, rn, rm` are the SAME four
\ bytes, so an addend that arrived here as register 31 would silently emit a
\ multiply where the module says multiply-add, and the answer would be wrong by
\ the whole addend. formal/Common/Insn.v puts that word outside `wf` for the same
\ reason (`madd_mul_alias_at_xzr`); this is that boundary where the register is
\ finally known.
31 constant ZERO-REG

: ?ADDEND ( n -- n )
   dup ZERO-REG = if E-A64COMB-ADDEND throw then ;

\ The three-source form's four registers, in the order `madd rd, rn, rm, ra`
\ names them - which is the order the schema in src/compiler/native/a64ir.f
\ declares its operands in, so neither reader has to know the other's.
: QUAD ( IR-ID:ir-op-id -- n n n n )
   {: id:IR-ID:ir-op-id :}
   id 0 RESULT-REG  id 0 OPERAND-REG  id 1 OPERAND-REG  id 2 OPERAND-REG ?ADDEND ;

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

\ The floating forms. Every one of them is a register-to-register instruction of
\ the shape its schema declares, so they read their operands and their result the
\ way every other form here does and end in the assembler's own encoder. The two
\ conversions and the two crossings name registers of two different files in the
\ two fields, which the encoders know: ENC-SCVTF and ENC-FMOVXD take a D
\ destination and an X source, ENC-FCVTZS and ENC-FMOVDX the other way round.
: WORD-FNEG ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id 0 RESULT-REG  id 0 OPERAND-REG  ENC-FNEG ;

: WORD-FABS ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id 0 RESULT-REG  id 0 OPERAND-REG  ENC-FABS ;

: WORD-FSQRT ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id 0 RESULT-REG  id 0 OPERAND-REG  ENC-FSQRT ;

: WORD-SCVTF ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id 0 RESULT-REG  id 0 OPERAND-REG  ENC-SCVTF ;

: WORD-FCVTZS ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id 0 RESULT-REG  id 0 OPERAND-REG  ENC-FCVTZS ;

: WORD-FMOVXD ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id 0 RESULT-REG  id 0 OPERAND-REG  ENC-FMOVXD ;

: WORD-FMOVDX ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id 0 RESULT-REG  id 0 OPERAND-REG  ENC-FMOVDX ;

\ The copy of the D file: one register into another, both fields D. It is the
\ floating twin of a64.mov and it is elided under the same rule - see SELF-MOV?
\ below, which asks the operation what file its copy is in rather than which
\ opcode it is.
: WORD-FMOVDD ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id 0 RESULT-REG  id 0 OPERAND-REG  ENC-FMOVDD ;

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
\
\ WHICH ADDRESSING MODE AN ACCESS IS WRITTEN IN. The offset an access carries is
\ the distance from where the pointer stands to the cell it names, and the
\ placement in src/compiler/native/select.f stands the pointer where the fewest
\ adjustments are needed - so a cell can be under the pointer as easily as over
\ it. Over it is the scaled unsigned field, Ldr and Str; under it is the unscaled
\ signed field, Ldur and Stur. It is ONE dialect form written two ways rather
\ than two forms, because which way it goes is not a property of the access: the
\ same a64.dload names the same cell whichever place the routine happens to
\ stand at. Both are one instruction, so nothing about the layout turns on it.
: DENC-LDR ( n n n -- n )
   dup 0 < if ENC-LDUR exit then ENC-LDR ;

: DENC-STR ( n n n -- n )
   dup 0 < if ENC-STUR exit then ENC-STR ;

: WORD-DLOAD ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id 0 RESULT-REG  A64EFF:DSTACK-GPR  id DSLOT-OFF  DENC-LDR ;

: WORD-DSTORE ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id 0 OPERAND-REG  A64EFF:DSTACK-GPR  id DSLOT-OFF  DENC-STR ;

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

\ The same two accesses one byte wide. Ldrb and Strb are their own encodings
\ with their own unscaled twelve-bit offset field, so the width is which
\ ENCODER is called and nothing else about the instruction changes: same
\ destination register, same base register out of the accepted allocation, same
\ zero offset. A byte access encoded with ENC-LDR would read eight bytes where
\ the program asked for one, which is why the two are separate arms of the table
\ below rather than one arm reading a width off the operation.
: WORD-ABLOAD ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id 0 RESULT-REG  id 0 OPERAND-REG  ADDR-OFF  ENC-LDRB ;

: WORD-ABSTORE ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id 0 OPERAND-REG  id 1 OPERAND-REG  ADDR-OFF  ENC-STRB ;

\ ---- the caller's return address ---------------------------------------------
\ The same Str and Ldr the frame accesses are, against the same stack pointer,
\ moving the register the routine's contract has its own field for. It is asked
\ for by name rather than written here, for the same reason the stack pointer and
\ the data-stack pointer are: the one place that says why no routine may hold
\ state in x30 is also the one place that says where it does appear.
: WORD-LNKSTR ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   A64EFF:LINK-GPR  A64EFF:SP-GPR  id SLOT-OFF  ENC-STR ;

: WORD-LNKLDR ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   A64EFF:LINK-GPR  A64EFF:SP-GPR  id SLOT-OFF  ENC-LDR ;

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
\ Blocks are laid out in an order this pass CHOOSES, and the section after the
\ next one is where it is chosen. The module's own order - the order the selector
\ built the blocks in - is still the one every OTHER reader of the module numbers
\ instructions by, and it still names every block: an ordinal is a block's name
\ here as it is everywhere else, and the label table above is keyed by it. What
\ this pass decides is only which block's instructions are WRITTEN next, which is
\ what the fall-through rule below is a question about.
\
\ Which blocks there are to name. A block of the function being emitted is one
\ this pass laid out, so an ordinal outside that range is a module this layout
\ cannot serve rather than a branch to nowhere; and a successor is such an
\ ordinal. Both are asked during the layout as well as during the emission,
\ because the layout has to know which block a terminator's trailing branch names
\ before it can decide whether that branch is emitted at all.
: BLK-ORD-CK ( n -- n )
   dup 0 < over N-BLK @ >= or if E-A64EMIT-BLOCK throw then ;

: SUCC-BLOCK ( IR-ID:ir-op-id n -- n )
   SUCC-AT IR-ID:BLOCK-LOCAL BLK-ORD-CK ;

\ How many instructions a FORM is, is a property of the form: one for all but the
\ three comparisons, the division, the two calls and the three
\ compare-and-branches, which are three each, and the two-way branch and the
\ eight conditional selects, which are two.
: INSNS-OF ( n -- n )
   {: k:n :}
   k O-SELZ = if 2 exit then
   k O-CMPSEL = if 2 exit then
   k O-SELZD = if 2 exit then
   k O-CMPSELD = if 2 exit then
   k O-FCMPSEL = if 2 exit then
   k O-FCMPSELZ = if 2 exit then
   k O-FCMPSELD = if 2 exit then
   k O-FCMPSELZD = if 2 exit then
   k O-FLAG = if 3 exit then
   k O-FFLAG = if 3 exit then
   k O-FFLAGZ = if 3 exit then
   k O-SDIV = if 3 exit then
   k O-CALL = if 3 exit then
   k O-WORDCALL = if 3 exit then
   k O-CMPBR = if 3 exit then
   k O-FCMPBR = if 3 exit then
   k O-FCMPBRZ = if 3 exit then
   k O-BRZ = if 2 exit then
   1 ;

\ How many instructions an OPERATION is, is that count less the two instructions
\ it can turn out not to need: a trailing branch to the block laid out next, and
\ a copy from a register into itself. The next four words say which.
\
\ Three forms end in an unconditional branch: the one-way branch is nothing else,
\ and the two-way branch and the compare-and-branch each end in one after their
\ conditional. This says which successor that trailing branch names, and -1 for
\ every form that ends in no such branch - the return, and everything that is not
\ a terminator at all.
: TAIL-SUCC ( n -- n )
   {: k:n :}
   k O-BR = if 0 exit then
   k O-BRZ = if 1 exit then
   k O-CMPBR = if 1 exit then
   k O-FCMPBR = if 1 exit then
   k O-FCMPBRZ = if 1 exit then
   -1 ;

\ ---- the chosen block order --------------------------------------------------
\ WHICH BRANCHES GET DELETED IS A CHOICE, AND THIS IS WHERE IT IS MADE. The rule
\ below deletes a terminator's trailing unconditional branch when its target is
\ the block written next. Written next was, until this section existed, whatever
\ the elaborator's build order happened to make it - so a loop written
\ `begin … while … repeat` had its exit stub built between the header and the
\ body, the header's trailing branch to the body could not fall through, and
\ neither could the stub's branch to the block after the loop. Two branches, in
\ every loop of that shape, deleted by nothing but laying the blocks out in a
\ different order.
\
\ THE RULE, IN ONE SENTENCE. Block zero is written first, because it is the
\ routine's entry - the caller enters at the first byte and the prologue is in it.
\ The block control leaves the routine through is written last, because the
\ record published for the routine says so - see RET-ORD below. After each block,
\ write the block its trailing unconditional branch names, if that block has not
\ been written yet; otherwise write the lowest-numbered block that has not been
\ written yet.
\
\ WHY THE TRAILING SUCCESSOR AND NOT THE OTHER ONE. It is the only successor
\ whose edge costs an instruction that laying it next would remove. A two-way
\ branch's FIRST successor is reached by the conditional, which is emitted
\ wherever that block sits; its second is reached by the unconditional below it,
\ which is the instruction the rule deletes. So the likeliest successor, in the
\ only sense this pass can pay for, is the one the trailing branch names - and
\ nothing here has to guess which way a test will go, because whichever way the
\ selection wired it, that is the arm the four bytes are on.
\
\ FOR AN UNFUSED LOOP TEST THAT IS THE BODY, and the loop comes out the shape a
\ reader expects: the header falls into the body, the exit stub sinks below the
\ latch, and the only unconditional branch left is the back edge. For a loop
\ whose test FUSED into the branch it is the exit stub instead, because
\ src/compiler/native/select.f wires the condition-holds arm first and staying in
\ the loop is what the condition holding means - so the stub gets the
\ fall-through, and the body and the back edge keep a branch each. That is one
\ branch more than the shape needs and this pass cannot take it: laying the body
\ next instead would mean inverting the conditional's condition, which is a
\ decision about an instruction and not about an order (dot
\ habu-choose-which-arm-ffe23e64).
\
\ WHY THE FALLBACK IS THE LOWEST BLOCK LEFT. A trace ends where its successor has
\ already been written - at a back edge, or at a join two arms reach. What comes
\ next then has to come from somewhere, and the module's own order is the one
\ answer that is already agreed: taking the lowest block not yet written keeps
\ the emission as close to the build order as the traces allow, so a routine
\ whose build order was already the best order is written out unchanged.
\
\ WHY THIS IS THE SAME OWNER AS THE LAYOUT AND NOT A PASS BEFORE IT. Three
\ readers ask the order: the layout asks which block to count next, the writer
\ asks which block to write next, and the fall-through rule asks where a branch's
\ TARGET sits relative to its own block. CURSOR-CK holds the first two against
\ each other at every block boundary, and that check only means anything if all
\ three are reading one table. Choosing the order also needs exactly what this
\ file already holds and nothing else: the dialect binding that says which opcode
\ a terminator is, and TAIL-SUCC, which says which successor the trailing branch
\ names. A separate file would need a second binding over the same module and a
\ second statement of TAIL-SUCC - the second authority on one rule this file
\ refuses everywhere else.
\
\ AND IT CHANGES NO DISPLACEMENT'S REACH. Every branch between blocks of one
\ routine is measured inside that routine, and INSN-MAX above bounds a routine at
\ three instructions per operation over the value and block ceilings NFROZEN
\ commits to. The narrowest field any of these branches has is the nineteen bits
\ the conditional and the compare-against-zero forms carry, which reaches
\ 2^18 instructions either way - two orders of magnitude past the longest routine
\ that can exist here. So no permutation of the blocks can put a branch out of
\ reach. The reach check stays where it is anyway, because it is about the
\ encoder masking its field and not about the layout.
\
\ WHAT IT DOES NOT TOUCH. The register allocation. That is computed over the
\ module's own block order, before this pass runs, and it is a function of the
\ module and the register budget alone - no interference, no hull and no
\ coalescing decision reads a byte offset or a layout position. This pass reads
\ the accepted assignment (SELF-MOV? below) and never the other way round. So a
\ value whose hull is stretched because the module records a loop's exit stub
\ between its header and its body has exactly the same hull after this
\ reordering: moving where the stub is WRITTEN does not move where it is
\ RECORDED. That artefact belongs to whoever changes the recorded order or the
\ coalescing, and nothing here can reach it.

\ Where a block sits in the order, and which block sits at a position. A position
\ is a number of the same range an ordinal is - the order is a permutation of the
\ block ordinals - so one bound check serves both.
: AT-POS ( n -- n )
   BLK-ORD-CK cells B-ORDER + @ ;

: POS-OF ( n -- n )
   BLK-ORD-CK cells B-PLACE + @ ;

\ Has this block been given a position yet? Every row starts below zero, so the
\ question is answered by the table the answer is written into rather than by a
\ second count that could disagree with it.
: LAID? ( n -- bool )
   BLK-ORD-CK cells B-PLACE + @ 0 >= ;

: LAY ( n n -- )
   {: b:n p:n :}
   b BLK-ORD-CK {: bb:n :}
   p BLK-ORD-CK {: pp:n :}
   bb  pp cells B-ORDER + !
   pp  bb cells B-PLACE + ! ;

\ The block a terminator's trailing unconditional branch names, or -1 when the
\ block ends in no such branch. It is TAIL-SUCC asked of a whole block, which is
\ the form the ordering needs: the elision rule asks it of an operation.
: TAIL-BLOCK ( IR-ID:ir-block-id -- n )
   TERM-AT {: t:IR-ID:ir-op-id :}
   t SLOT-AT TAIL-SUCC {: s:n :}
   s 0 < if -1 exit then
   t s SUCC-BLOCK ;

\ The lowest-numbered block with no position yet, or -1 when every block has one.
: NEXT-UNLAID ( -- n )
   0 begin dup N-BLK @ < while
      dup LAID? 0= if exit then
      1+
   repeat
   drop -1 ;

\ Which block follows this one: the one its trailing branch names when that block
\ is still unwritten, and otherwise the lowest block left.
: FOLLOWER ( IR-ID:ir-fun-id n -- n )
   {: f:IR-ID:ir-fun-id b:n :}
   f b BLOCK-AT TAIL-BLOCK {: s:n :}
   s 0 < if NEXT-UNLAID exit then
   s LAID? if NEXT-UNLAID exit then
   s ;

\ THE LAST BLOCK WRITTEN IS THE ONE CONTROL LEAVES THE ROUTINE THROUGH, AND THAT
\ IS NOT A PREFERENCE. src/compiler/native/publish.f records a word's code length
\ as the emission LESS ONE INSTRUCTION, because the engine's records exclude a
\ word's trailing return - that is the span its inliner copies into a caller
\ (src/habu/habu2.f EM-COMPILE-FLUSH-PEND). So the emission's last instruction has
\ to BE the return. Left to itself the trace above would happily end a routine on
\ a loop's back edge, and the record published for it would then be a body with
\ its last branch cut off: the routine still runs, because the return is reached
\ in the middle, and every reader of the record - the inliner, the workload scan,
\ the redirection seam - is one instruction wrong about it. The trace is
\ therefore run over every block but this one, and this one is written last.
\
\ WHICH BLOCK THAT IS, RE-DERIVED HERE. The one whose terminator names no
\ successor. src/compiler/native/regalloc.f decides the same thing the same way
\ (MB-RET-ORD) and refuses a routine with none or with two, so a module that
\ reached this pass has exactly one; it is measured again rather than taken on
\ trust, because this pass is about to make the whole emission end in it.
: RET-ORD ( IR-ID:ir-fun-id -- n )
   {: f:IR-ID:ir-fun-id :}
   -1
   f BLOCK-COUNT 0 ?do
      f i BLOCK-AT TERM-AT SUCCS-OF 0= if
         dup 0 < 0= if E-A64EMIT-SHAPE throw then
         drop i
      then
   loop
   dup 0 < if E-A64EMIT-SHAPE throw then ;

\ The order itself, decided before a single instruction is counted. The block
\ count is established and bounded here rather than in LAYOUT below, because this
\ is now the first word that reads it - and because a shape this pass cannot
\ serve should be refused as that before any register assignment is read.
\
\ THE TWO ENDS ARE PINNED FIRST AND THE TRACE FILLS WHAT IS BETWEEN THEM. Block
\ zero is the entry, so it takes the first position; the return block takes the
\ last. A routine of one block is both, which is the only way they can be the
\ same block - a longer routine whose entry is also its exit has no path to any
\ of its other blocks, and it is refused here rather than emitted as a record the
\ seam would mis-measure. The trace then fills the positions between, and the
\ return block is already placed, so it is never picked up early.
\
\ LAY is what bounds the two numbers it writes, so a follower outside the
\ function's blocks is E-A64EMIT-BLOCK here rather than a row written past the
\ end of a table. It cannot happen: fewer than N-BLK blocks are laid when a
\ position is still to be filled, so NEXT-UNLAID always has one to answer with.
: ORDER-BLOCKS ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   f BLOCK-COUNT {: n:n :}
   n 1 < if E-A64EMIT-SHAPE throw then
   n BMAX > if E-A64EMIT-CAP throw then
   n N-BLK !
   f RET-ORD {: r:n :}
   n 0 ?do  -1 i cells B-PLACE + !  loop
   r n 1- LAY
   n 1 = if exit then
   r 0= if E-A64EMIT-SHAPE throw then
   0 0 LAY
   n 1- 1 ?do
      f  i 1- AT-POS  FOLLOWER  i LAY
   loop ;

\ THE RULE, WRITTEN ONCE. An operation's trailing unconditional branch is reached
\ by falling into it when the block it names is the block laid out immediately
\ after the one the operation terminates - and then it is not emitted at all.
\ The layout below subtracts it from the form's count and PUT-BR, PUT-BRZ and
\ PUT-CMPBR leave out the same instruction by asking this same word, so there is
\ no second statement of the rule that could come to disagree with the first.
\
\ IT IS ASKED IN POSITIONS AND ANSWERED ABOUT ORDINALS. The operation arrives
\ with the ORDINAL of the block it terminates, because that is what every caller
\ of it has, and the successor it names is an ordinal too; "immediately after" is
\ a statement about where the two were laid out, so both go through the order.
\ Nothing here depends on where any block STARTS, which is why it can still be
\ asked during the layout that is about to decide exactly that.
: FALL-THRU? ( IR-ID:ir-op-id n -- bool )
   {: id:IR-ID:ir-op-id home:n :}
   id SLOT-AT TAIL-SUCC {: s:n :}
   s 0 < if false exit then
   id s SUCC-BLOCK POS-OF  home POS-OF 1+ = ;

\ THE SECOND RULE, ALSO WRITTEN ONCE. A copy whose source and destination are the
\ same register moves that register into itself, which is no instruction at all,
\ and it is not emitted. The register allocator prefers one register for both
\ ends of a copy wherever the two do not interfere - step five of
\ src/compiler/native/regalloc.f - so this is what deletes the copy an
\ argument-carrying edge is split with; but nothing here asks whether the
\ allocator meant it to. The rule is register equality and only that, so a copy
\ whose ends happened to land in one register for any other reason goes the same
\ way.
\
\ THERE ARE TWO COPIES AND ONE RULE. a64.mov copies a general register and
\ a64.fmovdd copies a floating one, and the elision is the same statement about
\ both: same register in, same register out, no instruction. COPY? is what makes
\ it one statement - a form added to either file has to be named there and
\ nowhere else, and a second `<>` test beside this one is exactly the copy of the
\ rule that could come to disagree with it. The register NUMBER is what is
\ compared, and the two files are separately numbered, which is sound here
\ because a copy's ends are one class and a class has one file: the allocator
\ refuses a class spanning the two by name (E-A64RA-FILE), so `d3 = x3` is not a
\ comparison this word can be asked to make.
\
\ IT IS ASKED THROUGH THE SAME DOOR AS EVERY OTHER REGISTER. OPERAND-REG and
\ RESULT-REG are A64RAV:REG@, the one checked answer in the chain, so a stale or
\ unaccepted assignment refuses here exactly as it refuses when an instruction is
\ being encoded. What it costs is that the layout can no longer be computed
\ before the assignment has been accepted: how many instructions a copy is, is
\ now a fact about the allocation. EMIT below therefore probes the assignment
\ before it lays the blocks out, and says so.
: COPY? ( IR-ID:ir-op-id -- bool )
   {: id:IR-ID:ir-op-id :}
   id SLOT-AT {: k:n :}
   k O-MOV =  k O-FMOVDD =  or ;

: SELF-MOV? ( IR-ID:ir-op-id -- bool )
   {: id:IR-ID:ir-op-id :}
   id COPY? 0= if false exit then
   id 0 RESULT-REG  id 0 OPERAND-REG  = ;

\ How many of an operation's data-stack adjustments are no instruction at all.
\ The question is asked of the operation and not of its opcode - which is the
\ rule every reader in this file follows - so an operation carrying one
\ adjustment answers about that one, a call carrying two answers about both, and
\ an operation carrying neither answers nothing.
: DZERO1 ( IR-ID:ir-op-id IR-ID:ir-symbol-id -- n )
   {: id:IR-ID:ir-op-id key:IR-ID:ir-symbol-id :}
   id key ATTR-HAS? 0= if 0 exit then
   id key ATTR-INT 0= if 1 exit then
   0 ;

: DZERO-MOVES ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id 0 BND-DBYTES @ DZERO1
   id 0 BND-DBACK @ DZERO1 + ;

: OP-INSNS ( IR-ID:ir-op-id n -- n )
   {: id:IR-ID:ir-op-id home:n :}
   id SLOT-AT INSNS-OF
   id home FALL-THRU? if 1- then
   id SELF-MOV? if 1- then
   id DZERO-MOVES - ;

: BLOCK-INSNS ( IR-ID:ir-block-id n -- n )
   {: bk:IR-ID:ir-block-id home:n :}
   0
   bk OP-COUNT 0 ?do
      bk i OP-AT home OP-INSNS +
   loop ;

: START-AT ( n -- n )
   BLK-ORD-CK cells B-START + @ ;

\ Where each block's first instruction lands, measured in instructions from the
\ start of the routine. It is computed before a single byte is written, because a
\ forward branch has to know where it is going before it can be encoded.
\
\ THE WALK IS OVER POSITIONS AND THE TABLE IS KEYED BY ORDINALS, which is the
\ whole of what the chosen order changes here. Blocks are counted in the order
\ they will be written - that is what makes each one's start the sum of the
\ instructions written before it - and each one's start is filed under its own
\ name, because a branch names a block and not a position.
\
\ Each block is counted knowing its own ordinal, because that is what its
\ terminator's fall-through question is asked against. LAY-AT is left holding the
\ whole routine's instruction count, which is what WALK holds its cursor against
\ when it has emitted the last block.
: LAYOUT ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   0 LAY-AT !
   N-BLK @ 0 ?do
      LAY-AT @ i AT-POS cells B-START + !
      LAY-AT @  f i AT-POS BLOCK-AT  i AT-POS BLOCK-INSNS  +  LAY-AT !
   loop ;

\ ---- the branches ------------------------------------------------------------
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

\ The conditional branch reaches as far as its own nineteen-bit displacement
\ field, which is asked for by that form's own name: ENC-BCOND masks the field
\ exactly as the other two encoders do, so a target out of reach would become a
\ target somewhere else rather than a refusal.
: BCOND-WORD ( n n -- n )
   {: d:n k:n :}
   d A64IR:BCOND-FITS? 0= if E-A64EMIT-REACH throw then
   d k ENC-BCOND ;

\ Going to one block, handing it its arguments. The arguments are already in the
\ registers the destination's block arguments were given - that is the register
\ allocation's own decision and the validator has agreed with it - so the
\ operands reach no encoder here and the whole instruction is the jump.
\
\ And when the block it goes to is the one laid out next, the jump is the fall
\ into it and there is no instruction at all. FALL-THRU? is the layout's own
\ word, asked here with the same two arguments, so the instruction left out is
\ exactly the instruction the layout did not count.
: PUT-BR ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id home:n :}
   id home FALL-THRU? if exit then
   id  id 0 SUCC-BLOCK DELTA B-WORD  APPEND ;

\ The two-way branch: go to the first successor when the tested register is
\ zero, and to the second when it is not. The conditional is always emitted; the
\ unconditional below it is the one the fall-through rule can delete, and when it
\ does the second successor is reached by running off the end of this block into
\ the block laid out next, which is that same successor.
: PUT-BRZ ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id home:n :}
   id  id 0 OPERAND-REG  id 0 SUCC-BLOCK DELTA  BZ-WORD  APPEND
   id home FALL-THRU? if exit then
   id  id 1 SUCC-BLOCK DELTA B-WORD  APPEND ;

\ The fused compare-and-branch, which is three instructions: compare the two
\ registers, go to the first successor when the condition the operation carries
\ holds, and go to the second when it does not. The comparison writes only the
\ condition flags and the branch beside it reads them there, so no register is
\ written and no flag is materialised - which is the whole difference from the
\ pair of operations this replaces.
\
\ THE UNCONDITIONAL HALF IS THE ONE THAT CAN GO, and this is the form it goes
\ from most often: src/compiler/native/select.f wires the condition-true arm
\ first, which leaves the second successor - the arm the condition did not choose
\ - as the block laid out next. So the usual emission of this operation is two
\ instructions, the compare and the conditional, and the not-taken path falls
\ into its own successor. The conditional branch's displacement is measured after
\ the compare has been appended, because a displacement is counted from the
\ instruction that carries it.
: PUT-CMPBR ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id home:n :}
   id  id 0 OPERAND-REG id 1 OPERAND-REG ENC-CMP  APPEND
   id  id 0 SUCC-BLOCK DELTA  id COND-OF  BCOND-WORD  APPEND
   id home FALL-THRU? if exit then
   id  id 1 SUCC-BLOCK DELTA B-WORD  APPEND ;

\ The two fused FLOAT compare-and-branches, which are the same three instructions
\ with an Fcmp in front instead of a Cmp - the two-register form for the three
\ comparisons that take two doubles, and the compare-against-zero form for the
\ two that take one. Everything after the first instruction is identical to the
\ integer form's, deliberately: the conditional branch reads the flags the same
\ way whichever instruction wrote them, and the trailing unconditional half goes
\ under exactly the same fall-through rule.
\
\ WHAT THE Fcmp DOES THAT THE Cmp DOES NOT is raise the unordered condition when
\ either operand is a NaN, and that is the whole of how a compiled float branch
\ keeps the interpreted word's answer for a NaN. The conditions
\ src/compiler/native/select.f names - MI, GT and EQ - are all false under it, so
\ the conditional below is NOT taken and control reaches the second successor,
\ which the selection wired to the arm the source's `if` takes when its flag is
\ zero. No check is written here because there is nothing to check: the rule is
\ the instruction's and the condition's.
: PUT-FCMPBR ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id home:n :}
   id  id 0 OPERAND-REG id 1 OPERAND-REG ENC-FCMP  APPEND
   id  id 0 SUCC-BLOCK DELTA  id COND-OF  BCOND-WORD  APPEND
   id home FALL-THRU? if exit then
   id  id 1 SUCC-BLOCK DELTA B-WORD  APPEND ;

: PUT-FCMPBRZ ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id home:n :}
   id  id 0 OPERAND-REG ENC-FCMP0  APPEND
   id  id 0 SUCC-BLOCK DELTA  id COND-OF  BCOND-WORD  APPEND
   id home FALL-THRU? if exit then
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

\ The two float comparisons that answer a number, which are the same three
\ instructions with an Fcmp in front. The Cset and the negation are the general
\ file's, because a Habu flag is a number and lives there whichever file the
\ values compared came out of; this is the sequence the engine's own (FCMP) and
\ (FCMP0) emit, so a compiled float comparison answers what an interpreted one
\ answers - all bits set or none, and none for a NaN.
: PUT-FFLAG ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id 0 RESULT-REG {: rd:n :}
   id  id 0 OPERAND-REG id 1 OPERAND-REG ENC-FCMP  APPEND
   id  rd id COND-OF ENC-CSET  APPEND
   id  rd rd ENC-NEG  APPEND ;

: PUT-FFLAGZ ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id 0 RESULT-REG {: rd:n :}
   id  id 0 OPERAND-REG ENC-FCMP0  APPEND
   id  rd id COND-OF ENC-CSET  APPEND
   id  rd rd ENC-NEG  APPEND ;

\ The two conditional selects, each two instructions: write the flags, then move
\ one of two registers into the result on the condition. Nothing branches, so
\ neither of them asks the fall-through rule anything and neither carries a
\ successor - they are ordinary value operations that happen to read the flags
\ the instruction in front of them wrote.
\
\ THE FIRST SOURCE IS THE CONDITION-HOLDS ANSWER in both, which is the same
\ order a64.cmpbr puts its successors in and the order the dialect's own note
\ states. A Csel writes its first source register when the condition holds and
\ its second when it does not, so the operand order and the instruction agree
\ without anything here turning them round.
: PUT-SELZ ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id  id 0 OPERAND-REG 0 ENC-CMPI  APPEND
   id  id 0 RESULT-REG  id 1 OPERAND-REG  id 2 OPERAND-REG
       A64IR-COND:NE A64IR:COND-CODE  ENC-CSEL  APPEND ;

: PUT-CMPSEL ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id  id 0 OPERAND-REG id 1 OPERAND-REG ENC-CMP  APPEND
   id  id 0 RESULT-REG  id 2 OPERAND-REG  id 3 OPERAND-REG
       id COND-OF  ENC-CSEL  APPEND ;

\ The same two, choosing between DOUBLES. Each is its partner above with the
\ second instruction moved to the D file: the Cmp is unchanged, because what
\ decides the arm is a cell either way, and the Csel becomes an Fcsel because
\ the registers it moves are D registers. The operand positions are the same
\ four, so the condition-holds answer is still the first source and the polarity
\ argument above covers both pairs at once. The three register numbers a Csel
\ names and the three an Fcsel names come out of the same allocation through the
\ same door - REG-OF - and which FILE each one is a number in is a property of
\ the value the operand names, which is what the schema's operand types settled.
: PUT-SELZD ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id  id 0 OPERAND-REG 0 ENC-CMPI  APPEND
   id  id 0 RESULT-REG  id 1 OPERAND-REG  id 2 OPERAND-REG
       A64IR-COND:NE A64IR:COND-CODE  ENC-FCSEL  APPEND ;

: PUT-CMPSELD ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id  id 0 OPERAND-REG id 1 OPERAND-REG ENC-CMP  APPEND
   id  id 0 RESULT-REG  id 2 OPERAND-REG  id 3 OPERAND-REG
       id COND-OF  ENC-FCSEL  APPEND ;

\ The four whose flags an FLOAT compare wrote. Each is one of the four above
\ with its first instruction changed from a Cmp over cells to the Fcmp over
\ doubles the source really wrote, and nothing after that first instruction
\ differs: a Csel and an Fcsel read the flags the same way whichever instruction
\ left them, and the condition-holds answer is still the first source.
\
\ WHAT THE Fcmp DOES THAT THE Cmp DOES NOT is raise the unordered condition when
\ either operand is a NaN, and that is the whole of how a fused float select
\ keeps the interpreted word's answer for one. The conditions
\ src/compiler/native/select.f names for a float comparison - MI, GT and EQ - are
\ all false under it, so the select writes its SECOND source, which the selection
\ wired to the arm the source's `if` takes when its flag is zero. No check is
\ written here because there is nothing to check: the rule is the instruction's
\ and the condition's, exactly as it is in PUT-FCMPBR above.
\
\ THE TWO ZERO FORMS READ THEIR CONDITION OFF THE OPERATION and not off `ne` the
\ way PUT-SELZ and PUT-SELZD do. Those two test a Habu flag a program computed,
\ so the only question is whether it is zero; these two compare a double against
\ the immediate zero, so which relation is being asked - `f0<` or `f0=` - is the
\ condition the operation carries.
: PUT-FCMPSEL ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id  id 0 OPERAND-REG id 1 OPERAND-REG ENC-FCMP  APPEND
   id  id 0 RESULT-REG  id 2 OPERAND-REG  id 3 OPERAND-REG
       id COND-OF  ENC-CSEL  APPEND ;

: PUT-FCMPSELZ ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id  id 0 OPERAND-REG ENC-FCMP0  APPEND
   id  id 0 RESULT-REG  id 1 OPERAND-REG  id 2 OPERAND-REG
       id COND-OF  ENC-CSEL  APPEND ;

: PUT-FCMPSELD ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id  id 0 OPERAND-REG id 1 OPERAND-REG ENC-FCMP  APPEND
   id  id 0 RESULT-REG  id 2 OPERAND-REG  id 3 OPERAND-REG
       id COND-OF  ENC-FCSEL  APPEND ;

: PUT-FCMPSELZD ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id  id 0 OPERAND-REG ENC-FCMP0  APPEND
   id  id 0 RESULT-REG  id 1 OPERAND-REG  id 2 OPERAND-REG
       id COND-OF  ENC-FCSEL  APPEND ;

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

\ ---- moving the data-stack pointer -------------------------------------------
\ One adjustment, and the whole of what an adjustment is: a distance, in
\ whichever direction its sign names, and NO INSTRUCTION AT ALL when it is zero.
\ Zero is the ordinary case rather than the exception - the placement in
\ src/compiler/native/select.f stands the routine's pointer where the most of
\ these come out zero - and an `add x19, x19, #0` would be an instruction that
\ moves nothing, written because a field happened to be there.
\
\ THE ELISION IS THE LAYOUT'S RULE TOO. DZERO-MOVES above answers, from the
\ operation, how many of its adjustments are nothing, and BOTH the layout and
\ this word are that one answer - the same discipline SELF-MOV? and FALL-THRU?
\ keep, and for the same reason: an instruction the layout counted and the
\ emitter did not write moves every branch after it.
: PUT-DMOVE ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id d:n :}
   d 0= if exit then
   d 0 > if
      id  A64EFF:DSTACK-GPR A64EFF:DSTACK-GPR d  ENC-ADDI  APPEND
      exit
   then
   id  A64EFF:DSTACK-GPR A64EFF:DSTACK-GPR d negate  ENC-SUBI  APPEND ;

\ The routine's own two, which differ only in which way the field is read: the
\ entry field says how far DOWN from where the caller left the pointer the body
\ stands, and the exit field how far UP from there the results are published.
: PUT-DTAKE ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id  id DBYTES-SIZE negate  PUT-DMOVE ;

: PUT-DPUBLISH ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id  id DBYTES-SIZE  PUT-DMOVE ;

\ One call, which is the branch and up to two adjustments: the data-stack
\ pointer to the base the callee is entered at, the branch that leaves the return
\ address in the link register, and the pointer back to where the body stands.
\ The two adjustments are the operation's own fields and either of them can be a
\ distance of nothing, which is no instruction; only the branch is this pass's
\ arithmetic, and only the branch is always written.
\
\ THE TARGET IS BLOCK ZERO OF THE ROUTINE BEING EMITTED, which is where the
\ caller entered and therefore where the callee has to enter: the prologue that
\ takes the frame, saves the return address and reads the arguments is the first
\ thing in it. Its displacement is measured exactly as a branch's is - the block's
\ start less this instruction's own position - and it is held against the field
\ the dialect declares for the Bl form before the encoder is called, because that
\ encoder masks its displacement field rather than bounding it.
0 constant CALL-BLOCK                \ the routine's own entry

: BL-WORD ( n -- n )
   {: d:n :}
   d A64IR:B-FITS? 0= if E-A64EMIT-REACH throw then
   d ENC-BL ;

\ What a branch out of this routine adds to what the routine destroys. A
\ self-call adds nothing at all, and that is not an omission: the callee IS this
\ routine, so what it destroys is what is being counted here and the union with
\ itself changes nothing. A call to another word adds that word's own recorded
\ answer, or - for a word this process has no row for, which is every word the
\ engine's own emitter compiled - the whole register file, because nothing is
\ known about it.
: NOTE-CALLEE ( n -- )
   {: e:n :}
   e A64EFF:GPR-ALL NCLOB:GPR-CLOB A64EFF:GPRS-N  EM-KGPR @ or  EM-KGPR !
   e A64EFF:FPR-ALL NCLOB:FPR-CLOB A64EFF:FPRS-N  EM-KFPR @ or  EM-KFPR ! ;

: PUT-CALL ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id  id DBYTES-SIZE  PUT-DMOVE
   id  CALL-BLOCK DELTA BL-WORD  APPEND
   id  id DBACK-SIZE negate  PUT-DMOVE ;

\ ---- calling another word ----------------------------------------------------
\ The same one to three instructions, and only the branch is computed
\ differently.
\ A self-call's target is block zero of this routine, so its displacement is the
\ label table's answer; this one's target is an address, so the displacement is
\ that address less the address the branch instruction itself will occupy - which
\ is the placement this pass was told plus the instructions written before it.
\
\ WHY THIS NEEDS NO RELOCATION TABLE AND NOTHING TO PATCH. A relocation exists to
\ carry a displacement across the moment a value it depends on becomes known. Here
\ nothing becomes known later: the callee is a word already compiled and
\ published, so its address is fixed before this routine is even selected, and
\ where this routine lands is decided by the publication seam BEFORE the emission
\ is made and handed here. So the displacement is exact when the instruction is
\ encoded, exactly as a block branch's is, and the emitter stays the only writer
\ of a byte of this routine. The alternative - emit a branch to nowhere and let
\ the seam patch it through the source map - would make the seam an instruction
\ encoder, and then two files would decide what a Bl is.
\
\ AND BOTH ENDS ARE INSTRUCTION ALIGNED BY CONSTRUCTION, so the subtraction is a
\ whole number of instructions: src/compiler/native/a64ir.f refuses an entry that
\ is not, and PLACE-AT below refuses a placement that is not. The reach is asked
\ against the Bl field the same way every other branch's is, because that encoder
\ masks its displacement rather than bounding it.
: PLACEMENT-CK ( -- n )
   EM-PLACED @ 0= if E-A64EMIT-PLACE throw then
   EM-PLACE @ ;

: WORD-DELTA ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id ENTRY-ADDR  PLACEMENT-CK -  INSN-BYTES /  N-INS @ - ;

: PUT-WORD-CALL ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id ENTRY-ADDR NOTE-CALLEE
   id  id DBYTES-SIZE  PUT-DMOVE
   id  id WORD-DELTA BL-WORD  APPEND
   id  id DBACK-SIZE negate  PUT-DMOVE ;

\ ---- leaving through another word --------------------------------------------
\ The tail branch, which is ONE instruction and never more. It is the same
\ displacement the word call above computes - the callee's entry less the address
\ this instruction will occupy - encoded into a B instead of a Bl, so the callee's
\ own return goes to the address x30 already holds, which is OUR caller's.
\
\ THERE IS NO ADJUSTMENT AND NO SECOND INSTRUCTION, and that is a property of the
\ form rather than a case that happens to be empty: the selector only builds this
\ operation where the data-stack pointer already stands at the callee's entry
\ base, and the schema carries no adjustment field for it to have written one in.
\
\ IT NOTES ITS CALLEE EXACTLY AS A CALL DOES. What this routine destroys covers
\ what the routine it leaves through destroys - the branch is the last thing that
\ happens here, but a caller of THIS routine gets the callee's registers written
\ under this routine's name, so the union is the same union.
: PUT-TAILCALL ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id ENTRY-ADDR NOTE-CALLEE
   id  id WORD-DELTA B-WORD  APPEND ;

\ One copy, which is one instruction unless it is a copy from a register into
\ itself, and then it is none. SELF-MOV? is the layout's own word, asked here
\ with the same argument, so the instruction left out is exactly the instruction
\ the layout did not count. An elided copy gets no source-map row for the same
\ reason an elided branch gets none: a row's index is which instruction WORD@
\ answers at it.
: PUT-MOV ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id SELF-MOV? if exit then
   id  id WORD-MOV  APPEND ;

\ The same, in the other register file. It asks the same word for the same
\ reason, so a floating copy the allocator coalesced away costs no instruction
\ either - and the layout, which subtracts SELF-MOV? from every operation's
\ count, has already left the same one out.
: PUT-FMOVDD ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id SELF-MOV? if exit then
   id  id WORD-FMOVDD  APPEND ;

\ ---- one operation, as the instructions it is --------------------------------
\ The whole encoding table. Every arm names the instructions one machine
\ operation becomes; nothing else in this file decides which bytes an operation
\ is.
\
\ The block ordinal comes down with the operation because three of the arms need
\ it: a terminator's trailing branch is emitted or not according to which block
\ was laid out after the one it terminates, and nothing about the operation
\ itself says which block that is.
: PUT-OP ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id home:n :}
   id SLOT-AT SLOT-OPCODE
   MATCH A64IR:opcode
      movz     OF id  id WORD-MOVZ  APPEND ENDOF
      movk     OF id  id WORD-MOVK  APPEND ENDOF
      movn     OF id  id WORD-MOVN  APPEND ENDOF
      mov      OF id PUT-MOV ENDOF
      add      OF id  id TRIPLE ENC-ADD  APPEND ENDOF
      sub      OF id  id TRIPLE ENC-SUB  APPEND ENDOF
      mul      OF id  id TRIPLE ENC-MUL  APPEND ENDOF
      madd     OF id  id QUAD ENC-MADD  APPEND ENDOF
      addi     OF id  id PAIRI ENC-ADDI  APPEND ENDOF
      subi     OF id  id PAIRI ENC-SUBI  APPEND ENDOF
      sdiv     OF id PUT-SDIV ENDOF
      and      OF id  id TRIPLE ENC-AND  APPEND ENDOF
      orr      OF id  id TRIPLE ENC-ORR  APPEND ENDOF
      eor      OF id  id TRIPLE ENC-EOR  APPEND ENDOF
      lslv     OF id  id TRIPLE ENC-LSLV  APPEND ENDOF
      lsrv     OF id  id TRIPLE ENC-LSRV  APPEND ENDOF
      mvn      OF id  id WORD-MVN  APPEND ENDOF
      store    OF id  id WORD-STORE  APPEND ENDOF
      load     OF id  id WORD-LOAD  APPEND ENDOF
      reserve  OF id  id WORD-RESERVE  APPEND ENDOF
      release  OF id  id WORD-RELEASE  APPEND ENDOF
      dtake    OF id PUT-DTAKE ENDOF
      dload    OF id  id WORD-DLOAD  APPEND ENDOF
      dstore   OF id  id WORD-DSTORE  APPEND ENDOF
      dpublish OF id PUT-DPUBLISH ENDOF
      aload    OF id  id WORD-ALOAD  APPEND ENDOF
      astore   OF id  id WORD-ASTORE  APPEND ENDOF
      abload   OF id  id WORD-ABLOAD  APPEND ENDOF
      abstore  OF id  id WORD-ABSTORE  APPEND ENDOF
      flag     OF id PUT-FLAG ENDOF
      selz     OF id PUT-SELZ ENDOF
      cmpsel   OF id PUT-CMPSEL ENDOF
      br       OF id home PUT-BR ENDOF
      brz      OF id home PUT-BRZ ENDOF
      cmpbr    OF id home PUT-CMPBR ENDOF
      call     OF id PUT-CALL ENDOF
      wordcall OF id PUT-WORD-CALL ENDOF
      linksave OF id  id WORD-LNKSTR  APPEND ENDOF
      linkload OF id  id WORD-LNKLDR  APPEND ENDOF
      ret      OF id  ENC-RET  APPEND ENDOF
      fadd     OF id  id TRIPLE ENC-FADD  APPEND ENDOF
      fsub     OF id  id TRIPLE ENC-FSUB  APPEND ENDOF
      fmul     OF id  id TRIPLE ENC-FMUL  APPEND ENDOF
      fdiv     OF id  id TRIPLE ENC-FDIV  APPEND ENDOF
      fneg     OF id  id WORD-FNEG    APPEND ENDOF
      fabs     OF id  id WORD-FABS    APPEND ENDOF
      fsqrt    OF id  id WORD-FSQRT   APPEND ENDOF
      scvtf    OF id  id WORD-SCVTF   APPEND ENDOF
      fcvtzs   OF id  id WORD-FCVTZS  APPEND ENDOF
      fmovxd   OF id  id WORD-FMOVXD  APPEND ENDOF
      fmovdx   OF id  id WORD-FMOVDX  APPEND ENDOF
      fmovdd   OF id PUT-FMOVDD ENDOF
      fflag    OF id PUT-FFLAG ENDOF
      fflagz   OF id PUT-FFLAGZ ENDOF
      fcmpbr   OF id home PUT-FCMPBR ENDOF
      fcmpbrz  OF id home PUT-FCMPBRZ ENDOF
      selzd    OF id PUT-SELZD ENDOF
      cmpseld  OF id PUT-CMPSELD ENDOF
      fcmpsel   OF id PUT-FCMPSEL ENDOF
      fcmpselz  OF id PUT-FCMPSELZ ENDOF
      fcmpseld  OF id PUT-FCMPSELD ENDOF
      fcmpselzd OF id PUT-FCMPSELZD ENDOF
      tailcall  OF id PUT-TAILCALL ENDOF
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
   k O-RET = k O-BR = or k O-BRZ = or k O-CMPBR = or
   k O-FCMPBR = or k O-FCMPBRZ = or k O-TAILCALL = or ;

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

\ ---- which instructions are the routine's crossings, and which are its work ---
\ A Habu routine is entered with its arguments in the caller's data-stack cells
\ and leaves its results in them, so five of the dialect's forms are its
\ CROSSINGS rather than anything it computes: the two pointer moves the
\ convention needs, the loads that read the arguments out of those cells, the
\ stores that write the results back, and the return. Everything else is the
\ routine's work - the instructions a caller that copied this body would have to
\ write for itself.
\
\ THE SAME TWO FORMS APPEAR AT A CALL SITE, WHICH IS WHY THE CALLS ARE COUNTED
\ TOO. A site publishing its arguments and reading its results back uses the very
\ a64.dstore and a64.dload the entry and the exit use, so in a routine that CALLS
\ they are not all interface and the difference below would not be that routine's
\ body. BODY-INSNS refuses such an emission by name rather than answering a
\ number that means something else.
: IFACE-FORM? ( n -- bool )
   {: k:n :}
   k O-DTAKE = k O-DLOAD = or k O-DSTORE = or k O-DPUBLISH = or k O-RET = or ;

: CALL-FORM? ( n -- bool )
   {: k:n :}
   k O-CALL = k O-WORDCALL = or k O-TAILCALL = or ;

\ One operation written, and what it added to the two counts above. The cursor is
\ read on both sides of the writer, so an elided adjustment costs the interface
\ nothing here exactly as it costs the layout nothing there.
: PUT-COUNTED ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id home:n :}
   id SLOT-AT {: k:n :}
   N-INS @ {: was:n :}
   id home PUT-OP
   k CALL-FORM? if 1 EM-NCALL +! then
   k O-TAILCALL = if 1 EM-TAIL +! then
   k IFACE-FORM? 0= if exit then
   N-INS @ was - EM-IFACE +! ;

: WALK-BLOCK ( IR-ID:ir-block-id n -- )
   {: bk:IR-ID:ir-block-id home:n :}
   bk OP-COUNT 0 ?do
      bk i OP-AT home PUT-COUNTED
   loop ;

\ The cursor against the layout, at the start of every block and once more when
\ the last one has been written. Where a block's instructions begin is what every
\ displacement in the routine was computed from, so the writer arriving anywhere
\ else means the count the layout made and the instructions this pass emitted are
\ two different routines - which is the one failure the shared fall-through rule
\ is meant to make unreachable, and therefore the one worth stating out loud
\ rather than trusting. It costs one comparison per block and it is fail-closed:
\ nothing is sealed, so no caller can read the bytes of a routine whose layout
\ and emission disagree.
: CURSOR-CK ( n -- )
   START-AT N-INS @ <> if E-A64EMIT-LAYOUT throw then ;

\ The blocks are written in the chosen order, which is the order the layout
\ counted them in - and the cursor check below is what says so at every boundary
\ rather than leaving it to the two loops looking alike.
: WALK ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   N-BLK @ 0 ?do
      i AT-POS CURSOR-CK
      f i AT-POS BLOCK-AT  i AT-POS  WALK-BLOCK
   loop
   N-INS @ LAY-AT @ <> if E-A64EMIT-LAYOUT throw then ;

\ ---- what one emission run is told -------------------------------------------
\ The binding is taken whatever the outcome, so neither an emission without a
\ binding nor a refused emission can leave one behind for the next caller.
: BND-TAKE ( -- )
   BND-MODE @ {: have:n :}
   BOUND-NO BND-MODE !
   have BOUND-YES <> if E-A64EMIT-BIND throw then ;

\ The declared placement becomes this emission's, and the declaration is spent
\ whatever the run's outcome - so a refused emission leaves no placement for the
\ next one to measure a branch against, and a placement declared and never used
\ cannot survive into a second routine.
: PLACE-TAKE ( -- )
   PLACE-MODE @ PLACE-YES = if 1 else 0 then EM-PLACED !
   PLACE-AT-N @ EM-PLACE !
   PLACE-NO PLACE-MODE !
   0 PLACE-AT-N ! ;

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

\ What this routine destroys, decided once, at the seal, from two answers that
\ were reached different ways. The allocation's answer is every register it
\ assigned to a value (A64RAV:GPR-WRITTEN); this run's answer is every register
\ it put into a destination field. The first is what may be published, because a
\ register the emission does not happen to write today is still one an
\ instruction could name tomorrow without the allocation changing; the second is
\ what proves the first is not too narrow. An emission that wrote a register no
\ value claimed means the two disagree about what the routine is, and neither can
\ be published: E-A64EMIT-CLOBBER.
: CLOBBER-SEAL ( -- )
   A64RAV:GPR-WRITTEN A64EFF:GPRS-N {: g:n :}
   A64RAV:FPR-WRITTEN A64EFF:FPRS-N {: f:n :}
   EM-WGPR @ g invert and 0<> if E-A64EMIT-CLOBBER throw then
   EM-WFPR @ f invert and 0<> if E-A64EMIT-CLOBBER throw then
   g EM-KGPR @ or EM-CGPR !
   f EM-KFPR @ or EM-CFPR ! ;

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
   c b A64IR-OPCODE:AND     BIND1
   c b A64IR-OPCODE:ORR     BIND1
   c b A64IR-OPCODE:EOR     BIND1
   c b A64IR-OPCODE:LSLV    BIND1
   c b A64IR-OPCODE:LSRV    BIND1
   c b A64IR-OPCODE:MVN     BIND1
   c b A64IR-OPCODE:STORE   BIND1
   c b A64IR-OPCODE:LOAD    BIND1
   c b A64IR-OPCODE:RESERVE  BIND1
   c b A64IR-OPCODE:RELEASE  BIND1
   c b A64IR-OPCODE:DTAKE    BIND1
   c b A64IR-OPCODE:DLOAD    BIND1
   c b A64IR-OPCODE:DSTORE   BIND1
   c b A64IR-OPCODE:DPUBLISH BIND1
   c b A64IR-OPCODE:FLAG     BIND1
   c b A64IR-OPCODE:SELZ     BIND1
   c b A64IR-OPCODE:CMPSEL   BIND1
   c b A64IR-OPCODE:BR       BIND1
   c b A64IR-OPCODE:BRZ      BIND1
   c b A64IR-OPCODE:CMPBR    BIND1
   c b A64IR-OPCODE:RET      BIND1
   c b A64IR-OPCODE:ALOAD    BIND1
   c b A64IR-OPCODE:ASTORE   BIND1
   c b A64IR-OPCODE:ABLOAD   BIND1
   c b A64IR-OPCODE:ABSTORE  BIND1
   c b A64IR-OPCODE:CALL      BIND1
   c b A64IR-OPCODE:WORDCALL  BIND1
   c b A64IR-OPCODE:TAILCALL  BIND1
   c b A64IR-OPCODE:MADD      BIND1
   c b A64IR-OPCODE:ADDI      BIND1
   c b A64IR-OPCODE:SUBI      BIND1
   c b A64IR-OPCODE:MOVN      BIND1
   c b A64IR-OPCODE:LINKSAVE  BIND1
   c b A64IR-OPCODE:LINKLOAD  BIND1
   c b A64IR-OPCODE:FADD     BIND1
   c b A64IR-OPCODE:FSUB     BIND1
   c b A64IR-OPCODE:FMUL     BIND1
   c b A64IR-OPCODE:FDIV     BIND1
   c b A64IR-OPCODE:FNEG     BIND1
   c b A64IR-OPCODE:FABS     BIND1
   c b A64IR-OPCODE:FSQRT    BIND1
   c b A64IR-OPCODE:SCVTF    BIND1
   c b A64IR-OPCODE:FCVTZS   BIND1
   c b A64IR-OPCODE:FMOVXD   BIND1
   c b A64IR-OPCODE:FMOVDX   BIND1
   c b A64IR-OPCODE:FMOVDD   BIND1
   c b A64IR-OPCODE:FFLAG    BIND1
   c b A64IR-OPCODE:FFLAGZ   BIND1
   c b A64IR-OPCODE:FCMPBR   BIND1
   c b A64IR-OPCODE:FCMPBRZ  BIND1
   c b A64IR-OPCODE:SELZD    BIND1
   c b A64IR-OPCODE:CMPSELD  BIND1
   c b A64IR-OPCODE:FCMPSEL   BIND1
   c b A64IR-OPCODE:FCMPSELZ  BIND1
   c b A64IR-OPCODE:FCMPSELD  BIND1
   c b A64IR-OPCODE:FCMPSELZD BIND1
   c b A64IR:KEY-IMM    0 BND-IMM !
   c b A64IR:KEY-SHIFT  0 BND-SH !
   c b A64IR:KEY-SLOT   0 BND-SLOT !
   c b A64IR:KEY-FRAME  0 BND-FRAME !
   c b A64IR:KEY-DSLOT  0 BND-DSLOT !
   c b A64IR:KEY-DBYTES 0 BND-DBYTES !
   c b A64IR:KEY-COND   0 BND-COND !
   c b A64IR:KEY-DBACK  0 BND-DBACK !
   c b A64IR:KEY-ENTRY  0 BND-ENTRY !
   c b A64IR:KEY-OFF    0 BND-OFF !
   BOUND-YES BND-MODE ! ;

\ Whether a binding is live, for a caller cleaning up after a refused run. See
\ src/compiler/native/select.f BOUND? for why each pass answers for itself.
: BOUND? ( -- bool )
   BND-MODE @ BOUND-YES = ;

\ Give up a binding without emitting against it. A placement declared beside it
\ goes with it, for the same reason: it described a routine that was never
\ emitted.
: RELEASE ( -- )
   PLACE-TAKE
   BND-TAKE ;

\ ---- declaring where this routine will be written ----------------------------
\ The address the next emission's own first instruction will occupy. Only a
\ module that calls another word needs it, because only such a call is measured
\ from anywhere but the block layout; a caller that declares one for a routine
\ that turns out not to call is not refused, and the seam checks it anyway, which
\ is one more place a placement that drifted is caught rather than one fewer.
\
\ IT IS NOT THIS PASS'S NUMBER AND THIS PASS DOES NOT INVENT ONE. There is no
\ default and no fallback: an emission that needs a placement and was given none
\ is refused by name. What is checked here is only that the number could be the
\ address of an instruction at all, which is the same question A64IR asks of a
\ callee's entry - both ends of the subtraction have to be instruction addresses
\ for the displacement to be a whole number of instructions.
: PLACE-AT ( n -- )
   {: at:n :}
   PLACE-MODE @ PLACE-YES = if E-A64EMIT-PLACE throw then
   at 0 < if E-A64EMIT-PLACE throw then
   at INSN-BYTES mod 0<> if E-A64EMIT-PLACE throw then
   at PLACE-AT-N !
   PLACE-YES PLACE-MODE ! ;

\ Whether the sealed emission was made against a placement, and which one. The
\ publication seam reads both: an emission that was measured from an address is
\ only correct where that address is, so the seam holds it against the slot it is
\ about to claim and refuses the pair rather than writing the routine somewhere
\ its branches do not point.
: PLACED? ( -- bool )
   SEAL-CK EM-PLACED @ 0<> ;

: PLACEMENT ( -- n )
   SEAL-CK EM-PLACE @ ;

\ ---- the pass ----------------------------------------------------------------
\ Emit the whole of one frozen machine module, under the register assignment the
\ validator has accepted for it, into this package's buffers. Nothing is readable
\ until this returns; a run that refuses leaves no sealed emission.
\
\ THE SHAPE AND THE ORDER ARE DECIDED FIRST AND THE LAYOUT IS NOT. Whether these
\ operations are something this leaf can emit at all is a question about the
\ module alone, so SHAPE-CK is asked before any assignment is read - a module of
\ a shape this pass cannot serve is refused as that, and not as a complaint
\ about registers. Which order the blocks are written in is a question about the
\ module alone too: it reads terminators and successors and nothing else, so it
\ is asked next, and a routine too big to lay out is refused there for the same
\ reason. But how many instructions the module IS depends on the assignment,
\ because a copy whose two ends are one register is no instruction (SELF-MOV?
\ above), so the layout cannot be computed until the assignment has been
\ accepted. ALLOC-CK therefore comes after the order and before the layout: the
\ acceptance is probed, and only then are the blocks measured and written.
: EMIT ( IR-CTX:ctx IR-BUILD:module -- )
   {: c:IR-CTX:ctx m:IR-BUILD:module :}
   BND-TAKE
   PLACE-TAKE
   ST-EMPTY ST !
   0 N-INS !
   0 EM-WGPR !
   0 EM-WFPR !
   0 EM-KGPR !
   0 EM-KFPR !
   0 EM-IFACE !
   0 EM-NCALL !
   0 EM-TAIL !
   m BND-MODULE-CK
   c TARGET-CK
   m VIEWS!
   FUN-OF {: f:IR-ID:ir-fun-id :}
   f SHAPE-CK
   f ORDER-BLOCKS
   m ALLOC-CK
   f LAYOUT
   f WALK
   CLOBBER-SEAL
   ST-SEALED ST ! ;

\ ---- the sealed emission -----------------------------------------------------
: SEALED? ( -- bool )
   ST @ ST-SEALED = ;

\ What the routine this emission is destroys, one reader per register file. It
\ reads off the SEALED run for the same reason every byte the publication seam
\ writes does: an emission that refused leaves no answer here, and an answer here
\ was reached under the allocation this emission was made against rather than
\ under whichever one is live when the question is asked.
: GPR-CLOBBER ( -- A64EFF:gprs )
   SEAL-CK EM-CGPR @ A64EFF:GPR-SET ;

: FPR-CLOBBER ( -- A64EFF:fprs )
   SEAL-CK EM-CFPR @ A64EFF:FPR-SET ;

\ Does this emission LEAVE through a branch rather than through a return? Two
\ seams need the answer and neither can read it off the bytes: the publication
\ records a word's length as the span its callers may copy, which the engine
\ defines as everything before the trailing return - and a routine that has no
\ trailing return has no instruction to leave out. And the body recorder has to
\ decline such a routine, because a copied `b` would branch out of whatever
\ caller it was copied into.
: LEAVES-BY-BRANCH? ( -- bool )
   SEAL-CK EM-TAIL @ 0<> ;

\ How many instructions were emitted, and how many bytes they occupy.
: INSNS ( -- n )
   SEAL-CK N-INS @ ;

\ And how many of them are the routine's BODY: the emission less its crossings -
\ the two data-stack pointer moves, the loads that read its arguments out of the
\ caller's cells, the stores that write its results back, and the return.
\
\ WHAT IT IS FOR, AND WHY IT IS MEASURED HERE RATHER THAN DERIVED FROM AN ARITY.
\ A caller that copies this routine's body into itself writes exactly these
\ instructions and none of the crossings - src/compiler/native/inline.f carries
\ the argument - so this is what such a copy COSTS, and the size rule that
\ decides whether to make one is asked about this number. It used to be derived
\ instead: the whole emission less an interface computed from the declared arity.
\ That derivation stopped being true when the crossings stopped being a fixed
\ count - the residency pass emits no store for a cell that already holds the
\ value and no load for a value nothing reads out of one, and the placement emits
\ a pointer move only where the pointer really moves - so an arity-derived
\ interface OVERSTATES what most routines pay and therefore UNDERSTATES their
\ bodies, which is the unsound direction for a rule that admits small bodies.
\ Counting them while they are written cannot be wrong about them.
\
\ AND A ROUTINE THAT CALLS HAS NO ANSWER HERE. Its call sites publish and take
\ back through the same two forms its own crossings use, so the difference would
\ not be its body; and a routine that calls is one no caller may copy anyway, for
\ the reason inline.f gives. It is refused by name rather than answered.
: BODY-INSNS ( -- n )
   SEAL-CK
   EM-NCALL @ 0<> if E-A64EMIT-BODY throw then
   N-INS @ EM-IFACE @ - ;

\ ---- the block layout, read back ---------------------------------------------
\ How many blocks were laid out, and where each one starts. A caller that wants
\ to know whether a branch went where the layout said it would reads these and
\ the instruction at the branch's own position; a fixture asserts both.
: BLOCKS ( -- n )
   SEAL-CK N-BLK @ ;

: BLOCK-START@ ( n -- n )
   SEAL-CK BLK-ORD-CK cells B-START + @ ;

\ And which block was written at a position, which is the order this pass chose.
\ It is read back for the same reason the starts are: the order decides which
\ branches exist at all, so a fixture that wants to say a routine was written in
\ the order its blocks were built - or in some other one - has to be able to ask.
: BLOCK-AT-POS@ ( n -- n )
   SEAL-CK AT-POS ;

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

\ emit.f - turn one accepted straight-line A64IR module into the ARM64 bytes
\ that are the machine's own reading of it, and a map from every emitted byte
\ back to the source it came from.
\
\ It decides nothing but where each block's instructions land, which is what a
\ branch's displacement is measured from. Registers come only through
\ A64RAV:REG@, the one door an accepted allocation answers; encodings come only
\ from src/arch/arm64/asm.f, which bounds every field before it packs a bit.
\
\ The layout and the fixups are ONE pass: the label table IS the block-start
\ table, a block IS a label, and its ordinal is its name, so nothing is patched.
\
\ Instructions are elided by rules written once and asked twice -
\ by the layout that counts and by the writer that appends, with CURSOR-CK
\ holding the two together: a trailing branch to the block laid out next
\ (FALL-THRU?), a copy into its own register (COPY?), and a data-stack
\ adjustment of nothing, and a reload of the frame cell just stored from the
\ same register. Adjacent data-stack adjustments are combined before
\ either pass, without changing the accepted IR. An elided instruction gets NO
\ source-map row, because row k describes the instruction WORD@ k answers.
\
\ Block zero is written first and the block control leaves through last.
\ Publication explicitly distinguishes a trailing RET slot from a full span.

require lib/prelude.f
require lib/errors.f
require src/compiler/native-effect.f
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
require src/arch/arm64/backend.f
require src/arch/arm64/machine.f
require src/habu/arith-abi.f            \ E-DIV-ZERO, the divide's refusal

package A64EMIT
using A64ASM
using NFROZEN
private

\ Each pass retains its own dimensions while later passes read its results.
variable SCRATCH-VALUES
variable SCRATCH-BLOCKS
variable SCRATCH-FUNS
variable SCRATCH-OPS
: VMAX ( -- n ) SCRATCH-VALUES @ ;
: BMAX ( -- n ) SCRATCH-BLOCKS @ ;
: FMAX ( -- n ) SCRATCH-FUNS @ ;
: OMAX ( -- n ) SCRATCH-OPS @ ;
: SCRATCH-SIZES! ( -- )
   NFROZEN:VALUE-COUNT 1 max SCRATCH-VALUES !
   NFROZEN:TOTAL-BLOCKS 1 max SCRATCH-BLOCKS !
   NFROZEN:TOTAL-FUNS 1 max SCRATCH-FUNS !
   NFROZEN:TOTAL-OPS 1 max SCRATCH-OPS ! ;

\ ---- the bound dialect -------------------------------------------------------
A64IR-OPCODE:MOV       A64IR:ORD constant O-MOV
A64IR-OPCODE:DTAKE     A64IR:ORD constant O-DTAKE
A64IR-OPCODE:DLOAD     A64IR:ORD constant O-DLOAD
A64IR-OPCODE:DSTORE    A64IR:ORD constant O-DSTORE
A64IR-OPCODE:DPUBLISH  A64IR:ORD constant O-DPUBLISH
A64IR-OPCODE:DPUSH     A64IR:ORD constant O-DPUSH
A64IR-OPCODE:DPOP      A64IR:ORD constant O-DPOP
A64IR-OPCODE:FDPUSH    A64IR:ORD constant O-FDPUSH
A64IR-OPCODE:FDPOP     A64IR:ORD constant O-FDPOP
A64IR-OPCODE:FLAG      A64IR:ORD constant O-FLAG
A64IR-OPCODE:BR        A64IR:ORD constant O-BR
A64IR-OPCODE:BRZ       A64IR:ORD constant O-BRZ
A64IR-OPCODE:RET       A64IR:ORD constant O-RET
A64IR-OPCODE:SDIV      A64IR:ORD constant O-SDIV
A64IR-OPCODE:CALL      A64IR:ORD constant O-CALL
A64IR-OPCODE:CMPBR     A64IR:ORD constant O-CMPBR
A64IR-OPCODE:WORDCALL  A64IR:ORD constant O-WORDCALL
A64IR-OPCODE:FFLAG     A64IR:ORD constant O-FFLAG
A64IR-OPCODE:FFLAGZ    A64IR:ORD constant O-FFLAGZ
A64IR-OPCODE:FCMPBR    A64IR:ORD constant O-FCMPBR
A64IR-OPCODE:FCMPBRZ   A64IR:ORD constant O-FCMPBRZ
A64IR-OPCODE:FMOVDD    A64IR:ORD constant O-FMOVDD
A64IR-OPCODE:SELZ      A64IR:ORD constant O-SELZ
A64IR-OPCODE:CMPSEL    A64IR:ORD constant O-CMPSEL
A64IR-OPCODE:SELZD     A64IR:ORD constant O-SELZD
A64IR-OPCODE:CMPSELD   A64IR:ORD constant O-CMPSELD
A64IR-OPCODE:FCMPSEL   A64IR:ORD constant O-FCMPSEL
A64IR-OPCODE:FCMPSELZ  A64IR:ORD constant O-FCMPSELZ
A64IR-OPCODE:FCMPSELD  A64IR:ORD constant O-FCMPSELD
A64IR-OPCODE:FCMPSELZD A64IR:ORD constant O-FCMPSELZD
A64IR-OPCODE:TAILCALL  A64IR:ORD constant O-TAILCALL
A64IR-OPCODE:FDLOAD    A64IR:ORD constant O-FDLOAD
A64IR-OPCODE:FDSTORE   A64IR:ORD constant O-FDSTORE
A64IR-OPCODE:TRAP      A64IR:ORD constant O-TRAP
A64IR-OPCODE:FLAGI     A64IR:ORD constant O-FLAGI
A64IR-OPCODE:CMPBRI    A64IR:ORD constant O-CMPBRI
A64IR-OPCODE:RESERVE   A64IR:ORD constant O-RESERVE
A64IR-OPCODE:RELEASE   A64IR:ORD constant O-RELEASE
A64IR-OPCODE:LINKSAVE  A64IR:ORD constant O-LINKSAVE
A64IR-OPCODE:LINKLOAD  A64IR:ORD constant O-LINKLOAD
A64IR-OPCODE:DATAADDR  A64IR:ORD constant O-DATAADDR
A64IR-OPCODE:STORE     A64IR:ORD constant O-STORE
A64IR-OPCODE:LOAD      A64IR:ORD constant O-LOAD
A64IR-OPCODE:FSTORE    A64IR:ORD constant O-FSTORE
A64IR-OPCODE:FLOAD     A64IR:ORD constant O-FLOAD

0 constant BOUND-NO
1 constant BOUND-YES

\ ---- how much of one routine this pass holds ----------------------------------
\ Three per operation is the ceiling: five forms emit more than one and none
\ emits more than three.
3 constant INSN-PER-OP
: INSN-CAP ( -- n ) INSN-PER-OP OMAX BMAX 3 * + * ;

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
variable B-BASE                      \ where this function's blocks start in the module
0 B-BASE !
variable LAY-AT
0 LAY-AT !

variable EM-WGPR
0 EM-WGPR !
variable EM-WFPR
0 EM-WFPR !

\ The interface total is read off the cursor's own difference, so it is the same
\ number by construction whatever elisions apply.
variable EM-IFACE
0 EM-IFACE !
variable EM-NCALL
0 EM-NCALL !
variable EM-TAIL                     \ branches out of the routine this emission wrote
0 EM-TAIL !
variable EM-LAST                     \ the form of the last operation it wrote
\ WHETHER THIS FUNCTION'S FRAME IS ONE INSTRUCTION AT EACH END, decided HERE and
\ not in the selector. AArch64 writes the base register back as part of a load
\ or a store, and A64FRAME puts the link at the frame's base, so a frame that
\ keeps only the link is taken and given back by the transfer itself. The
\ decision waits for the emitter because the frame is not final until then: the
\ spill fixpoint raises it every time the allocator takes another slot, and a
\ frame that outgrows the nine-bit writeback field has to go back to the pair.
\ By this pass the module is the one being written, so the size it carries is
\ the size the instruction will hold.
variable EM-FUSED                    \ nonzero when this function's frame is fused
variable EM-FRAME                    \ ...and the bytes both of its ends move
-1 EM-LAST !

\ ---- where this routine will be written --------------------------------------
\ A branch to another WORD is measured from where this routine's bytes go, which
\ is the publication seam's answer, so this pass is TOLD it and never invents one.
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
A64IR:OPCODES TYPED-BUFFER BND-OP IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-IMM IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-DATA-OFF IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-ADDR IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-SH IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-SLOT IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-FRAME IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-DSLOT IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-DBYTES IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-COND IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-DBACK IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-DWB IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-ENTRY IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-TRAP IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-THROW IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-FUN IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-OFF IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-MASK IR-ID:ir-symbol-id

DYNAMIC-BUFFER CODE-BUF n
: CODE ( -- ptr u8 ) 0 CODE-BUF BYTE-VIEW ;
\ A relocation pass may not recognise an address chain by decoding region bytes,
\ so the kind travels from the elaborator and is recorded per INSTRUCTION.
DYNAMIC-BUFFER M-ADDR-BUF n
: M-ADDR ( -- ptr n ) 0 M-ADDR-BUF ;
DYNAMIC-BUFFER M-ARD-BUF n
: M-ARD ( -- ptr n ) 0 M-ARD-BUF ;
DYNAMIC-BUFFER M-OFF-BUF n
: M-OFF ( -- ptr n ) 0 M-OFF-BUF ;
DYNAMIC-BUFFER M-ST-BUF n
: M-ST ( -- ptr n ) 0 M-ST-BUF ;
DYNAMIC-BUFFER M-LN-BUF n
: M-LN ( -- ptr n ) 0 M-LN-BUF ;
DYNAMIC-BUFFER M-SRC IR-ID:ir-source-id

\ Keyed by ORDINAL, because a branch names an ordinal.
DYNAMIC-BUFFER B-START-BUF n
: B-START ( -- ptr n ) 0 B-START-BUF ;

\ One permutation held twice, because both directions are asked in an inner loop.
DYNAMIC-BUFFER B-ORDER-BUF n
: B-ORDER ( -- ptr n ) 0 B-ORDER-BUF ;
DYNAMIC-BUFFER B-PLACE-BUF n
: B-PLACE ( -- ptr n ) 0 B-PLACE-BUF ;

\ A block whose whole content is an unconditional branch is passed through, so a
\ branch to it can name the far end and the block itself becomes unreachable.
DYNAMIC-BUFFER B-GOTO-BUF n
: B-GOTO ( -- ptr n ) 0 B-GOTO-BUF ;
DYNAMIC-BUFFER B-KEEP-BUF n
: B-KEEP ( -- ptr n ) 0 B-KEEP-BUF ;
variable N-LAID                        \ how many blocks the order actually holds

\ A function's start has to outlive the per-function tables, because an
\ a64.codeaddr in an EARLIER function names a LATER one's entry. So EMIT lays
\ every function out once to fill this, then lays each out again to write it.
DYNAMIC-BUFFER F-START-BUF n
: F-START ( -- ptr n ) 0 F-START-BUF ;

\ Two signed distances per operation: before and after its other instructions.
\ Calls have both; take/publish have only the first and no other instruction.
DYNAMIC-BUFFER D-MOVES n
: DHEAD ( IR-ID:ir-op-id -- n ) IR-ID:OP-LOCAL 2 * ;
: DHEAD@ ( IR-ID:ir-op-id -- n ) DHEAD D-MOVES @ ;
: DTAIL@ ( IR-ID:ir-op-id -- n ) DHEAD 1+ D-MOVES @ ;

: RESERVE-SCRATCH ( -- )
   SCRATCH-SIZES!
   INSN-CAP INSN-BYTES * CELL 1- + CELL / CODE-BUF-RESERVE
   INSN-CAP M-ADDR-BUF-RESERVE
   INSN-CAP M-ARD-BUF-RESERVE
   INSN-CAP M-OFF-BUF-RESERVE
   INSN-CAP M-ST-BUF-RESERVE
   INSN-CAP M-LN-BUF-RESERVE
   INSN-CAP M-SRC-RESERVE
   BMAX B-START-BUF-RESERVE
   BMAX B-ORDER-BUF-RESERVE
   BMAX B-PLACE-BUF-RESERVE
   BMAX B-GOTO-BUF-RESERVE
   BMAX B-KEEP-BUF-RESERVE
   FMAX F-START-BUF-RESERVE
   OMAX 2 * D-MOVES-RESERVE
   ;
variable N-FUNS                        \ how many functions the emission holds

\ ---- the dialect's operation family ------------------------------------------
\ A form outside the family has no encoding here and is refused rather than guessed.
: OPCODE-SLOT ( IR-ID:ir-symbol-id -- n )
   {: sym:IR-ID:ir-symbol-id :}
   -1
   A64IR:OPCODES 0 ?do
      sym i BND-OP @ SAME-SYM? if drop i leave then
   loop
   dup 0 < if E-A64EMIT-OPCODE throw then ;

\ ---- reading the frozen module -----------------------------------------------
: SLOT-AT ( IR-ID:ir-op-id -- n )
   OPCODE-AT OPCODE-SLOT ;

\ ---- the registers, through the one door that answers ------------------------
\ The one checked answer in the chain, and the only way a register reaches an
\ instruction here.
: REG-OF ( IR-ID:ir-value-id -- n )
   IR-ID:VALUE-LOCAL A64RAV:REG@ ;

\ ---- the registers this emission WRITES --------------------------------------
\ Counted per FILE, because a register number names a register of ONE file - d3
\ and x3 are two registers and both are number three.
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

\ ATTR-SLOT refuses a missing key; this is the other question, asked by a reader
\ that walks operations of every form and acts on the fields it FINDS.
: ATTR-HAS? ( IR-ID:ir-op-id IR-ID:ir-symbol-id -- bool )
   {: id:IR-ID:ir-op-id want:IR-ID:ir-symbol-id :}
   false
   id ATTRS-OF 0 ?do
      id i ATTR-KEY-AT want SAME-SYM? if drop true leave then
   loop ;

: IMM-OF ( IR-ID:ir-op-id -- n )
   0 BND-IMM @ ATTR-INT ;

: ADDR-OF ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id SLOT-AT O-DATAADDR = if A64IR:ADDR-DATA exit then
   id 0 BND-ADDR @ ATTR-HAS? 0= if A64IR:ADDR-NONE exit then
   id 0 BND-ADDR @ ATTR-INT ;

\ The dialect records a number of BITS and the encoding holds the half it
\ selects, so SCALE/ refuses a shift that names no whole half.
: HALF-OF ( IR-ID:ir-op-id -- n )
   0 BND-SH @ ATTR-INT A64IR:HALF-BITS SCALE/ ;

\ ---- the frame operands ------------------------------------------------------
: SLOT-OFF ( IR-ID:ir-op-id -- n )
   0 BND-SLOT @ ATTR-INT ;

: FRAME-SIZE ( IR-ID:ir-op-id -- n )
   0 BND-FRAME @ ATTR-INT ;

: OFF-IMM ( IR-ID:ir-op-id -- n )
   0 BND-OFF @ ATTR-INT ;

\ The dialect holds the MASK and the encoders take its thirteen-bit description,
\ so the packer converts here; it cannot refuse, because the dialect's bound is
\ that same packer.
: MASK-IMM ( IR-ID:ir-op-id -- n )
   0 BND-MASK @ ATTR-INT >LIMM ;

\ ---- the data-stack operands -------------------------------------------------
: DSLOT-OFF ( IR-ID:ir-op-id -- n )
   0 BND-DSLOT @ ATTR-INT ;

: DBYTES-SIZE ( IR-ID:ir-op-id -- n )
   0 BND-DBYTES @ ATTR-INT ;

: DBACK-SIZE ( IR-ID:ir-op-id -- n )
   0 BND-DBACK @ ATTR-INT ;

: DWB-SIZE ( IR-ID:ir-op-id -- n )
   0 BND-DWB @ ATTR-INT ;

\ The callee's own entry and not a displacement, which is why the placement has
\ to be known here.
: ENTRY-ADDR ( IR-ID:ir-op-id -- n )
   0 BND-ENTRY @ ATTR-INT ;

: FUN-OF ( IR-ID:ir-op-id -- n )
   0 BND-FUN @ ATTR-INT ;

\ Under a key of its own, because two passes recognise a tail branch by the
\ presence of `a64.entry` and a trap is not one.
: TRAP-ADDR ( IR-ID:ir-op-id -- n )
   0 BND-TRAP @ ATTR-INT ;

\ The runtime's `throw`, which the divide's cold side hands the refusal to.
: THROW-ADDR ( IR-ID:ir-op-id -- n )
   0 BND-THROW @ ATTR-INT ;

\ ---- one instruction per operation -------------------------------------------
: WORD-MOVZ ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id 0 RESULT-REG  id IMM-OF  id HALF-OF  MOVZHW ;

: WORD-MOVK ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id 0 RESULT-REG  id IMM-OF  id HALF-OF  MOVKHW ;

\ A MOVN MAY NOT CARRY AN ADDRESS: the relocation pass rewrites a chain by
\ writing four plain immediates, and a movn builds its value out of ONES.
: WORD-MOVN ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id ADDR-OF A64IR:ADDR-NONE <> if E-A64EMIT-ADDR throw then
   id 0 RESULT-REG  id IMM-OF  id HALF-OF  MOVNHW ;

: WORD-MOV ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id 0 RESULT-REG  id 0 OPERAND-REG  ENC-MOV ;

: WORD-MVN ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id 0 RESULT-REG  id 0 OPERAND-REG  ENC-MVN ;

: TRIPLE ( IR-ID:ir-op-id -- n n n )
   {: id:IR-ID:ir-op-id :}
   id 0 RESULT-REG  id 0 OPERAND-REG  id 1 OPERAND-REG ;

: PAIRI ( IR-ID:ir-op-id -- n n n )
   {: id:IR-ID:ir-op-id :}
   id 0 RESULT-REG  id 0 OPERAND-REG  id OFF-IMM ;

: PAIRM ( IR-ID:ir-op-id -- n n n )
   {: id:IR-ID:ir-op-id :}
   id 0 RESULT-REG  id 0 OPERAND-REG  id MASK-IMM ;

\ `madd rd, rn, rm, xzr` and `mul rd, rn, rm` are the SAME four bytes, so an
\ addend arriving as register 31 would silently emit a multiply.
31 constant ZERO-REG

: ?ADDEND ( n -- n )
   dup ZERO-REG = if E-A64EMIT-ADDEND throw then ;

\ In the order `madd rd, rn, rm, ra` names them, which is the order the schema
\ declares its operands in.
: QUAD ( IR-ID:ir-op-id -- n n n n )
   {: id:IR-ID:ir-op-id :}
   id 0 RESULT-REG  id 0 OPERAND-REG  id 1 OPERAND-REG  id 2 OPERAND-REG ?ADDEND ;

: WORD-STORE ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id 0 OPERAND-REG  A64M:SP-GPR  id SLOT-OFF  ENC-STR ;

: WORD-LOAD ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id 0 RESULT-REG  A64M:SP-GPR  id SLOT-OFF  ENC-LDR ;

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

: WORD-FMOVDD ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id 0 RESULT-REG  id 0 OPERAND-REG  ENC-FMOVDD ;

: WORD-RESERVE ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   A64M:SP-GPR A64M:SP-GPR  id FRAME-SIZE  ENC-SUBI ;

: WORD-RELEASE ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   A64M:SP-GPR A64M:SP-GPR  id FRAME-SIZE  ENC-ADDI ;

\ Over the pointer is the scaled unsigned field, Ldr and Str; under it is the
\ unscaled signed field, Ldur and Stur. One dialect form written two ways.
: DENC-LDR ( n n n -- n )
   dup 0 < if ENC-LDUR exit then ENC-LDR ;

: DENC-STR ( n n n -- n )
   dup 0 < if ENC-STUR exit then ENC-STR ;

: WORD-DLOAD ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id 0 RESULT-REG  A64M:DSTACK-GPR  id DSLOT-OFF  DENC-LDR ;

: WORD-DSTORE ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id 0 OPERAND-REG  A64M:DSTACK-GPR  id DSLOT-OFF  DENC-STR ;

\ The two fused forms, which is where the pointer move went. Post-index stores
\ AT the pointer and then moves it forward; pre-index moves it back and reads
\ THERE - so the store takes the move and the load takes its negative, and
\ neither has an offset to read because the form has no field for one.
: WORD-DPUSH ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id 0 OPERAND-REG  A64M:DSTACK-GPR  id DWB-SIZE  ENC-STRPOST ;

: WORD-DPOP ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id 0 RESULT-REG  A64M:DSTACK-GPR  id DWB-SIZE negate  ENC-LDRPRE ;

\ Each is its general twin with one encoder swapped, so a double reaches memory
\ and leaves it in the file it lives in.
: DENC-LDRD ( n n n -- n )
   dup 0 < if ENC-LDURD exit then ENC-LDRD ;

: DENC-STRD ( n n n -- n )
   dup 0 < if ENC-STURD exit then ENC-STRD ;

: WORD-FSTORE ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id 0 OPERAND-REG  A64M:SP-GPR  id SLOT-OFF  ENC-STRD ;

: WORD-FLOAD ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id 0 RESULT-REG  A64M:SP-GPR  id SLOT-OFF  ENC-LDRD ;

: WORD-FDLOAD ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id 0 RESULT-REG  A64M:DSTACK-GPR  id DSLOT-OFF  DENC-LDRD ;

: WORD-FDSTORE ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id 0 OPERAND-REG  A64M:DSTACK-GPR  id DSLOT-OFF  DENC-STRD ;

\ The same two fused forms in the other register file, which is one opcode bit.
: WORD-FDPUSH ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id 0 OPERAND-REG  A64M:DSTACK-GPR  id DWB-SIZE  ENC-STRDPOST ;

: WORD-FDPOP ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id 0 RESULT-REG  A64M:DSTACK-GPR  id DWB-SIZE negate  ENC-LDRDPRE ;

\ ---- the two addressed forms -------------------------------------------------
\ Offset zero is `[Xn]`, written here because this dialect has no addressing
\ mode with an offset for an operation to carry one in.
0 constant ADDR-OFF

: WORD-ALOAD ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id 0 RESULT-REG  id 0 OPERAND-REG  ADDR-OFF  ENC-LDR ;

: WORD-ASTORE ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id 0 OPERAND-REG  id 1 OPERAND-REG  ADDR-OFF  ENC-STR ;

: WORD-FALOAD ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id 0 RESULT-REG  id 0 OPERAND-REG  ADDR-OFF  ENC-LDRD ;

: WORD-FASTORE ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id 0 OPERAND-REG  id 1 OPERAND-REG  ADDR-OFF  ENC-STRD ;

\ The width is which ENCODER is called: ENC-LDR would read eight bytes where the
\ program asked for one.
: WORD-ABLOAD ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id 0 RESULT-REG  id 0 OPERAND-REG  ADDR-OFF  ENC-LDRB ;

: WORD-ABSTORE ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id 0 OPERAND-REG  id 1 OPERAND-REG  ADDR-OFF  ENC-STRB ;

\ ---- the caller's return address ---------------------------------------------
\ x30 is asked for by name rather than written here, from the one place that
\ says why no routine may hold state in it.
: WORD-LNKSTR ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   A64M:LINK-GPR  A64M:SP-GPR  id SLOT-OFF  ENC-STR ;

: WORD-LNKLDR ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   A64M:LINK-GPR  A64M:SP-GPR  id SLOT-OFF  ENC-LDR ;

\ The same two transfers with the frame move folded in. The offset is the whole
\ frame and the transfer lands at its base, which is where A64FRAME puts the
\ link, so one writeback does what the pair did. Neither reads the frame off
\ the operation: the load end is a linkload, which carries the slot and not the
\ size, so both take the size this function's shape was decided from.
: WORD-LNKPUSH ( -- n )
   A64M:LINK-GPR  A64M:SP-GPR  EM-FRAME @ negate  ENC-STRPRE ;

: WORD-LNKPOP ( -- n )
   A64M:LINK-GPR  A64M:SP-GPR  EM-FRAME @  ENC-LDRPOST ;

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

\ The offset is the cursor at the moment the instruction was appended, not four
\ times its index, so a run that emitted one too few is visible in the map.
: APPEND ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id w:n :}
   N-INS @ INSN-CAP >= if E-A64EMIT-CAP throw then
   N-INS @ {: k:n :}
   k INSN-BYTES * {: off:n :}
   w off WORD!
   id off k MAP!
   id ADDR-OF {: kind:n :}
   kind k cells M-ADDR + !
   kind A64IR:ADDR-NONE = if -1 else id 0 RESULT-REG then  k cells M-ARD + !
   k 1+ N-INS ! ;

\ ---- the block layout --------------------------------------------------------
\ An ordinal outside the function's blocks is a module this layout cannot serve.
: BLK-ORD-CK ( n -- n )
   dup 0 < over N-BLK @ >= or if E-A64EMIT-BLOCK throw then ;

\ A successor carries a block's ordinal in the MODULE and these tables are keyed
\ by its ordinal in the FUNCTION, so the function's base comes off it here.
: SUCC-BLOCK ( IR-ID:ir-op-id n -- n )
   SUCC-AT IR-ID:BLOCK-LOCAL  B-BASE @ -  BLK-ORD-CK ;

\ A function's blocks are contiguous, which the subtraction above rests on, so
\ it is measured while it is filed rather than assumed.
: B-BASE! ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   f 0 BLOCK-AT IR-ID:BLOCK-LOCAL B-BASE !
   f BLOCK-COUNT 0 ?do
      f i BLOCK-AT IR-ID:BLOCK-LOCAL  B-BASE @ -  i <>
      if E-A64EMIT-SHAPE throw then
   loop ;

\ The division is the guard, the three-instruction refusal on its cold side and
\ the divide (PUT-SDIV). How long the form is and how far the guard jumps are
\ ONE number, so a refusal that grows cannot leave the guard landing inside it.
5 constant DIV-INSNS                 \ instructions one division is
DIV-INSNS 1 -  constant DIV-SKIP     \ words from the guard to the divide

\ A property of the FORM: one for all but the three comparisons, the division,
\ the two calls and the three compare-and-branches, and the two-way branch and
\ the eight conditional selects, which are two.
: INSNS-OF ( n -- n )
   {: k:n :}
   k O-DATAADDR = if 3 exit then
   k O-SELZ = if 2 exit then
   k O-CMPSEL = if 2 exit then
   k O-SELZD = if 2 exit then
   k O-CMPSELD = if 2 exit then
   k O-FCMPSEL = if 2 exit then
   k O-FCMPSELZ = if 2 exit then
   k O-FCMPSELD = if 2 exit then
   k O-FCMPSELZD = if 2 exit then
   k O-FLAG = if 3 exit then
   k O-FLAGI = if 3 exit then
   k O-FFLAG = if 3 exit then
   k O-FFLAGZ = if 3 exit then
   k O-SDIV = if DIV-INSNS exit then
   k O-CALL = if 3 exit then
   k O-WORDCALL = if 3 exit then
   k O-CMPBR = if 3 exit then
   k O-CMPBRI = if 3 exit then
   k O-FCMPBR = if 3 exit then
   k O-FCMPBRZ = if 3 exit then
   k O-BRZ = if 2 exit then
   k O-TRAP = if 2 exit then
   1 ;

\ Which successor the trailing unconditional branch names, and -1 for a form
\ that ends in no such branch.
: TAIL-SUCC ( n -- n )
   {: k:n :}
   k O-BR = if 0 exit then
   k O-BRZ = if 1 exit then
   k O-CMPBR = if 1 exit then
   k O-CMPBRI = if 1 exit then
   k O-FCMPBR = if 1 exit then
   k O-FCMPBRZ = if 1 exit then
   -1 ;

\ ---- the chosen block order --------------------------------------------------

\ The order is a permutation of the block ordinals, so one bound serves both.
: AT-POS ( n -- n )
   BLK-ORD-CK cells B-ORDER + @ ;

: POS-OF ( n -- n )
   BLK-ORD-CK cells B-PLACE + @ ;

: LAID? ( n -- bool )
   BLK-ORD-CK cells B-PLACE + @ 0 >= ;

: LAY ( n n -- )
   {: b:n p:n :}
   b BLK-ORD-CK {: bb:n :}
   p BLK-ORD-CK {: pp:n :}
   bb  pp cells B-ORDER + !
   pp  bb cells B-PLACE + ! ;

\ Read after GOTO! has run, so it holds the far end of however long a chain of
\ pass-through blocks stood in the way.
: GOTO-OF ( n -- n )
   BLK-ORD-CK cells B-GOTO + @ ;

: KEPT? ( n -- bool )
   BLK-ORD-CK cells B-KEEP + @ 0<> ;

: TAIL-BLOCK ( IR-ID:ir-block-id -- n )
   TERM-AT {: t:IR-ID:ir-op-id :}
   t SLOT-AT TAIL-SUCC {: s:n :}
   s 0 < if -1 exit then
   t s SUCC-BLOCK GOTO-OF ;

: NEXT-UNLAID ( -- n )
   0 begin dup N-BLK @ < while
      dup KEPT? over LAID? 0= and if exit then
      1+
   repeat
   drop -1 ;

: CAN-INVERT? ( IR-ID:ir-op-id -- bool )
   {: id:IR-ID:ir-op-id :}
   id SLOT-AT TAIL-SUCC 1 <> if false exit then
   id SLOT-AT O-BRZ = if true exit then
   \ AL/NV do not have complementary conditions.
   id COND-OF 14 < ;

: FOLLOWER ( IR-ID:ir-fun-id n -- n )
   {: f:IR-ID:ir-fun-id b:n :}
   f b BLOCK-AT TAIL-BLOCK {: s:n :}
   s 0 >= if s LAID? 0= if s exit then then
   \ If the preferred arm is already placed (including a cold tail), trace
   \ the other arm before starting an unrelated block's trace.
   f b BLOCK-AT TERM-AT {: t:IR-ID:ir-op-id :}
   t CAN-INVERT? if
      t 0 SUCC-BLOCK GOTO-OF {: other:n :}
      other LAID? 0= if other exit then
   then
   NEXT-UNLAID ;

\ A routine every path of which traps has no return for the emission to end on,
\ so no block is pinned last and the trace decides the whole order.
-1 constant NO-RET

: RET-ORD ( IR-ID:ir-fun-id -- n )
   {: f:IR-ID:ir-fun-id :}
   NO-RET
   f BLOCK-COUNT 0 ?do
      f i BLOCK-AT TERM-AT {: t:IR-ID:ir-op-id :}
      t SUCCS-OF 0=  t SLOT-AT O-TRAP <>  and if
         dup NO-RET <> if E-A64EMIT-SHAPE throw then
         drop i
      then
   loop ;

\ Asked in POSITIONS and answered about ORDINALS, so nothing here depends on
\ where any block starts - which is what lets the layout ask it.
: SWAP-SUCCS? ( IR-ID:ir-op-id n -- bool )
   {: id:IR-ID:ir-op-id home:n :}
   id CAN-INVERT? 0= if false exit then
   id 0 SUCC-BLOCK GOTO-OF POS-OF home POS-OF 1+ = ;

: FALL-THRU? ( IR-ID:ir-op-id n -- bool )
   {: id:IR-ID:ir-op-id home:n :}
   id home SWAP-SUCCS? if true exit then
   id SLOT-AT TAIL-SUCC {: s:n :}
   s 0 < if false exit then
   id s SUCC-BLOCK GOTO-OF POS-OF  home POS-OF 1+ = ;

\ The rule is register equality alone. The two files are separately numbered,
\ which is sound because a copy's ends are one class and the allocator refuses a
\ class spanning the two by name.
: COPY? ( IR-ID:ir-op-id -- bool )
   {: id:IR-ID:ir-op-id :}
   id SLOT-AT {: k:n :}
   k O-MOV =  k O-FMOVDD =  or ;

: SELF-MOV? ( IR-ID:ir-op-id -- bool )
   {: id:IR-ID:ir-op-id :}
   id COPY? 0= if false exit then
   id 0 RESULT-REG  id 0 OPERAND-REG  = ;

\ Only adjacent moves within one block combine. Every other operation is a
\ barrier, including memory transfers, calls and branches. Keep both moves if
\ their sum does not fit one immediate; optimization must not add a refusal.
: D-MERGE ( n n -- n )
   {: prev:n slot:n :}
   slot D-MOVES @ 0= if prev exit then
   prev 0 >= if
      prev D-MOVES @ slot D-MOVES @ + {: sum:n :}
      sum abs A64IR:OFF-LIMIT <= if
         0 prev D-MOVES !
         sum slot D-MOVES !
      then
   then
   slot ;

: PLAN-DOP ( n IR-ID:ir-op-id -- n )
   {: prev:n id:IR-ID:ir-op-id :}
   id SLOT-AT {: k:n :}
   id DHEAD {: slot:n :}
   0 slot D-MOVES !  0 slot 1+ D-MOVES !
   id 0 BND-DBYTES @ ATTR-HAS? if
      id DBYTES-SIZE
      k O-DTAKE = if negate then
      slot D-MOVES !
   then
   id 0 BND-DBACK @ ATTR-HAS? if
      id DBACK-SIZE negate slot 1+ D-MOVES !
   then
   prev slot D-MERGE
   k O-DTAKE = k O-DPUBLISH = or if exit then
   drop -1 slot 1+ D-MERGE ;

: DZERO-MOVES ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   0
   id 0 BND-DBYTES @ ATTR-HAS? if id DHEAD@ 0= if 1+ then then
   id 0 BND-DBACK @ ATTR-HAS? if id DTAIL@ 0= if 1+ then then ;

\ The selector's prologue is a reserve opening the entry block with the link
\ save right behind it; nothing else produces that pair. A reserve the spill
\ pass inserted for a routine that only spills has no link save after it and
\ stays two plain pointer moves.
: FRAME-SHAPE! ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   0 EM-FUSED !  0 EM-FRAME !
   f 0 BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk OP-COUNT 2 < if exit then
   bk 0 OP-AT {: r:IR-ID:ir-op-id :}
   r SLOT-AT O-RESERVE <> if exit then
   bk 1 OP-AT SLOT-AT O-LINKSAVE <> if exit then
   r FRAME-SIZE {: size:n :}
   size A64FRAME:FUSED? 0= if exit then
   size EM-FRAME !  1 EM-FUSED ! ;

\ The second half of a fused end writes nothing: its work is in the first.
: FUSED-SILENT? ( IR-ID:ir-op-id -- bool )
   {: id:IR-ID:ir-op-id :}
   EM-FUSED @ 0= if false exit then
   id SLOT-AT {: k:n :}
   k O-LINKSAVE = k O-RELEASE = or ;

: OP-INSNS ( IR-ID:ir-op-id n -- n )
   {: id:IR-ID:ir-op-id home:n :}
   id SLOT-AT INSNS-OF
   id home FALL-THRU? if 1- then
   id SELF-MOV? if 1- then
   id FUSED-SILENT? if 1- then
   id DZERO-MOVES - ;

\ These frame forms move eight bytes at SP without writeback. Exact operation
\ adjacency within one block leaves the stored register and cell unchanged,
\ with no entry between them. Keep the store for later readers; only the reload
\ disappears. Pairing opcodes keeps the general and floating files distinct.
\ Read the result register without recording a write while deciding eligibility.
: STORED-RELOAD? ( IR-ID:ir-block-id n -- bool )
   {: bk:IR-ID:ir-block-id at:n :}
   at 0= if false exit then
   bk at OP-AT {: id:IR-ID:ir-op-id :}
   id SLOT-AT {: k:n :}
   k O-LOAD = k O-FLOAD = or 0= if false exit then
   bk at 1- OP-AT {: prev:IR-ID:ir-op-id :}
   k O-LOAD = if
      prev SLOT-AT O-STORE <> if false exit then
   else
      prev SLOT-AT O-FSTORE <> if false exit then
   then
   id SLOT-OFF prev SLOT-OFF <> if false exit then
   id 0 RESULT-AT REG-OF prev 0 OPERAND-REG = ;

: BLOCK-INSNS ( IR-ID:ir-block-id n -- n )
   {: bk:IR-ID:ir-block-id home:n :}
   0
   bk OP-COUNT 0 ?do
      bk i STORED-RELOAD? 0= if bk i OP-AT home OP-INSNS + then
   loop ;

: START-AT ( n -- n )
   BLK-ORD-CK
   dup LAID? 0= if E-A64EMIT-BLOCK throw then
   cells B-START + @ ;

\ ---- the blocks control only passes through ----------------------------------
\ Two operations can be present and emit nothing, which is a fact about the
\ register assignment - so the ORDER is chosen after the acceptance is probed.
: OP-SILENT? ( IR-ID:ir-op-id -- bool )
   {: id:IR-ID:ir-op-id :}
   id SLOT-AT INSNS-OF
   id SELF-MOV? if 1- then
   id FUSED-SILENT? if 1- then
   id DZERO-MOVES -
   0= ;

: SILENT-BEFORE-TERM? ( IR-ID:ir-block-id -- bool )
   {: bk:IR-ID:ir-block-id :}
   0
   bk OP-COUNT 1- 0 ?do
      bk i OP-AT OP-SILENT? 0= if 1+ then
   loop
   0= ;

: PASS-THRU? ( IR-ID:ir-fun-id n -- bool )
   {: f:IR-ID:ir-fun-id b:n :}
   b 0= if false exit then
   f b BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk TERM-AT {: t:IR-ID:ir-op-id :}
   t SLOT-AT O-BR <> if false exit then
   bk SILENT-BEFORE-TERM? ;

variable CH-AT

: CHASE-STEP ( IR-ID:ir-fun-id -- bool )
   {: f:IR-ID:ir-fun-id :}
   f CH-AT @ PASS-THRU? 0= if false exit then
   f CH-AT @ BLOCK-AT TERM-AT 0 SUCC-BLOCK {: nxt:n :}
   nxt CH-AT @ = if false exit then
   nxt CH-AT !
   true ;

\ A chain of pass-through blocks that closed into a loop would be walked for
\ ever, so the walk is bounded by the number of blocks there are.
: CHASE ( IR-ID:ir-fun-id n -- n )
   {: f:IR-ID:ir-fun-id b:n :}
   b CH-AT !
   N-BLK @ 0 ?do
      f CHASE-STEP 0= if leave then
   loop
   CH-AT @ ;

: GOTO! ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   N-BLK @ 0 ?do  f i CHASE  i cells B-GOTO + !  loop ;

\ ---- which blocks are still reached ------------------------------------------
: KEEP1 ( n -- n )
   {: s:n :}
   s KEPT? if 0 exit then
   1 s cells B-KEEP + !
   1 ;

: KEEP-SUCCS ( IR-ID:ir-fun-id n -- n )
   {: f:IR-ID:ir-fun-id b:n :}
   f b BLOCK-AT TERM-AT {: t:IR-ID:ir-op-id :}
   0
   t SUCCS-OF 0 ?do
      t i SUCC-BLOCK GOTO-OF KEEP1 +
   loop ;

: KEEP-SWEEP ( IR-ID:ir-fun-id -- n )
   {: f:IR-ID:ir-fun-id :}
   0
   N-BLK @ 0 ?do
      i KEPT? if f i KEEP-SUCCS + then
   loop ;

: KEEP! ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   N-BLK @ 0 ?do  0 i cells B-KEEP + !  loop
   1 0 cells B-KEEP + !
   begin  f KEEP-SWEEP 0=  until ;

\ Taken rather than assumed, because the trace fills exactly this many positions.
: KEPT-COUNT ( -- n )
   0
   N-BLK @ 0 ?do  i KEPT? if 1+ then  loop ;

: ORDER-NO-RET ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id k:n :}
   0 0 LAY
   k 1 ?do
      f  i 1- AT-POS  FOLLOWER  i LAY
   loop ;

\ B-KEEP is 0 for unreachable, 1 for normal, 2 for a path ending only in traps.
\ Include a diagnostic's dispatch blocks: leaving those before the return would
\ still put error handling on the successful path. Cycles remain conservative.
: COLD? ( n -- bool ) cells B-KEEP + @ 2 = ;

: COLD-TERM? ( IR-ID:ir-fun-id n -- bool )
   BLOCK-AT TERM-AT {: t:IR-ID:ir-op-id :}
   t SLOT-AT O-TRAP = if true exit then
   t SUCCS-OF 0= if false exit then
   t SUCCS-OF 0 ?do
      t i SUCC-BLOCK GOTO-OF COLD? 0= if false unloop exit then
   loop
   true ;

: MARK-COLD ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   begin
      0
      N-BLK @ 0 ?do
         N-BLK @ 1- i - {: b:n :}
         b KEPT? b COLD? 0= and if
            f b COLD-TERM? if 2 b cells B-KEEP + ! 1+ then
         then
      loop
      0=
   until ;

\ Pre-place cold blocks after the successful return. FOLLOWER traces only the
\ remaining normal blocks; descending ordinals preserve the cold source order.
: LAY-COLD ( IR-ID:ir-fun-id n -- n )
   {: f:IR-ID:ir-fun-id last:n :}
   last
   f BLOCK-COUNT 0 ?do
      f BLOCK-COUNT 1- i - {: b:n :}
      b COLD? if b over LAY 1- then
   loop ;

: ORDER-BLOCKS ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   f BLOCK-COUNT {: n:n :}
   n 1 < if E-A64EMIT-SHAPE throw then
   n BMAX > if E-A64EMIT-CAP throw then
   n N-BLK !
   f B-BASE!
   f GOTO!
   f KEEP!
   KEPT-COUNT {: k:n :}
   k N-LAID !
   f RET-ORD {: r:n :}
   n 0 ?do  -1 i cells B-PLACE + !  loop
   r NO-RET = if f k ORDER-NO-RET exit then
   r KEPT? 0= if E-A64EMIT-SHAPE throw then
   f MARK-COLD
   f k 1- LAY-COLD {: last:n :}
   r last LAY
   k 1 = if exit then
   r 0= if E-A64EMIT-SHAPE throw then
   0 0 LAY
   last 1 ?do
      f  i 1- AT-POS  FOLLOWER  i LAY
   loop ;

\ Counted from the start of the EMISSION and not of the function, because a
\ displacement subtracts two of these and both ends must share an origin.
: LAYOUT ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id base:n :}
   base LAY-AT !
   N-LAID @ 0 ?do
      LAY-AT @ i AT-POS cells B-START + !
      LAY-AT @  f i AT-POS BLOCK-AT  i AT-POS BLOCK-INSNS  +  LAY-AT !
   loop ;

\ ---- the branches ------------------------------------------------------------
: DELTA ( n -- n )
   START-AT N-INS @ - ;

\ Both encoders MASK their displacement field rather than bounding it, so a
\ branch out of reach would silently become a branch somewhere else.
: B-WORD ( n -- n )
   {: d:n :}
   d A64IR:B-FITS? 0= if E-A64EMIT-REACH throw then
   d ENC-B ;

: BZ-WORD ( n n -- n )
   {: rt:n d:n :}
   d A64IR:BZ-FITS? 0= if E-A64EMIT-REACH throw then
   rt d ENC-CBZ ;

: BCOND-WORD ( n n -- n )
   {: d:n k:n :}
   d A64IR:BCOND-FITS? 0= if E-A64EMIT-REACH throw then
   d k ENC-BCOND ;

\ The arguments are already in the destination's registers by the allocation's
\ own decision, so they reach no encoder and the whole instruction is the jump.
: PUT-BR ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id home:n :}
   id home FALL-THRU? if exit then
   id  id 0 SUCC-BLOCK GOTO-OF DELTA B-WORD  APPEND ;

: PUT-BRZ ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id home:n :}
   id  id 0 OPERAND-REG
   id id home SWAP-SUCCS? if 1 else 0 then SUCC-BLOCK GOTO-OF DELTA BZ-WORD
   id home SWAP-SUCCS? if $01000000 xor then APPEND
   id home FALL-THRU? if exit then
   id  id 1 SUCC-BLOCK GOTO-OF DELTA B-WORD  APPEND ;

\ Invert the condition, not the comparison's operands: this also preserves
\ the unordered floating-point edge when the opposite successor falls through.
: PUT-COND-BR ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id home:n :}
   id  id id home SWAP-SUCCS? if 1 else 0 then SUCC-BLOCK GOTO-OF DELTA
   id COND-OF id home SWAP-SUCCS? if 1 xor then BCOND-WORD APPEND
   id home FALL-THRU? if exit then
   id  id 1 SUCC-BLOCK GOTO-OF DELTA B-WORD APPEND ;

\ The comparison writes only the flags and the branch beside it reads them
\ there, so no register is written. The conditional's displacement is measured
\ after the compare is appended, because it is counted from its own instruction.
: PUT-CMPBR ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id home:n :}
   id  id 0 OPERAND-REG id 1 OPERAND-REG ENC-CMP  APPEND
   id home PUT-COND-BR ;

: PUT-CMPBRI ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id home:n :}
   id  id 0 OPERAND-REG id OFF-IMM ENC-CMPI  APPEND
   id home PUT-COND-BR ;

\ The Fcmp raises the unordered condition for a NaN and the conditions selection
\ names are all false under it, so control reaches the SECOND successor.
: PUT-FCMPBR ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id home:n :}
   id  id 0 OPERAND-REG id 1 OPERAND-REG ENC-FCMP  APPEND
   id home PUT-COND-BR ;

: PUT-FCMPBRZ ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id home:n :}
   id  id 0 OPERAND-REG ENC-FCMP0  APPEND
   id home PUT-COND-BR ;

\ Compare, set one on the condition, negate - because a Habu flag is all bits
\ set. It is the sequence the engine's own emitter uses.
: PUT-FLAG ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id 0 RESULT-REG {: rd:n :}
   id  id 0 OPERAND-REG id 1 OPERAND-REG ENC-CMP  APPEND
   id  rd id COND-OF ENC-CSET  APPEND
   id  rd rd ENC-NEG  APPEND ;

: PUT-FLAGI ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id 0 RESULT-REG {: rd:n :}
   id  id 0 OPERAND-REG id OFF-IMM ENC-CMPI  APPEND
   id  rd id COND-OF ENC-CSET  APPEND
   id  rd rd ENC-NEG  APPEND ;

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

\ The FIRST source is the condition-holds answer, which is the order a Csel
\ reads and the order a64.cmpbr puts its successors in.
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

\ The two zero forms read their condition off the OPERATION, because which
\ relation is asked - `f0<` or `f0=` - is not always `ne`.
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

\ ---- moving the data-stack pointer -------------------------------------------
\ NO INSTRUCTION AT ALL when the distance is zero, which is the ordinary case.
: PUT-DMOVE ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id d:n :}
   d 0= if exit then
   d 0 > if
      id  A64M:DSTACK-GPR A64M:DSTACK-GPR d  ENC-ADDI  APPEND
      exit
   then
   id  A64M:DSTACK-GPR A64M:DSTACK-GPR d negate  ENC-SUBI  APPEND ;

: PUT-DTAKE ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id  id DHEAD@  PUT-DMOVE ;

: PUT-DPUBLISH ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id  id DHEAD@  PUT-DMOVE ;

\ A self-call is RECURSE, which names the DEFINITION and not the body the token
\ stands in - so it goes to function zero of this emission, which is the
\ definition, wherever it is staged. Inside function zero that is the block the
\ caller entered at; inside a quotation's function it is another function
\ entirely, and a branch to this one's own block zero would be the quotation
\ calling itself.
0 constant SELF-FUN                  \ the definition, which every self-call goes to

\ ---- the address of another function of this emission ------------------------
\ Known because MEASURE laid every function out and filed its start before a
\ byte was written; the ordinal is held against what the emission really holds.
: FUN-START ( n -- n )
   {: k:n :}
   k 0 < k N-FUNS @ >= or if E-A64EMIT-SHAPE throw then
   k cells F-START + @ ;

: BL-WORD ( n -- n )
   {: d:n :}
   d A64IR:B-FITS? 0= if E-A64EMIT-REACH throw then
   d ENC-BL ;

: PUT-CALL ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id  id DHEAD@  PUT-DMOVE
   id  SELF-FUN FUN-START  N-INS @ -  BL-WORD  APPEND
   id  id DTAIL@  PUT-DMOVE ;

\ ---- calling another word ----------------------------------------------------
\ Both ends are instruction aligned by construction, so the subtraction is a
\ whole number of instructions.
: PLACEMENT-CK ( -- n )
   EM-PLACED @ 0= if E-A64EMIT-PLACE throw then
   EM-PLACE @ ;

: WORD-DELTA ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id ENTRY-ADDR  PLACEMENT-CK -  INSN-BYTES /  N-INS @ - ;

: PUT-WORD-CALL ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id  id DHEAD@  PUT-DMOVE
   id  id WORD-DELTA BL-WORD  APPEND
   id  id DTAIL@  PUT-DMOVE ;

\ In BYTES, which is the unit this field counts - the branch fields count
\ instructions.
: ADR-DELTA ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id FUN-OF FUN-START  N-INS @ -  INSN-BYTES * ;

: ADR-WORD ( n n -- n )
   {: rd:n d:n :}
   d A64IR:ADR-FITS? 0= if E-A64EMIT-REACH throw then
   rd d ENC-ADR ;

\ Both ends of this delta are inside ONE emission, and the stripped link depends
\ on that: src/habu/aot-lib.f ADR-TARGET! resolves such an ADR in the member its
\ site is in and refuses one whose target is anywhere else.
: PUT-CODEADDR ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id  id 0 RESULT-REG  id ADR-DELTA  ADR-WORD  APPEND ;

\ ---- leaving through another word --------------------------------------------
\ ONE instruction and never more: the selector only builds it where the pointer
\ already stands at the callee's entry base.
: PUT-TAILCALL ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id  id WORD-DELTA B-WORD  APPEND ;

\ ---- branching to a routine of the engine's own text --------------------------
\ Both routines below name an ABSOLUTE entry and not a displacement, so the
\ distance is measured from where this emission was placed. The BL participates
\ in the existing external-call relocation map, which is what carries the callee
\ into a stripped image and retargets the branch there.
: EXT-DELTA ( n -- n )
   PLACEMENT-CK -  INSN-BYTES /  N-INS @ - ;

\ ---- leaving through the routine that ends the process -----------------------
\ It does NOT note its callee: control that reaches here never returns to this
\ routine, so nothing the trap routine writes can be read by anybody.
: PUT-TRAP ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id  id DHEAD@  PUT-DMOVE
   \ die lives in engine text; the link is dead because it exits the process.
   id  id TRAP-ADDR EXT-DELTA  BL-WORD  APPEND ;

\ ---- the divide, and the refusal on its cold side ----------------------------
\ ARM64's Sdiv ANSWERS ZERO for a zero divisor, so the divisor is tested. A zero
\ divisor is a CALLER error the program can fix and recover from - it came from
\ the program's own arithmetic - so the cold side hands the caller
\ ARITH-ABI:E-DIV-ZERO through the runtime's `throw`, which is the refusal the
\ engine's own `/` makes (src/habu/habu1.f BDIV0?). It used to be a `brk`: a
\ word compiled at tier 1 and every AOT executable died with a register dump
\ where the interpreted division threw a code the caller could catch.
\
\ THE HOT PATH IS THE TWO INSTRUCTIONS IT ALWAYS WAS - the compare-and-branch
\ and the divide - and the three between them are never executed by a program
\ whose divisor is not zero. That is why the refusal is written in line rather
\ than reached through a block of its own: a branch to a shared block would cost
\ this site the very instruction its branch to `throw` costs, and buy a block
\ the layout has to place.
\
\ The code is ONE instruction because a Movn spells it: -6400 is ~6399. A code
\ needing a move-wide chain would make the form longer than DIV-INSNS says, so
\ ?IMM16 (src/arch/arm64/asm.f) refuses it rather than emitting a short form.
\
\ The push is `str xd,[x19],#8`, which is the engine's own G-PUSH in
\ src/habu/rt.f, so `throw` pops this code exactly as it pops one a checked
\ `throw` pushed. It is written into the register the DIVIDE's result holds,
\ which no path reads: the hot path has not divided yet, and the cold path never
\ comes back.
\
\ MIN-N -1 / IS MIN-N, the modular answer Sdiv gives, like the wrap `+`, `-` and
\ `*` already make (docs/forth.md). It is not a second refusal.
ARITH-ABI:E-DIV-ZERO invert constant DIV-CODE-IMM  \ the code as a Movn carries it

: PUT-SDIV ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id 0 RESULT-REG {: rd:n :}
   id  id 1 OPERAND-REG DIV-SKIP  ENC-CBNZ  APPEND
   id  rd DIV-CODE-IMM 0  MOVNHW  APPEND
   id  rd A64M:DSTACK-GPR CELL  ENC-STRPOST  APPEND
   id  id THROW-ADDR EXT-DELTA  BL-WORD  APPEND
   id  id TRIPLE  ENC-SDIV  APPEND ;

\ ---- the two ends of the frame ----------------------------------------------
\ The fused end writes its instruction at the FIRST operation of its pair and
\ nothing at the second, so the instruction keeps the position the operation
\ that opens or closes the bracket had. An op that writes nothing gets no
\ source-map row, for the reason an elided copy gets none.
: PUT-RESERVE ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   EM-FUSED @ 0<> if id WORD-LNKPUSH APPEND exit then
   id  id WORD-RESERVE  APPEND ;

: PUT-LINKSAVE ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   EM-FUSED @ 0<> if exit then
   id  id WORD-LNKSTR  APPEND ;

: PUT-LINKLOAD ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   EM-FUSED @ 0<> if id WORD-LNKPOP APPEND exit then
   id  id WORD-LNKLDR  APPEND ;

: PUT-RELEASE ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   EM-FUSED @ 0<> if exit then
   id  id WORD-RELEASE  APPEND ;

\ An elided copy gets no source-map row, for the reason an elided branch gets none.
: PUT-MOV ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id SELF-MOV? if exit then
   id  id WORD-MOV  APPEND ;

: PUT-FMOVDD ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id SELF-MOV? if exit then
   id  id WORD-FMOVDD  APPEND ;

\ ---- one operation, as the instructions it is --------------------------------
: PUT-DATAADDR ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id 0 BND-DATA-OFF @ ATTR-INT A64IR:DATA-OFFSET DATA-VA VA>N + {: addr:n :}
   id 0 RESULT-REG {: rd:n :}
   id rd addr 2 A64IR:HALF-OF 2 MOVZHW APPEND
   id rd addr 1 A64IR:HALF-OF 1 MOVKHW APPEND
   id rd addr 0 A64IR:HALF-OF 0 MOVKHW APPEND ;

: PUT-OP ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id home:n :}
   id SLOT-AT A64IR:NTH
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
      andi     OF id  id PAIRM ENC-ANDI  APPEND ENDOF
      orri     OF id  id PAIRM ENC-ORRI  APPEND ENDOF
      eori     OF id  id PAIRM ENC-EORI  APPEND ENDOF
      sdiv     OF id PUT-SDIV ENDOF
      and      OF id  id TRIPLE ENC-AND  APPEND ENDOF
      orr      OF id  id TRIPLE ENC-ORR  APPEND ENDOF
      eor      OF id  id TRIPLE ENC-EOR  APPEND ENDOF
      lslv     OF id  id TRIPLE ENC-LSLV  APPEND ENDOF
      lsrv     OF id  id TRIPLE ENC-LSRV  APPEND ENDOF
      mvn      OF id  id WORD-MVN  APPEND ENDOF
      store    OF id  id WORD-STORE  APPEND ENDOF
      load     OF id  id WORD-LOAD  APPEND ENDOF
      reserve  OF id PUT-RESERVE ENDOF
      release  OF id PUT-RELEASE ENDOF
      dtake    OF id PUT-DTAKE ENDOF
      dload    OF id  id WORD-DLOAD  APPEND ENDOF
      dstore   OF id  id WORD-DSTORE  APPEND ENDOF
      dpublish OF id PUT-DPUBLISH ENDOF
      dpush    OF id  id WORD-DPUSH  APPEND ENDOF
      dpop     OF id  id WORD-DPOP  APPEND ENDOF
      fdpush   OF id  id WORD-FDPUSH  APPEND ENDOF
      fdpop    OF id  id WORD-FDPOP  APPEND ENDOF
      aload    OF id  id WORD-ALOAD  APPEND ENDOF
      astore   OF id  id WORD-ASTORE  APPEND ENDOF
      fload    OF id  id WORD-FLOAD  APPEND ENDOF
      fstore   OF id  id WORD-FSTORE  APPEND ENDOF
      faload   OF id  id WORD-FALOAD  APPEND ENDOF
      fastore  OF id  id WORD-FASTORE  APPEND ENDOF
      fdload   OF id  id WORD-FDLOAD  APPEND ENDOF
      fdstore  OF id  id WORD-FDSTORE  APPEND ENDOF
      abload   OF id  id WORD-ABLOAD  APPEND ENDOF
      abstore  OF id  id WORD-ABSTORE  APPEND ENDOF
      flag     OF id PUT-FLAG ENDOF
      flagi    OF id PUT-FLAGI ENDOF
      selz     OF id PUT-SELZ ENDOF
      cmpsel   OF id PUT-CMPSEL ENDOF
      br       OF id home PUT-BR ENDOF
      brz      OF id home PUT-BRZ ENDOF
      cmpbr    OF id home PUT-CMPBR ENDOF
      cmpbri   OF id home PUT-CMPBRI ENDOF
      call     OF id PUT-CALL ENDOF
      wordcall OF id PUT-WORD-CALL ENDOF
      linksave OF id PUT-LINKSAVE ENDOF
      linkload OF id PUT-LINKLOAD ENDOF
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
      trap      OF id PUT-TRAP ENDOF
      codeaddr  OF id PUT-CODEADDR ENDOF
      dataaddr  OF id PUT-DATAADDR ENDOF
   ;MATCH ;

\ ---- the shape this leaf emits from ------------------------------------------
: FUN-AT ( n -- IR-ID:ir-fun-id )
   {: k:n :}
   k 0 < k N-FUNS @ >= or if E-A64EMIT-SHAPE throw then
   MKEY k IR-ID:PACK-FUN ;

: FUNS-CK ( -- )
   FUN-COUNT {: n:n :}
   n 1 < if E-A64EMIT-SHAPE throw then
   n FMAX > if E-A64EMIT-CAP throw then
   n N-FUNS ! ;

: TERMINATOR? ( IR-ID:ir-block-id n -- bool )
   OP-AT SLOT-AT {: k:n :}
   k O-RET = k O-BR = or k O-BRZ = or k O-CMPBR = or k O-CMPBRI = or
   k O-FCMPBR = or k O-FCMPBRZ = or k O-TAILCALL = or k O-TRAP = or ;

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
\ Five forms are a routine's CROSSINGS rather than its work. The same two appear
\ at a CALL SITE, which is why BODY-INSNS refuses a routine that calls.
: IFACE-FORM? ( n -- bool )
   {: k:n :}
   k O-DTAKE = k O-DLOAD = or k O-DSTORE = or k O-DPUBLISH = or k O-RET = or
   k O-FDLOAD = or k O-FDSTORE = or
   k O-DPUSH = or k O-DPOP = or  k O-FDPUSH = or k O-FDPOP = or ;

: CALL-FORM? ( n -- bool )
   {: k:n :}
   k O-CALL = k O-WORDCALL = or k O-TAILCALL = or ;

\ Both end control here, and a caller may not COPY such a routine: a copied `b`
\ would branch out of whatever caller it was copied into.
: LEAVE-FORM? ( n -- bool )
   {: k:n :}
   k O-TAILCALL = k O-TRAP = or ;

: PUT-COUNTED ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id home:n :}
   id SLOT-AT {: k:n :}
   N-INS @ {: was:n :}
   id home PUT-OP
   k EM-LAST !
   k CALL-FORM? if 1 EM-NCALL +! then
   k LEAVE-FORM? if 1 EM-TAIL +! then
   k IFACE-FORM? 0= if exit then
   N-INS @ was - EM-IFACE +! ;

: WALK-BLOCK ( IR-ID:ir-block-id n -- )
   {: bk:IR-ID:ir-block-id home:n :}
   bk OP-COUNT 0 ?do
      bk i STORED-RELOAD? 0= if bk i OP-AT home PUT-COUNTED then
   loop ;

\ Where a block's instructions begin is what every displacement was computed
\ from, so the writer arriving elsewhere means two different routines.
: CURSOR-CK ( n -- )
   START-AT N-INS @ <> if E-A64EMIT-LAYOUT throw then ;

: WALK ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   N-LAID @ 0 ?do
      i AT-POS CURSOR-CK
      f i AT-POS BLOCK-AT  i AT-POS  WALK-BLOCK
   loop
   N-INS @ LAY-AT @ <> if E-A64EMIT-LAYOUT throw then ;

\ ---- what one emission run is told -------------------------------------------
: BND-TAKE ( -- )
   BND-MODE @ {: have:n :}
   BOUND-NO BND-MODE !
   have BOUND-YES <> if E-A64EMIT-BIND throw then ;

\ Spent whatever the outcome, so a refused emission leaves no placement for the
\ next one to measure a branch against.
: PLACE-TAKE ( -- )
   PLACE-MODE @ PLACE-YES = if 1 else 0 then EM-PLACED !
   PLACE-AT-N @ EM-PLACE !
   PLACE-NO PLACE-MODE !
   0 PLACE-AT-N ! ;

: BND-MODULE-CK ( IR-BUILD:module -- )
   IR-BUILD:FMODULE  0 BND-MOD @  IR-ID:MODULE-SAME?
   0= if E-A64EMIT-MODULE throw then ;

: DIALECT-CK ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b  c b IR-BUILD:DIALECT@  A64IR:NAME IR-BUILD:SYMBOL-IS?
   0= if E-A64EMIT-MODULE throw then
   c b IR-BUILD:SCHEMA-MAJOR@ A64IR:MAJOR <> if E-A64EMIT-MODULE throw then
   c b IR-BUILD:SCHEMA-MINOR@ A64IR:MINOR <> if E-A64EMIT-MODULE throw then ;

\ Emission asks the registry for itself. The lowering stage accepted this
\ contract for its own reasons; whether instructions can be written for the
\ machine is this stage's question, and an architecture with no backend loaded
\ never reaches it - the registry refuses with E-CTGT-UNLOADED first.
: TARGET-CK ( IR-CTX:ctx -- )
   A64IR:CONTRACT@ CTARGET:EMITS?
   0= if E-A64EMIT-TARGET throw then ;

\ The probe makes staleness a refusal before a byte is written. It asks whether
\ the first value lives in a register at all, because in a module that spills
\ that value is the memory token, which has none.
: ALLOC-CK ( IR-BUILD:module -- )
   {: m:IR-BUILD:module :}
   A64RAV:ACCEPTED? 0= if E-A64EMIT-ALLOC throw then
   A64RA:VALUES 0 > if 0 A64RAV:REGISTERED? drop then
   m IR-BUILD:FMODULE A64RA:MODULE@ IR-ID:MODULE-SAME?
   0= if E-A64EMIT-ALLOC throw then ;

: SEAL-CK ( -- )
   ST @ ST-SEALED <> if E-A64EMIT-STATE throw then ;

: IDLE-CK ( -- )
   BND-MODE @ BOUND-NO <> if E-A64EMIT-BIND throw then
   ST @ ST-EMPTY <> if E-A64EMIT-STATE throw then
   N-INS @ 0<> if E-A64EMIT-STATE throw then
   PLACE-MODE @ PLACE-NO <> if E-A64EMIT-PLACE throw then
   EM-PLACED @ 0<> if E-A64EMIT-PLACE throw then ;

: CLEAR-CODE ( -- )
   INSN-CAP INSN-BYTES * 0 ?do 0 i BYTE! loop ;

\ The allocation bounds which registers this run may write. An emission that
\ wrote a register no value claimed means the two disagree.
: WRITES-CK ( -- )
   A64RAV:GPR-WRITTEN NEFF:GPRS-N {: g:n :}
   A64RAV:FPR-WRITTEN NEFF:FPRS-N {: f:n :}
   EM-WGPR @ g invert and 0<> if E-A64EMIT-CLOBBER throw then
   EM-WFPR @ f invert and 0<> if E-A64EMIT-CLOBBER throw then ;

: ORD-CK ( n -- n )
   dup 0 < over N-INS @ >= or if E-A64EMIT-BOUND throw then ;

public

\ ---- binding the dialect -----------------------------------------------------
\ The only moment a module can be asked its opcode and key identities, because
\ its symbols are its own ordinals.
: BIND-DIALECT ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   BND-MODE @ BOUND-YES = if E-A64EMIT-BIND throw then
   c b DIALECT-CK
   c b 0 BND-OP A64IR:OPCODES A64IR:BIND-OPCODES! 0 BND-MOD !
   c b A64IR:KEY-IMM    0 BND-IMM !
   c b A64IR:KEY-DATA-OFFSET 0 BND-DATA-OFF !
   c b A64IR:KEY-SHIFT  0 BND-SH !
   c b A64IR:KEY-ADDR   0 BND-ADDR !
   c b A64IR:KEY-SLOT   0 BND-SLOT !
   c b A64IR:KEY-FRAME  0 BND-FRAME !
   c b A64IR:KEY-DSLOT  0 BND-DSLOT !
   c b A64IR:KEY-DBYTES 0 BND-DBYTES !
   c b A64IR:KEY-DWB    0 BND-DWB !
   c b A64IR:KEY-COND   0 BND-COND !
   c b A64IR:KEY-DBACK  0 BND-DBACK !
   c b A64IR:KEY-ENTRY  0 BND-ENTRY !
   c b A64IR:KEY-TRAP-ENTRY 0 BND-TRAP !
   c b A64IR:KEY-THROW-ENTRY 0 BND-THROW !
   c b A64IR:KEY-OFF    0 BND-OFF !
   c b A64IR:KEY-MASK   0 BND-MASK !
   c b A64IR:KEY-FUN    0 BND-FUN !
   BOUND-YES BND-MODE ! ;

: BOUND? ( -- bool )
   BND-MODE @ BOUND-YES = ;

: RELEASE ( -- )
   PLACE-TAKE
   BND-TAKE ;

\ Retiring one definition is deliberately nonthrowing.  The compiler calls it
\ on both the accepting and rejecting path after returning any live bindings.
: RETIRE ( -- )
   ST-EMPTY ST !
   0 N-INS !
   0 EM-PLACED !
   0 EM-PLACE !
   PLACE-NO PLACE-MODE !
   0 PLACE-AT-N ! ;

\ The code buffer may retain absolute addresses from the last compiled word.
\ Scrub it once, after the compiler has proved that no emission is live.
: CAPTURE-PREPARE ( -- )
   IDLE-CK
   CLEAR-CODE ;

\ ---- declaring where this routine will be written ----------------------------
\ There is no default: an emission that needs a placement and was given none is
\ refused by name. What is checked is that it could be an instruction address.
: PLACE-AT ( n -- )
   {: at:n :}
   PLACE-MODE @ PLACE-YES = if E-A64EMIT-PLACE throw then
   at 0 < if E-A64EMIT-PLACE throw then
   at INSN-BYTES mod 0<> if E-A64EMIT-PLACE throw then
   at PLACE-AT-N !
   PLACE-YES PLACE-MODE ! ;

: PLACED? ( -- bool )
   SEAL-CK EM-PLACED @ 0<> ;

: PLACEMENT ( -- n )
   SEAL-CK EM-PLACE @ ;

\ ---- the pass ----------------------------------------------------------------
\ Shape first, then the ORDER - both questions about the module alone - and only
\ then the acceptance, because a copy's instruction count is a fact about the
\ assignment. The functions are measured before any of them is written.
: SHAPES-CK ( -- )
   N-FUNS @ 0 ?do i FUN-AT SHAPE-CK loop ;

: PLAN-DMOVES ( -- )
   N-FUNS @ 0 ?do
      i FUN-AT {: f:IR-ID:ir-fun-id :}
      f BLOCK-COUNT 0 ?do
         f i BLOCK-AT {: bk:IR-ID:ir-block-id :}
         -1
         bk OP-COUNT 0 ?do bk i OP-AT PLAN-DOP loop
         drop
      loop
   loop ;

: MEASURE ( -- )
   0
   N-FUNS @ 0 ?do
      dup i cells F-START + !
      i FUN-AT {: f:IR-ID:ir-fun-id :}
      f ORDER-BLOCKS
      f FRAME-SHAPE!
      f over LAYOUT
      drop LAY-AT @
   loop
   drop ;

: WRITE-ALL ( -- )
   N-FUNS @ 0 ?do
      i FUN-AT {: f:IR-ID:ir-fun-id :}
      f ORDER-BLOCKS
      f FRAME-SHAPE!
      f  i cells F-START + @  LAYOUT
      i cells F-START + @ N-INS @ <> if E-A64EMIT-LAYOUT throw then
      f WALK
   loop ;

\ ---- where this emission's address chains start ------------------------------
\ DATA carriers have three instructions; full absolute carriers have four.
\ Every instruction must retain the address kind and destination. The decoder
\ runs only at such a declared site, never over numeric lookalikes.
: SITE-CEIL ( -- n ) INSN-CAP 3 / ;
DYNAMIC-BUFFER SITES-BUF n
: SITES ( -- ptr n ) 0 SITES-BUF ;
variable N-SITES

: ADDR-LANE? ( n n n -- bool )
   {: k:n kind:n rd:n :}
   k cells M-ADDR + @ kind =
   k cells M-ARD + @ rd = and ;

: CHAIN-LANES ( n -- n ) {: k:n :}
   k 3 + N-INS @ <= if
      k cells M-ADDR + @ A64IR:ADDR-DATA = if
         k cells M-ARD + @ {: rd:n :}
         k INSN-BYTES * {: off:n :}
         off BYTE@ off 1+ BYTE@ 8 lshift or
         off 2 + BYTE@ 16 lshift or off 3 + BYTE@ 24 lshift or
         $FFE0001F and $D2C00000 rd or = if 3 exit then
      then
   then
   A64IR:HALVES ;

: CHAIN-CK ( n n -- )
   {: k:n lanes:n :}
   k lanes + N-INS @ > if E-A64EMIT-ADDR throw then
   k cells M-ADDR + @ {: kind:n :}
   k cells M-ARD + @ {: rd:n :}
   lanes 0 ?do
      k i + kind rd ADDR-LANE? 0= if E-A64EMIT-ADDR throw then
   loop ;

: SITE+ ( n -- )
   {: k:n :}
   N-SITES @ SITE-CEIL >= if E-A64EMIT-ADDR throw then
   k N-SITES @ cells SITES + !
   N-SITES @ 1+ N-SITES ! ;

variable SCAN-K

: SCAN-ADDR-SITES ( -- )
   SITE-CEIL 1 max SITES-BUF-RESERVE
   0 N-SITES !
   0 SCAN-K !
   begin SCAN-K @ N-INS @ < while
      SCAN-K @ cells M-ADDR + @ A64IR:ADDR-NONE = if
         SCAN-K @ 1+ SCAN-K !
      else
         SCAN-K @ CHAIN-LANES {: lanes:n :}
         SCAN-K @ lanes CHAIN-CK
         SCAN-K @ SITE+
         SCAN-K @ lanes + SCAN-K !
      then
   repeat ;

: EMIT ( IR-CTX:ctx IR-BUILD:module -- )
   {: c:IR-CTX:ctx m:IR-BUILD:module :}
   BND-TAKE
   PLACE-TAKE
   ST-EMPTY ST !
   0 N-INS !
   0 EM-WGPR !
   0 EM-WFPR !
   0 EM-IFACE !
   0 EM-NCALL !
   0 EM-TAIL !
   0 EM-FUSED !
   0 EM-FRAME !
   -1 EM-LAST !
   m BND-MODULE-CK
   c TARGET-CK
   m VIEWS!
   RESERVE-SCRATCH
   FUNS-CK
   SHAPES-CK
   m ALLOC-CK
   PLAN-DMOVES
   MEASURE
   WRITE-ALL
   WRITES-CK
   SCAN-ADDR-SITES
   ST-SEALED ST ! ;

\ ---- the sealed emission -----------------------------------------------------
: SEALED? ( -- bool )
   ST @ ST-SEALED = ;

\ A routine that returns on one path and traps on another does both: it may not
\ be copied. TRAILING-RETURN? separately selects legacy versus exact span length.
: LEAVES-BY-BRANCH? ( -- bool )
   SEAL-CK EM-TAIL @ 0<> ;

\ The last OPERATION is asked about rather than the last instruction word,
\ because a return is exactly one instruction.
: TRAILING-RETURN? ( -- bool )
   SEAL-CK EM-LAST @ O-RET = ;

: INSNS ( -- n )
   SEAL-CK N-INS @ ;

\ The byte offset of one function in this sealed emission. Publication uses the
\ clause function's real measured start for the companion `;does` record.
: FUNCTION-OFFSET@ ( n -- n )
   SEAL-CK FUN-START INSN-BYTES * ;

\ An INSTRUCTION INDEX, which is the coordinate the seam turns into an address
\ by the same arithmetic it uses for a call site.
: ADDR-SITES ( -- n )
   SEAL-CK N-SITES @ ;

: ADDR-SITE@ ( n -- n )
   {: i:n :}
   SEAL-CK
   i 0 < i N-SITES @ >= or if E-A64EMIT-BOUND throw then
   i cells SITES + @ ;

: ADDR-SITE-KIND@ ( n -- n )
   {: i:n :}
   SEAL-CK
   i 0 < i N-SITES @ >= or if E-A64EMIT-BOUND throw then
   i cells SITES + @ cells M-ADDR + @ ;

\ Measured while the crossings are written rather than derived from an arity: an
\ arity-derived interface OVERSTATES what most routines pay and so UNDERSTATES
\ their bodies, which is the unsound direction. A routine that calls is refused.
: BODY-INSNS ( -- n )
   SEAL-CK
   EM-NCALL @ 0<> if E-A64EMIT-BODY throw then
   N-INS @ EM-IFACE @ - ;

\ ---- the block layout, read back ---------------------------------------------
\ The count of POSITIONS and not of blocks: the layout leaves out the blocks
\ every branch was redirected past. BLOCK-START@ is still keyed by ORDINAL.
: BLOCKS ( -- n )
   SEAL-CK N-LAID @ ;

: BLOCK-START@ ( n -- n )
   SEAL-CK BLK-ORD-CK cells B-START + @ ;

: DROPPED ( -- n )
   SEAL-CK N-BLK @ N-LAID @ - ;

: GOTO@ ( n -- n )
   SEAL-CK GOTO-OF ;

\ A position past the last laid is a refusal, not a leftover: the table is not
\ cleared beyond the positions this routine has.
: BLOCK-AT-POS@ ( n -- n )
   SEAL-CK
   dup 0 < over N-LAID @ >= or if E-A64EMIT-BLOCK throw then
   AT-POS ;

: SIZE ( -- n )
   SEAL-CK N-INS @ INSN-BYTES * ;

\ Published because it is half of an invariant nobody can state from one side:
\ INSN-CAP * INSN-BYTES under the Adr field's reach is what makes E-A64EMIT-REACH
\ unreachable for an Adr.
: INSN-MAX ( -- n )
   INSN-CAP ;

: BYTES ( -- ptr u8 )
   SEAL-CK CODE ;

: WORD@ ( n -- n )
   SEAL-CK ORD-CK INSN-BYTES * {: off:n :}
   off BYTE@
   off 1+ BYTE@ 8 lshift or
   off 2 + BYTE@ 16 lshift or
   off 3 + BYTE@ 24 lshift or ;

\ ---- the source map ----------------------------------------------------------
: MAP-OFFSET@ ( n -- n )
   SEAL-CK ORD-CK cells M-OFF + @ ;

: MAP-SPAN@ ( n -- IR-SOURCE:span )
   SEAL-CK ORD-CK {: k:n :}
   k M-SRC @  k cells M-ST + @  k cells M-LN + @  IR--SOURCE-SPAN:MAKE ;

public
: RESET-SCRATCH ( -- )
   0 SCRATCH-VALUES ! 0 SCRATCH-BLOCKS ! 0 SCRATCH-FUNS ! 0 SCRATCH-OPS ! ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;using
;using
;package

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
\ Three instructions are elided, and each rule is written once and asked twice -
\ by the layout that counts and by the writer that appends, with CURSOR-CK
\ holding the two together: a trailing branch to the block laid out next
\ (FALL-THRU?), a copy into its own register (COPY?), and a data-stack
\ adjustment of nothing. An elided instruction gets NO source-map row, because
\ row k describes the instruction WORD@ k answers.
\
\ Block zero is written first and the block control leaves through last, because
\ publish.f records a length that says the emission ends in the return.

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
using A64ASM
using NFROZEN
private

\ ---- the bound dialect -------------------------------------------------------
\ One slot per member of the family, so a member added to A64IR:opcode fails to
\ compile here until it has a slot and an encoding.
76 constant OPCODES-N
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
63 constant O-ANDI
64 constant O-ORRI
65 constant O-EORI
66 constant O-FLOAD
67 constant O-FSTORE
68 constant O-FALOAD
69 constant O-FASTORE
70 constant O-FDLOAD
71 constant O-FDSTORE
72 constant O-TRAP
73 constant O-CODEADDR
74 constant O-FLAGI
75 constant O-CMPBRI

0 constant BOUND-NO
1 constant BOUND-YES

\ ---- how much of one routine this pass holds ----------------------------------
\ Three per operation is the ceiling: five forms emit more than one and none
\ emits more than three.
3 constant INSN-PER-OP
INSN-PER-OP VMAX BMAX 3 * + * constant INSN-CAP

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
variable EM-CGPR
0 EM-CGPR !
variable EM-CFPR
0 EM-CFPR !

\ A routine destroys what its own instructions write AND everything the routines
\ it calls write; this is the second half, and it is not something these
\ instructions did.
variable EM-KGPR
0 EM-KGPR !
variable EM-KFPR
0 EM-KFPR !

\ The interface total is read off the cursor's own difference, so it is the same
\ number by construction whatever elisions apply.
variable EM-IFACE
0 EM-IFACE !
variable EM-NCALL
0 EM-NCALL !
variable EM-TAIL                     \ branches out of the routine this emission wrote
0 EM-TAIL !
variable EM-LAST                     \ the form of the last operation it wrote
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
OPCODES-N TYPED-BUFFER BND-OP IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-IMM IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-ADDR IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-SH IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-SLOT IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-FRAME IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-DSLOT IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-DBYTES IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-COND IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-DBACK IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-ENTRY IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-TRAP IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-FUN IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-OFF IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-MASK IR-ID:ir-symbol-id

create CODE INSN-CAP INSN-BYTES * allot
\ A relocation pass may not recognise an address chain by decoding region bytes,
\ so the kind travels from the elaborator and is recorded per INSTRUCTION.
create M-ADDR INSN-CAP cells allot
create M-ARD INSN-CAP cells allot
create M-OFF INSN-CAP cells allot
create M-ST INSN-CAP cells allot
create M-LN INSN-CAP cells allot
INSN-CAP TYPED-BUFFER M-SRC IR-ID:ir-source-id

\ Keyed by ORDINAL, because a branch names an ordinal.
create B-START BMAX cells allot

\ One permutation held twice, because both directions are asked in an inner loop.
create B-ORDER BMAX cells allot        \ position -> block ordinal
create B-PLACE BMAX cells allot        \ block ordinal -> position

\ A block whose whole content is an unconditional branch is passed through, so a
\ branch to it can name the far end and the block itself becomes unreachable.
create B-GOTO BMAX cells allot         \ block ordinal -> the block a branch to it should name
create B-KEEP BMAX cells allot         \ block ordinal -> is it still reachable
variable N-LAID                        \ how many blocks the order actually holds

\ A function's start has to outlive the per-function tables, because an
\ a64.codeaddr in an EARLIER function names a LATER one's entry. So EMIT lays
\ every function out once to fill this, then lays each out again to write it.
create F-START FMAX cells allot        \ function ordinal -> its first instruction
variable N-FUNS                        \ how many functions the emission holds

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
      trap      OF O-TRAP      ENDOF
      codeaddr  OF O-CODEADDR  ENDOF
      flagi     OF O-FLAGI     ENDOF
      cmpbri    OF O-CMPBRI    ENDOF
      madd      OF O-MADD      ENDOF
      addi      OF O-ADDI      ENDOF
      subi      OF O-SUBI      ENDOF
      movn      OF O-MOVN      ENDOF
      andi      OF O-ANDI      ENDOF
      orri      OF O-ORRI      ENDOF
      eori      OF O-EORI      ENDOF
      fload     OF O-FLOAD     ENDOF
      fstore    OF O-FSTORE    ENDOF
      faload    OF O-FALOAD    ENDOF
      fastore   OF O-FASTORE   ENDOF
      fdload    OF O-FDLOAD    ENDOF
      fdstore   OF O-FDSTORE   ENDOF
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
      O-TRAP      of A64IR-OPCODE:TRAP      endof
      O-CODEADDR  of A64IR-OPCODE:CODEADDR  endof
      O-FLAGI     of A64IR-OPCODE:FLAGI     endof
      O-CMPBRI    of A64IR-OPCODE:CMPBRI    endof
      O-MADD      of A64IR-OPCODE:MADD      endof
      O-ADDI      of A64IR-OPCODE:ADDI      endof
      O-SUBI      of A64IR-OPCODE:SUBI      endof
      O-MOVN      of A64IR-OPCODE:MOVN      endof
      O-ANDI      of A64IR-OPCODE:ANDI      endof
      O-ORRI      of A64IR-OPCODE:ORRI      endof
      O-EORI      of A64IR-OPCODE:EORI      endof
      O-FLOAD     of A64IR-OPCODE:FLOAD     endof
      O-FSTORE    of A64IR-OPCODE:FSTORE    endof
      O-FALOAD    of A64IR-OPCODE:FALOAD    endof
      O-FASTORE   of A64IR-OPCODE:FASTORE   endof
      O-FDLOAD    of A64IR-OPCODE:FDLOAD    endof
      O-FDSTORE   of A64IR-OPCODE:FDSTORE   endof
      E-A64EMIT-OPCODE throw
   endcase ;

\ A form outside the family has no encoding here and is refused rather than guessed.
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
   dup ZERO-REG = if E-A64COMB-ADDEND throw then ;

\ In the order `madd rd, rn, rm, ra` names them, which is the order the schema
\ declares its operands in.
: QUAD ( IR-ID:ir-op-id -- n n n n )
   {: id:IR-ID:ir-op-id :}
   id 0 RESULT-REG  id 0 OPERAND-REG  id 1 OPERAND-REG  id 2 OPERAND-REG ?ADDEND ;

: WORD-STORE ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id 0 OPERAND-REG  A64EFF:SP-GPR  id SLOT-OFF  ENC-STR ;

: WORD-LOAD ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id 0 RESULT-REG  A64EFF:SP-GPR  id SLOT-OFF  ENC-LDR ;

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
   A64EFF:SP-GPR A64EFF:SP-GPR  id FRAME-SIZE  ENC-SUBI ;

: WORD-RELEASE ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   A64EFF:SP-GPR A64EFF:SP-GPR  id FRAME-SIZE  ENC-ADDI ;

\ Over the pointer is the scaled unsigned field, Ldr and Str; under it is the
\ unscaled signed field, Ldur and Stur. One dialect form written two ways.
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

\ Each is its general twin with one encoder swapped, so a double reaches memory
\ and leaves it in the file it lives in.
: DENC-LDRD ( n n n -- n )
   dup 0 < if ENC-LDURD exit then ENC-LDRD ;

: DENC-STRD ( n n n -- n )
   dup 0 < if ENC-STURD exit then ENC-STRD ;

: WORD-FSTORE ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id 0 OPERAND-REG  A64EFF:SP-GPR  id SLOT-OFF  ENC-STRD ;

: WORD-FLOAD ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id 0 RESULT-REG  A64EFF:SP-GPR  id SLOT-OFF  ENC-LDRD ;

: WORD-FDLOAD ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id 0 RESULT-REG  A64EFF:DSTACK-GPR  id DSLOT-OFF  DENC-LDRD ;

: WORD-FDSTORE ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id 0 OPERAND-REG  A64EFF:DSTACK-GPR  id DSLOT-OFF  DENC-STRD ;

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

\ A property of the FORM: one for all but the three comparisons, the division,
\ the two calls and the three compare-and-branches, and the two-way branch and
\ the eight conditional selects, which are two.
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
   k O-FLAGI = if 3 exit then
   k O-FFLAG = if 3 exit then
   k O-FFLAGZ = if 3 exit then
   k O-SDIV = if 3 exit then
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

: FOLLOWER ( IR-ID:ir-fun-id n -- n )
   {: f:IR-ID:ir-fun-id b:n :}
   f b BLOCK-AT TAIL-BLOCK {: s:n :}
   s 0 < if NEXT-UNLAID exit then
   s LAID? if NEXT-UNLAID exit then
   s ;

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
: FALL-THRU? ( IR-ID:ir-op-id n -- bool )
   {: id:IR-ID:ir-op-id home:n :}
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
   r k 1- LAY
   k 1 = if exit then
   r 0= if E-A64EMIT-SHAPE throw then
   0 0 LAY
   k 1- 1 ?do
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
   id  id 0 OPERAND-REG  id 0 SUCC-BLOCK GOTO-OF DELTA  BZ-WORD  APPEND
   id home FALL-THRU? if exit then
   id  id 1 SUCC-BLOCK GOTO-OF DELTA B-WORD  APPEND ;

\ The comparison writes only the flags and the branch beside it reads them
\ there, so no register is written. The conditional's displacement is measured
\ after the compare is appended, because it is counted from its own instruction.
: PUT-CMPBR ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id home:n :}
   id  id 0 OPERAND-REG id 1 OPERAND-REG ENC-CMP  APPEND
   id  id 0 SUCC-BLOCK GOTO-OF DELTA  id COND-OF  BCOND-WORD  APPEND
   id home FALL-THRU? if exit then
   id  id 1 SUCC-BLOCK GOTO-OF DELTA B-WORD  APPEND ;

: PUT-CMPBRI ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id home:n :}
   id  id 0 OPERAND-REG id OFF-IMM ENC-CMPI  APPEND
   id  id 0 SUCC-BLOCK GOTO-OF DELTA  id COND-OF  BCOND-WORD  APPEND
   id home FALL-THRU? if exit then
   id  id 1 SUCC-BLOCK GOTO-OF DELTA B-WORD  APPEND ;

\ The Fcmp raises the unordered condition for a NaN and the conditions selection
\ names are all false under it, so control reaches the SECOND successor.
: PUT-FCMPBR ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id home:n :}
   id  id 0 OPERAND-REG id 1 OPERAND-REG ENC-FCMP  APPEND
   id  id 0 SUCC-BLOCK GOTO-OF DELTA  id COND-OF  BCOND-WORD  APPEND
   id home FALL-THRU? if exit then
   id  id 1 SUCC-BLOCK GOTO-OF DELTA B-WORD  APPEND ;

: PUT-FCMPBRZ ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id home:n :}
   id  id 0 OPERAND-REG ENC-FCMP0  APPEND
   id  id 0 SUCC-BLOCK GOTO-OF DELTA  id COND-OF  BCOND-WORD  APPEND
   id home FALL-THRU? if exit then
   id  id 1 SUCC-BLOCK GOTO-OF DELTA B-WORD  APPEND ;

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

\ It skips exactly the one instruction between it and the divide, so it is
\ written here rather than measured off the label table.
2 constant DIV-SKIP                  \ words from the guard to the divide

: PUT-SDIV ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id  id 1 OPERAND-REG DIV-SKIP ENC-CBNZ  APPEND
   id  ENC-BRK  APPEND
   id  id TRIPLE ENC-SDIV  APPEND ;

\ ---- moving the data-stack pointer -------------------------------------------
\ NO INSTRUCTION AT ALL when the distance is zero, which is the ordinary case.
: PUT-DMOVE ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id d:n :}
   d 0= if exit then
   d 0 > if
      id  A64EFF:DSTACK-GPR A64EFF:DSTACK-GPR d  ENC-ADDI  APPEND
      exit
   then
   id  A64EFF:DSTACK-GPR A64EFF:DSTACK-GPR d negate  ENC-SUBI  APPEND ;

: PUT-DTAKE ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id  id DBYTES-SIZE negate  PUT-DMOVE ;

: PUT-DPUBLISH ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id  id DBYTES-SIZE  PUT-DMOVE ;

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

\ A self-call adds nothing, and does not stop adding nothing inside a quotation:
\ the callee is a function of THIS emission, whose registers this same set is
\ being collected over. A word with no recorded row adds the whole register file,
\ because nothing is known about it.
: NOTE-CALLEE ( n -- )
   {: e:n :}
   e A64EFF:GPR-ALL NCLOB:GPR-CLOB A64EFF:GPRS-N  EM-KGPR @ or  EM-KGPR !
   e A64EFF:FPR-ALL NCLOB:FPR-CLOB A64EFF:FPRS-N  EM-KFPR @ or  EM-KFPR ! ;

: PUT-CALL ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id  id DBYTES-SIZE  PUT-DMOVE
   id  SELF-FUN FUN-START  N-INS @ -  BL-WORD  APPEND
   id  id DBACK-SIZE negate  PUT-DMOVE ;

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
   id ENTRY-ADDR NOTE-CALLEE
   id  id DBYTES-SIZE  PUT-DMOVE
   id  id WORD-DELTA BL-WORD  APPEND
   id  id DBACK-SIZE negate  PUT-DMOVE ;

\ In BYTES, which is the unit this field counts - the branch fields count
\ instructions.
: ADR-DELTA ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id FUN-OF FUN-START  N-INS @ -  INSN-BYTES * ;

: ADR-WORD ( n n -- n )
   {: rd:n d:n :}
   d A64IR:ADR-FITS? 0= if E-A64EMIT-REACH throw then
   rd d ENC-ADR ;

: PUT-CODEADDR ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id  id 0 RESULT-REG  id ADR-DELTA  ADR-WORD  APPEND ;

\ ---- leaving through another word --------------------------------------------
\ ONE instruction and never more: the selector only builds it where the pointer
\ already stands at the callee's entry base.
: PUT-TAILCALL ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id ENTRY-ADDR NOTE-CALLEE
   id  id WORD-DELTA B-WORD  APPEND ;

\ ---- leaving through the routine that ends the process -----------------------
\ It does NOT note its callee: control that reaches here never returns to this
\ routine, so nothing the trap routine writes can be read by anybody.
: PUT-TRAP ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id  id DBYTES-SIZE  PUT-DMOVE
   id  id TRAP-ADDR  PLACEMENT-CK -  INSN-BYTES /  N-INS @ -  B-WORD  APPEND ;

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
      reserve  OF id  id WORD-RESERVE  APPEND ENDOF
      release  OF id  id WORD-RELEASE  APPEND ENDOF
      dtake    OF id PUT-DTAKE ENDOF
      dload    OF id  id WORD-DLOAD  APPEND ENDOF
      dstore   OF id  id WORD-DSTORE  APPEND ENDOF
      dpublish OF id PUT-DPUBLISH ENDOF
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
      trap      OF id PUT-TRAP ENDOF
      codeaddr  OF id PUT-CODEADDR ENDOF
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
   k O-FDLOAD = or k O-FDSTORE = or ;

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
      bk i OP-AT home PUT-COUNTED
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

: BIND1 ( IR-CTX:ctx IR-BUILD:builder A64IR:opcode -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder o:A64IR:opcode :}
   c b o A64IR:OPCODE  o SLOT-OF BND-OP ! ;

: DIALECT-CK ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b  c b IR-BUILD:DIALECT@  A64IR:NAME IR-BUILD:SYMBOL-IS?
   0= if E-A64EMIT-MODULE throw then
   c b IR-BUILD:SCHEMA-MAJOR@ A64IR:MAJOR <> if E-A64EMIT-MODULE throw then
   c b IR-BUILD:SCHEMA-MINOR@ A64IR:MINOR <> if E-A64EMIT-MODULE throw then ;

: TARGET-CK ( IR-CTX:ctx -- )
   IR-CTX:BINDING@ CBIND:VALIDATE CBIND:TARGET@ CTARGET:ARCH@
   CTARGET-ARCH:AARCH64 CTARGET-ARCH:EQ
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

\ The allocation's answer is what may be published; this run's answer is what
\ proves it is not too narrow. An emission that wrote a register no value
\ claimed means the two disagree about what the routine is.
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
\ The only moment a module can be asked its opcode and key identities, because
\ its symbols are its own ordinals.
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
   c b A64IR-OPCODE:TRAP      BIND1
   c b A64IR-OPCODE:CODEADDR  BIND1
   c b A64IR-OPCODE:FLAGI     BIND1
   c b A64IR-OPCODE:CMPBRI    BIND1
   c b A64IR-OPCODE:MADD      BIND1
   c b A64IR-OPCODE:ADDI      BIND1
   c b A64IR-OPCODE:SUBI      BIND1
   c b A64IR-OPCODE:MOVN      BIND1
   c b A64IR-OPCODE:ANDI      BIND1
   c b A64IR-OPCODE:ORRI      BIND1
   c b A64IR-OPCODE:EORI      BIND1
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
   c b A64IR-OPCODE:FLOAD    BIND1
   c b A64IR-OPCODE:FSTORE   BIND1
   c b A64IR-OPCODE:FALOAD   BIND1
   c b A64IR-OPCODE:FASTORE  BIND1
   c b A64IR-OPCODE:FDLOAD   BIND1
   c b A64IR-OPCODE:FDSTORE  BIND1
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
   c b A64IR:KEY-ADDR   0 BND-ADDR !
   c b A64IR:KEY-SLOT   0 BND-SLOT !
   c b A64IR:KEY-FRAME  0 BND-FRAME !
   c b A64IR:KEY-DSLOT  0 BND-DSLOT !
   c b A64IR:KEY-DBYTES 0 BND-DBYTES !
   c b A64IR:KEY-COND   0 BND-COND !
   c b A64IR:KEY-DBACK  0 BND-DBACK !
   c b A64IR:KEY-ENTRY  0 BND-ENTRY !
   c b A64IR:KEY-TRAP-ENTRY 0 BND-TRAP !
   c b A64IR:KEY-OFF    0 BND-OFF !
   c b A64IR:KEY-MASK   0 BND-MASK !
   c b A64IR:KEY-FUN    0 BND-FUN !
   BOUND-YES BND-MODE ! ;

: BOUND? ( -- bool )
   BND-MODE @ BOUND-YES = ;

: RELEASE ( -- )
   PLACE-TAKE
   BND-TAKE ;

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

: MEASURE ( -- )
   0
   N-FUNS @ 0 ?do
      dup i cells F-START + !
      i FUN-AT {: f:IR-ID:ir-fun-id :}
      f ORDER-BLOCKS
      f over LAYOUT
      drop LAY-AT @
   loop
   drop ;

: WRITE-ALL ( -- )
   N-FUNS @ 0 ?do
      i FUN-AT {: f:IR-ID:ir-fun-id :}
      f ORDER-BLOCKS
      f  i cells F-START + @  LAYOUT
      i cells F-START + @ N-INS @ <> if E-A64EMIT-LAYOUT throw then
      f WALK
   loop ;

\ ---- where this emission's address chains start ------------------------------
\ One chain is four instructions, so the bound is derived rather than chosen.
\ A site is admitted only for a run of EXACTLY four consecutive lanes of one
\ kind writing the SAME register; every other run length is a refusal.
INSN-CAP A64IR:HALVES / constant SITE-CEIL
create SITES SITE-CEIL cells allot
variable N-SITES

: RUN-AT ( n -- n )
   {: k:n :}
   k cells M-ADDR + @ {: kind:n :}
   k cells M-ARD + @ {: rd:n :}
   0
   N-INS @ k ?do
      i cells M-ADDR + @ kind =
      i cells M-ARD + @ rd = and 0= if leave then
      1+
   loop ;

: SITE+ ( n -- )
   {: k:n :}
   N-SITES @ SITE-CEIL >= if E-A64EMIT-ADDR throw then
   k N-SITES @ cells SITES + !
   N-SITES @ 1+ N-SITES ! ;

variable SCAN-K

: SCAN-ADDR-SITES ( -- )
   0 N-SITES !
   0 SCAN-K !
   begin SCAN-K @ N-INS @ < while
      SCAN-K @ cells M-ADDR + @ A64IR:ADDR-NONE = if
         SCAN-K @ 1+ SCAN-K !
      else
         SCAN-K @ RUN-AT {: n:n :}
         n A64IR:HALVES <> if E-A64EMIT-ADDR throw then
         SCAN-K @ SITE+
         SCAN-K @ n + SCAN-K !
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
   0 EM-KGPR !
   0 EM-KFPR !
   0 EM-IFACE !
   0 EM-NCALL !
   0 EM-TAIL !
   -1 EM-LAST !
   m BND-MODULE-CK
   c TARGET-CK
   m VIEWS!
   FUNS-CK
   SHAPES-CK
   m ALLOC-CK
   MEASURE
   WRITE-ALL
   CLOBBER-SEAL
   SCAN-ADDR-SITES
   ST-SEALED ST ! ;

\ ---- the sealed emission -----------------------------------------------------
: SEALED? ( -- bool )
   ST @ ST-SEALED = ;

\ Read off the SEALED run, so an answer was reached under the allocation this
\ emission was made against.
: GPR-CLOBBER ( -- A64EFF:gprs )
   SEAL-CK EM-CGPR @ A64EFF:GPR-SET ;

: FPR-CLOBBER ( -- A64EFF:fprs )
   SEAL-CK EM-CFPR @ A64EFF:FPR-SET ;

\ A routine that returns on one path and traps on another does both: it may not
\ be copied, AND its emission ends in the return the recorded length leaves out.
: LEAVES-BY-BRANCH? ( -- bool )
   SEAL-CK EM-TAIL @ 0<> ;

\ The last OPERATION is asked about rather than the last instruction word,
\ because a return is exactly one instruction.
: TRAILING-RETURN? ( -- bool )
   SEAL-CK EM-LAST @ O-RET = ;

: INSNS ( -- n )
   SEAL-CK N-INS @ ;

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

private
get-current prot-wid-add

public
get-current prot-wid-add

;using
;using
;package

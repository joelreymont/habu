\ combine.f - the module in which an operation and the one instruction-field it
\ could have stood in are one instruction: a multiply and the addition that reads
\ its product, and a constant and the arithmetic, bitwise or comparison that
\ reads it. One concern: finding those pairs and writing the module that holds
\ the combined form.
\
\ It runs BEFORE the allocator, which is the whole reason it can see a pattern at
\ all: the same two instructions after allocation are often unfusable.
\
\ Every fold requires the folded value to have EXACTLY ONE use and to be defined
\ in the same block. A value read twice still needs its register, so folding one
\ reader would leave the producer in place and ADD an instruction.
\
\ One rewrite at a time: the value map is a package-owned slot and the old module
\ is read through the one cursor src/compiler/native/frozen.f owns.

require lib/prelude.f
require lib/errors.f
require src/compiler/digest.f
require src/compiler/ir/id.f
require src/compiler/ir/context.f
require src/compiler/ir/symbol.f
require src/compiler/ir/type.f
require src/compiler/ir/source.f
require src/compiler/ir/schema.f
require src/compiler/ir/fun.f
require src/compiler/ir/build.f
require src/compiler/native/a64ir.f
require src/compiler/native/frozen.f

package A64COMB
using NFROZEN
private

\ ---- the bound dialect -------------------------------------------------------
\ One slot per member of the machine operation family, so a member added to
\ A64IR:opcode fails to compile here until it has a slot and a rebuild rule.
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
\ This pass writes no attribute of its own but COPIES every one the selector
\ built, and a field copied under the wrong key would misread a frame.
14 constant KEYS-N
0 constant K-IMM
1 constant K-SHIFT
2 constant K-SLOT
3 constant K-FRAME
4 constant K-DSLOT
5 constant K-DBYTES
6 constant K-COND
7 constant K-DBACK
8 constant K-ENTRY
9 constant K-OFF
10 constant K-MASK
11 constant K-TRAP-ENTRY               \ the trap form's target, under a key of its own
12 constant K-FUN                      \ which function of the emission an address form names
13 constant K-ADDR                     \ the relocation kind of the value a move-wide chain builds

0 constant BOUND-NO
1 constant BOUND-YES

\ A name is copied out of the old module's interner and interned into the new
\ one, because the two modules number their symbols separately.
128 constant NAME-CAP

\ Values in one function, and operations in one block. Both are the ceilings the
\ neighbouring passes keep, for the same reason.
NFROZEN:VMAX constant VMAX
1024 constant OPS-MAX

here CELL 1- and CELL swap - CELL 1- and allot
variable BND-MODE
BOUND-NO BND-MODE !
variable N-FUSED                     \ pairs this rewrite folded, counted as it goes

1 TYPED-BUFFER BND-MOD IR-ID:ir-module-id
OPCODES-N TYPED-BUFFER BND-OP IR-ID:ir-symbol-id
KEYS-N TYPED-BUFFER BND-KEY IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-GPR IR-ID:ir-type-id
1 TYPED-BUFFER BND-MEM IR-ID:ir-type-id
1 TYPED-BUFFER BND-FPR IR-ID:ir-type-id

1 TYPED-BUFFER S-CTX IR-CTX:ctx
1 TYPED-BUFFER S-BLD IR-BUILD:builder
1 TYPED-BUFFER S-SID IR-ID:ir-source-id
VMAX TYPED-BUFFER VMAP IR-ID:ir-value-id
create VSET VMAX cells allot
create NAMEBUF NAME-CAP allot

\ One cell per operation: which operation of this block folds into the one at
\ this position, or -1. A multiply named here is not copied.
create FOLD-AT OPS-MAX cells allot
create FOLDED OPS-MAX cells allot
create IMM-AT OPS-MAX cells allot
create MASK-AT OPS-MAX cells allot
create CMP-AT OPS-MAX cells allot

\ ---- the slots, read back ----------------------------------------------------
: CTX ( -- IR-CTX:ctx )              0 S-CTX @ ;
: BLD ( -- IR-BUILD:builder )        0 S-BLD @ ;
: SID ( -- IR-ID:ir-source-id )      0 S-SID @ ;
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
      E-A64SPILL-OPCODE throw
   endcase ;

\ An operation of a form outside the family has no rule here and is refused
\ rather than copied blind.
: OPCODE-SLOT ( IR-ID:ir-symbol-id -- n )
   {: sym:IR-ID:ir-symbol-id :}
   -1
   OPCODES-N 0 ?do
      sym i BND-OP @ SAME-SYM? if drop i leave then
   loop
   dup 0 < if E-A64COMB-OPCODE throw then ;

\ Which declared key this symbol is. A frozen module carries no attribute under a
\ key its opcode's schema did not declare - the freeze verifier decides that - so
\ this refusal is fail-closed rather than reachable.
\ A frozen module carries no attribute under a key its schema did not declare,
\ so this refusal is fail-closed rather than reachable.
: KEY-SLOT-OF ( IR-ID:ir-symbol-id -- n )
   {: sym:IR-ID:ir-symbol-id :}
   -1
   KEYS-N 0 ?do
      sym i BND-KEY @ SAME-SYM? if drop i leave then
   loop
   dup 0 < if E-A64COMB-OPCODE throw then ;

: OP-SLOT ( IR-ID:ir-op-id -- n )
   OPCODE-AT OPCODE-SLOT ;

\ ---- the value map -----------------------------------------------------------
\ A folded producer binds NOTHING here, so a reader this pass failed to account
\ for reaches an unset slot and the rewrite is refused.
: VCLEAR ( -- )
   VMAX 0 ?do
      0 i cells VSET + !
   loop ;

: VSLOT ( IR-ID:ir-value-id -- n )
   IR-ID:VALUE-LOCAL
   dup 0 < over VMAX >= or if E-A64COMB-CAP throw then ;

: VBIND ( IR-ID:ir-value-id IR-ID:ir-value-id -- )
   {: src:IR-ID:ir-value-id new:IR-ID:ir-value-id :}
   src VSLOT {: k:n :}
   new k VMAP !
   1 k cells VSET + ! ;

: VOF ( IR-ID:ir-value-id -- IR-ID:ir-value-id )
   VSLOT {: k:n :}
   k cells VSET + @ 0= if E-A64COMB-SHAPE throw then
   k VMAP @ ;

\ ---- reading the frozen module -----------------------------------------------
: SRC-CK ( IR-ID:ir-source-id -- )
   IR-ID:SOURCE-LOCAL 0<> if E-A64COMB-SHAPE throw then ;

: OP-SPAN ( IR-ID:ir-op-id -- IR-SOURCE:span )
   {: id:IR-ID:ir-op-id :}
   id SPAN-AT IR--SOURCE-SPAN:UNMAKE
   {: src:IR-ID:ir-source-id st:n ln:n :}
   src SRC-CK
   BLD SID st ln IR-BUILD:ADD-SPAN ;

: FUN-SPAN ( IR-ID:ir-fun-id -- IR-SOURCE:span )
   {: f:IR-ID:ir-fun-id :}
   V-FUNR VW MKEY f IR-FUN:FSPAN@ IR--SOURCE-SPAN:UNMAKE
   {: src:IR-ID:ir-source-id st:n ln:n :}
   src SRC-CK
   BLD SID st ln IR-BUILD:ADD-SPAN ;

: BLOCK-SPAN ( IR-ID:ir-block-id -- IR-SOURCE:span )
   {: bk:IR-ID:ir-block-id :}
   V-BLKR VW MKEY bk IR-FUN:FBLOCK-SPAN@ IR--SOURCE-SPAN:UNMAKE
   {: src:IR-ID:ir-source-id st:n ln:n :}
   src SRC-CK
   BLD SID st ln IR-BUILD:ADD-SPAN ;

\ The two modules number their types separately, so a value's class is carried
\ across by identity and not by ordinal.
: TYPE-OF ( IR-ID:ir-value-id -- IR-ID:ir-type-id )
   {: id:IR-ID:ir-value-id :}
   id VALUE-TYPE-AT {: t:IR-ID:ir-type-id :}
   t 0 BND-GPR @ SAME-TYPE? if CTX BLD A64IR:GPR-TYPE exit then
   t 0 BND-FPR @ SAME-TYPE? if CTX BLD A64IR:FPR-TYPE exit then
   t 0 BND-MEM @ SAME-TYPE? if CTX BLD A64IR:MEM-TYPE exit then
   E-A64COMB-SHAPE throw ;

\ ---- how many operands of the function name a value --------------------------
\ Counted per OPERAND and not per operation: an addition whose two operands are
\ one value uses it twice, and a count saying once would fold a live product.
: USES-IN-OP ( IR-ID:ir-value-id IR-ID:ir-op-id -- n )
   {: v:IR-ID:ir-value-id id:IR-ID:ir-op-id :}
   0
   id OPERANDS-OF 0 ?do
      id i OPERAND-AT v SAME-VALUE? if 1+ then
   loop ;

: USES-IN-BLOCK ( IR-ID:ir-value-id IR-ID:ir-block-id -- n )
   {: v:IR-ID:ir-value-id bk:IR-ID:ir-block-id :}
   0
   bk OP-COUNT 0 ?do
      v  bk i OP-AT  USES-IN-OP  +
   loop ;

: USES-OF ( IR-ID:ir-fun-id IR-ID:ir-value-id -- n )
   {: f:IR-ID:ir-fun-id v:IR-ID:ir-value-id :}
   0
   f BLOCK-COUNT 0 ?do
      v  f i BLOCK-AT  USES-IN-BLOCK  +
   loop ;

\ ---- which pairs this block folds --------------------------------------------
\ Only a definition IN THIS BLOCK may be folded, because the combined form
\ stands where the reader stands.
: DEF-INDEX ( IR-ID:ir-block-id IR-ID:ir-value-id -- n )
   {: bk:IR-ID:ir-block-id v:IR-ID:ir-value-id :}
   -1
   bk OP-COUNT 0 ?do
      bk i OP-AT {: id:IR-ID:ir-op-id :}
      id RESULTS-OF 1 = if
         id 0 RESULT-AT v SAME-VALUE? if drop i leave then
      then
   loop ;

\ A multiply defining one value, that value read by exactly one operand of the
\ whole function.
: FOLDABLE-MUL? ( IR-ID:ir-fun-id IR-ID:ir-block-id n -- bool )
   {: f:IR-ID:ir-fun-id bk:IR-ID:ir-block-id k:n :}
   k 0 < if false exit then
   bk k OP-AT {: id:IR-ID:ir-op-id :}
   id OP-SLOT O-MUL <> if false exit then
   id RESULTS-OF 1 <> if false exit then
   f  id 0 RESULT-AT  USES-OF 1 = ;

\ The combined form is written where the ADDITION stands, so a multiply below it
\ would be a computation moved backwards past its own inputs.
: FOLDS-HERE? ( IR-ID:ir-fun-id IR-ID:ir-block-id n n -- bool )
   {: f:IR-ID:ir-fun-id bk:IR-ID:ir-block-id d:n k:n :}
   d k >= if false exit then
   f bk d FOLDABLE-MUL? ;

\ A multiply behind EITHER operand will do, and the first asked wins, so
\ `x*y + x*y` could never be read as folding both.
: FOLD-FOR ( IR-ID:ir-fun-id IR-ID:ir-block-id n -- n )
   {: f:IR-ID:ir-fun-id bk:IR-ID:ir-block-id k:n :}
   bk k OP-AT {: id:IR-ID:ir-op-id :}
   id OP-SLOT O-ADD <> if -1 exit then
   id OPERANDS-OF 2 <> if -1 exit then
   bk  id 0 OPERAND-AT  DEF-INDEX {: d0:n :}
   f bk d0 k FOLDS-HERE? if d0 exit then
   bk  id 1 OPERAND-AT  DEF-INDEX {: d1:n :}
   f bk d1 k FOLDS-HERE? if d1 exit then
   -1 ;

\ ---- which constants this block puts in the instruction ------------------------
\ ARM64's add and subtract carry a small number in the instruction, and the
\ selector always picks the register form, so the constant costs a move-wide.
\ Addition is commutative and `sub` is not: `x - 5` folds and `5 - x` does not.
: ATTR-BY-KEY ( IR-ID:ir-op-id n -- n )
   {: id:IR-ID:ir-op-id want:n :}
   -1
   id ATTRS-OF 0 ?do
      id i ATTR-KEY-AT KEY-SLOT-OF want = if drop id i ATTR-INT-AT leave then
   loop ;

\ A movz writing the bottom half of a cleared register IS the number; a movz
\ under a movk is one half of a larger one, which the single-use test excludes.
: MOVZ-VALUE ( IR-ID:ir-op-id -- n bool )
   {: id:IR-ID:ir-op-id :}
   id OP-SLOT O-MOVZ <> if 0 false exit then
   id RESULTS-OF 1 <> if 0 false exit then
   id K-SHIFT ATTR-BY-KEY 0<> if 0 false exit then
   id K-IMM ATTR-BY-KEY {: v:n :}
   v 0 < if 0 false exit then
   v true ;

\ The number this operation puts in a register, when it is a whole constant that
\ the arithmetic field can hold, and -1 when it is anything else.
: WHOLE-IMM ( IR-ID:ir-op-id -- n )
   MOVZ-VALUE {: v:n ok:bool :}
   ok 0= if -1 exit then
   v A64IR:OFF-LIMIT > if -1 exit then
   v ;

\ The logical field is not bounded by a width but by whether its thirteen-bit
\ description can rebuild the mask, which only the packer can answer.
: WHOLE-MASK? ( IR-ID:ir-op-id -- bool )
   MOVZ-VALUE {: v:n ok:bool :}
   ok 0= if false exit then
   v A64IR:MASK-IMM? ;

: WHOLE-MASK ( IR-ID:ir-op-id -- n )
   MOVZ-VALUE {: v:n ok:bool :}
   ok 0= if E-A64COMB-SHAPE throw then
   v ;

\ Whether the operation at this position is a constant this pass may fold into
\ the one reading it.
: FOLDABLE-IMM? ( IR-ID:ir-fun-id IR-ID:ir-block-id n -- bool )
   {: f:IR-ID:ir-fun-id bk:IR-ID:ir-block-id k:n :}
   k 0 < if false exit then
   bk k OP-AT {: id:IR-ID:ir-op-id :}
   id WHOLE-IMM 0 < if false exit then
   f  id 0 RESULT-AT  USES-OF 1 = ;

: IMM-FOLDS-HERE? ( IR-ID:ir-fun-id IR-ID:ir-block-id n n -- bool )
   {: f:IR-ID:ir-fun-id bk:IR-ID:ir-block-id d:n k:n :}
   d k >= if false exit then
   f bk d FOLDABLE-IMM? ;

\ An addition is asked of both operands and a subtraction only of its second. A
\ pair the multiply-add already claimed is left alone.
: IMM-FOLD-FOR ( IR-ID:ir-fun-id IR-ID:ir-block-id n -- n )
   {: f:IR-ID:ir-fun-id bk:IR-ID:ir-block-id k:n :}
   f bk k FOLD-FOR 0 >= if -1 exit then
   bk k OP-AT {: id:IR-ID:ir-op-id :}
   id OP-SLOT O-ADD <>  id OP-SLOT O-SUB <>  and if -1 exit then
   id OPERANDS-OF 2 <> if -1 exit then
   id RESULTS-OF 1 <> if -1 exit then
   id OP-SLOT O-ADD = if
      bk  id 0 OPERAND-AT  DEF-INDEX {: d0:n :}
      f bk d0 k IMM-FOLDS-HERE? if d0 exit then
   then
   bk  id 1 OPERAND-AT  DEF-INDEX {: d1:n :}
   f bk d1 k IMM-FOLDS-HERE? if d1 exit then
   -1 ;

\ ---- which masks this block puts in the instruction ---------------------------
\ and, orr and eor are all commutative, so either operand may be the mask.
\ No operation is a candidate for two folds, so no movz can be claimed twice.
: FOLDABLE-MASK? ( IR-ID:ir-fun-id IR-ID:ir-block-id n -- bool )
   {: f:IR-ID:ir-fun-id bk:IR-ID:ir-block-id k:n :}
   k 0 < if false exit then
   bk k OP-AT {: id:IR-ID:ir-op-id :}
   id WHOLE-MASK? 0= if false exit then
   f  id 0 RESULT-AT  USES-OF 1 = ;

: MASK-FOLDS-HERE? ( IR-ID:ir-fun-id IR-ID:ir-block-id n n -- bool )
   {: f:IR-ID:ir-fun-id bk:IR-ID:ir-block-id d:n k:n :}
   d k >= if false exit then
   f bk d FOLDABLE-MASK? ;

: LOGICAL-OP? ( IR-ID:ir-op-id -- bool )
   {: id:IR-ID:ir-op-id :}
   id OP-SLOT O-AND =  id OP-SLOT O-ORR =  or  id OP-SLOT O-EOR =  or ;

: MASK-FOLD-FOR ( IR-ID:ir-fun-id IR-ID:ir-block-id n -- n )
   {: f:IR-ID:ir-fun-id bk:IR-ID:ir-block-id k:n :}
   bk k OP-AT {: id:IR-ID:ir-op-id :}
   id LOGICAL-OP? 0= if -1 exit then
   id OPERANDS-OF 2 <> if -1 exit then
   id RESULTS-OF 1 <> if -1 exit then
   bk  id 0 OPERAND-AT  DEF-INDEX {: d0:n :}
   f bk d0 k MASK-FOLDS-HERE? if d0 exit then
   bk  id 1 OPERAND-AT  DEF-INDEX {: d1:n :}
   f bk d1 k MASK-FOLDS-HERE? if d1 exit then
   -1 ;

\ ---- which constants this block compares against without a register -----------
\ This dialect has no standalone compare: one reaches the machine only fused, as
\ the flag it materialises or as the branch that reads it, so both are folded.
\ Only the SECOND operand may be: `cmp rn, #imm` sets flags from rn minus imm,
\ and turning a left-hand constant round means changing the condition too.
: COMPARE-OP? ( IR-ID:ir-op-id -- bool )
   {: id:IR-ID:ir-op-id :}
   id OP-SLOT O-FLAG =  id OP-SLOT O-CMPBR =  or ;

: CMP-FOLD-FOR ( IR-ID:ir-fun-id IR-ID:ir-block-id n -- n )
   {: f:IR-ID:ir-fun-id bk:IR-ID:ir-block-id k:n :}
   bk k OP-AT {: id:IR-ID:ir-op-id :}
   id COMPARE-OP? 0= if -1 exit then
   id OPERANDS-OF 2 <> if -1 exit then
   bk  id 1 OPERAND-AT  DEF-INDEX {: d1:n :}
   f bk d1 k IMM-FOLDS-HERE? if d1 exit then
   -1 ;

\ The whole block's plan, read once before a single operation of it is copied,
\ so the walk and the operation it reaches later agree about what was decided.
: PLAN-BLOCK ( IR-ID:ir-fun-id IR-ID:ir-block-id -- )
   {: f:IR-ID:ir-fun-id bk:IR-ID:ir-block-id :}
   bk OP-COUNT {: n:n :}
   n OPS-MAX > if E-A64COMB-CAP throw then
   n 0 ?do
      -1 i cells FOLD-AT + !
      -1 i cells IMM-AT + !
      -1 i cells MASK-AT + !
      -1 i cells CMP-AT + !
      0 i cells FOLDED + !
   loop
   n 0 ?do
      f bk i FOLD-FOR {: d:n :}
      d 0 >= if
         d i cells FOLD-AT + !
         1 d cells FOLDED + !
      then
   loop
   n 0 ?do
      f bk i IMM-FOLD-FOR {: d:n :}
      d 0 >= if
         d i cells IMM-AT + !
         1 d cells FOLDED + !
      then
   loop
   n 0 ?do
      f bk i MASK-FOLD-FOR {: d:n :}
      d 0 >= if
         d i cells MASK-AT + !
         1 d cells FOLDED + !
      then
   loop
   n 0 ?do
      f bk i CMP-FOLD-FOR {: d:n :}
      d 0 >= if
         d i cells CMP-AT + !
         1 d cells FOLDED + !
      then
   loop ;

: FOLD-OF ( n -- n )
   cells FOLD-AT + @ ;

: IMM-OF ( n -- n )
   cells IMM-AT + @ ;

: MASK-OF ( n -- n )
   cells MASK-AT + @ ;

: CMP-OF ( n -- n )
   cells CMP-AT + @ ;

: FOLDED? ( n -- bool )
   cells FOLDED + @ 0<> ;

\ ---- staging one operation in the new module ---------------------------------
: OPEN ( IR-ID:ir-op-id A64IR:opcode -- )
   {: id:IR-ID:ir-op-id o:A64IR:opcode :}
   CTX BLD  CTX BLD o A64IR:OPCODE  IR-BUILD:BEGIN-OP
   CTX BLD  id OP-SPAN  IR-BUILD:SET-OP-SPAN ;

: OPERAND+ ( IR-ID:ir-value-id -- )
   CTX BLD rot IR-BUILD:ADD-OPERAND ;

: CLOSE ( -- IR-ID:ir-op-id )
   CTX BLD IR-BUILD:END-OP ;

: RESULT@ ( IR-ID:ir-op-id n -- IR-ID:ir-value-id )
   {: id:IR-ID:ir-op-id i:n :}
   CTX BLD id i IR-BUILD:OP-RESULT@ ;

\ ---- copying one operation of the old block ----------------------------------
: COPY-ATTRS ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id ATTRS-OF {: n:n :}
   n 0 ?do
      id i ATTR-KEY-AT KEY-SLOT-OF {: k:n :}
      id i ATTR-INT-AT {: v:n :}
      k K-IMM = if
         CTX BLD  CTX BLD A64IR:KEY-IMM  CTX BLD v A64IR:IMM-ATTR
         IR-BUILD:ADD-ATTR
      then
      k K-SHIFT = if
         CTX BLD  CTX BLD A64IR:KEY-SHIFT  CTX BLD v A64IR:SHIFT-ATTR
         IR-BUILD:ADD-ATTR
      then
      k K-ADDR = if
         CTX BLD  CTX BLD A64IR:KEY-ADDR  CTX BLD v A64IR:ADDR-ATTR
         IR-BUILD:ADD-ATTR
      then
      k K-SLOT = if
         CTX BLD  CTX BLD A64IR:KEY-SLOT  CTX BLD v A64IR:SLOT-ATTR
         IR-BUILD:ADD-ATTR
      then
      k K-FRAME = if
         CTX BLD  CTX BLD A64IR:KEY-FRAME  CTX BLD v A64IR:FRAME-ATTR
         IR-BUILD:ADD-ATTR
      then
      k K-DSLOT = if
         CTX BLD  CTX BLD A64IR:KEY-DSLOT  CTX BLD v A64IR:DSLOT-ATTR
         IR-BUILD:ADD-ATTR
      then
      k K-DBYTES = if
         CTX BLD  CTX BLD A64IR:KEY-DBYTES  CTX BLD v A64IR:DBYTES-ATTR
         IR-BUILD:ADD-ATTR
      then
      k K-COND = if
         CTX BLD  CTX BLD A64IR:KEY-COND  CTX BLD v A64IR:N>COND A64IR:COND-ATTR
         IR-BUILD:ADD-ATTR
      then
      k K-DBACK = if
         CTX BLD  CTX BLD A64IR:KEY-DBACK  CTX BLD v A64IR:DBACK-ATTR
         IR-BUILD:ADD-ATTR
      then
      k K-ENTRY = if
         CTX BLD  CTX BLD A64IR:KEY-ENTRY  CTX BLD v A64IR:ENTRY-ATTR
         IR-BUILD:ADD-ATTR
      then
      k K-OFF = if
         CTX BLD  CTX BLD A64IR:KEY-OFF  CTX BLD v A64IR:OFF-ATTR
         IR-BUILD:ADD-ATTR
      then
      k K-MASK = if
         CTX BLD  CTX BLD A64IR:KEY-MASK  CTX BLD v A64IR:MASK-ATTR
         IR-BUILD:ADD-ATTR
      then
      k K-TRAP-ENTRY = if
         CTX BLD  CTX BLD A64IR:KEY-TRAP-ENTRY  CTX BLD v A64IR:ENTRY-ATTR
         IR-BUILD:ADD-ATTR
      then
      k K-FUN = if
         CTX BLD  CTX BLD A64IR:KEY-FUN  CTX BLD v A64IR:FUN-ATTR
         IR-BUILD:ADD-ATTR
      then
   loop ;

\ Blocks are copied one for one and in order, so a successor is carried across
\ by its ordinal.
: COPY-SUCCS ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id SUCCS-OF {: n:n :}
   n 0 ?do
      CTX BLD
      BLD IR-BUILD:MODULE-KEY  id i SUCC-AT IR-ID:BLOCK-LOCAL  IR-ID:PACK-BLOCK
      IR-BUILD:ADD-SUCCESSOR
   loop ;

: COPY-OPERANDS ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id OPERANDS-OF {: n:n :}
   n 0 ?do
      id i OPERAND-AT VOF OPERAND+
   loop ;

: COPY-RESULTS ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id RESULTS-OF {: n:n :}
   n 0 ?do
      CTX BLD  id i RESULT-AT TYPE-OF  IR-BUILD:ADD-RESULT
   loop ;

: BIND-RESULTS ( IR-ID:ir-op-id IR-ID:ir-op-id -- )
   {: old:IR-ID:ir-op-id new:IR-ID:ir-op-id :}
   old RESULTS-OF {: n:n :}
   n 0 ?do
      old i RESULT-AT  new i RESULT@  VBIND
   loop ;

: COPY-OP ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id OP-SLOT SLOT-OPCODE {: o:A64IR:opcode :}
   id o OPEN
   id COPY-OPERANDS
   id COPY-RESULTS
   id COPY-SUCCS
   id COPY-ATTRS
   id  CLOSE  BIND-RESULTS ;

\ ---- the operation the pair becomes ------------------------------------------
\ Found by identity against the product rather than by position, because either
\ operand of an addition may carry it.
: ADDEND-OF ( IR-ID:ir-op-id IR-ID:ir-value-id -- IR-ID:ir-value-id )
   {: id:IR-ID:ir-op-id prod:IR-ID:ir-value-id :}
   id 0 OPERAND-AT prod SAME-VALUE? if id 1 OPERAND-AT exit then
   id 0 OPERAND-AT ;

\ Operand order is the schema's and `madd rd, rn, rm, ra`'s. The ADDITION's
\ result is what the new operation defines; the product is bound to nothing.
: EMIT-MADD ( IR-ID:ir-op-id IR-ID:ir-op-id -- )
   {: mul:IR-ID:ir-op-id add:IR-ID:ir-op-id :}
   mul 0 RESULT-AT {: prod:IR-ID:ir-value-id :}
   add A64IR-OPCODE:MADD OPEN
   mul 0 OPERAND-AT VOF OPERAND+
   mul 1 OPERAND-AT VOF OPERAND+
   add prod ADDEND-OF VOF OPERAND+
   CTX BLD  add 0 RESULT-AT TYPE-OF  IR-BUILD:ADD-RESULT
   add  CLOSE  BIND-RESULTS
   1 N-FUSED +! ;

\ Reads the operand that is NOT the folded constant and carries the constant in
\ its own attribute. This dialect's immediate is unsigned; a subtract stays one.
: EMIT-ADDI ( IR-ID:ir-op-id IR-ID:ir-op-id -- )
   {: mz:IR-ID:ir-op-id ar:IR-ID:ir-op-id :}
   mz 0 RESULT-AT {: k:IR-ID:ir-value-id :}
   ar OP-SLOT O-SUB =
   if A64IR-OPCODE:SUBI else A64IR-OPCODE:ADDI then {: o:A64IR:opcode :}
   ar o OPEN
   ar k ADDEND-OF VOF OPERAND+
   CTX BLD  ar 0 RESULT-AT TYPE-OF  IR-BUILD:ADD-RESULT
   CTX BLD  CTX BLD A64IR:KEY-OFF  CTX BLD mz WHOLE-IMM A64IR:OFF-ATTR
   IR-BUILD:ADD-ATTR
   ar  CLOSE  BIND-RESULTS
   1 N-FUSED +! ;

\ EMIT-ADDI against the other immediate key: the mask is the same number
\ whichever of the three opcodes reads it.
: EMIT-MASKI ( IR-ID:ir-op-id IR-ID:ir-op-id -- )
   {: mz:IR-ID:ir-op-id lg:IR-ID:ir-op-id :}
   mz 0 RESULT-AT {: k:IR-ID:ir-value-id :}
   lg OP-SLOT O-AND =
   if   A64IR-OPCODE:ANDI
   else lg OP-SLOT O-ORR =
        if A64IR-OPCODE:ORRI else A64IR-OPCODE:EORI then
   then {: o:A64IR:opcode :}
   lg o OPEN
   lg k ADDEND-OF VOF OPERAND+
   CTX BLD  lg 0 RESULT-AT TYPE-OF  IR-BUILD:ADD-RESULT
   CTX BLD  CTX BLD A64IR:KEY-MASK  CTX BLD mz WHOLE-MASK A64IR:MASK-ATTR
   IR-BUILD:ADD-ATTR
   lg  CLOSE  BIND-RESULTS
   1 N-FUSED +! ;

\ A comparison carries its condition under a REQUIRED key, so it is checked
\ rather than defaulted - a defaulted condition answers the wrong relation.
: COND-CODE-OF ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   id K-COND ATTR-BY-KEY {: v:n :}
   v 0 < if E-A64COMB-SHAPE throw then
   v ;

\ Operand 0 is taken by POSITION, because only operand 1 was ever a candidate.
\ The flag form defines a value; the branch form ends the block and carries its
\ successors. A third form is refused rather than written as the flag one.
: EMIT-CMPI ( IR-ID:ir-op-id IR-ID:ir-op-id -- )
   {: mz:IR-ID:ir-op-id cm:IR-ID:ir-op-id :}
   cm OP-SLOT {: s:n :}
   s O-FLAG =  s O-CMPBR =  or 0= if E-A64COMB-SHAPE throw then
   s O-CMPBR =
   if A64IR-OPCODE:CMPBRI else A64IR-OPCODE:FLAGI then {: o:A64IR:opcode :}
   cm o OPEN
   cm 0 OPERAND-AT VOF OPERAND+
   s O-FLAG = if
      CTX BLD  cm 0 RESULT-AT TYPE-OF  IR-BUILD:ADD-RESULT
   then
   cm COPY-SUCCS
   CTX BLD  CTX BLD A64IR:KEY-COND
   CTX BLD  cm COND-CODE-OF A64IR:N>COND A64IR:COND-ATTR  IR-BUILD:ADD-ATTR
   CTX BLD  CTX BLD A64IR:KEY-OFF  CTX BLD mz WHOLE-IMM A64IR:OFF-ATTR
   IR-BUILD:ADD-ATTR
   cm  CLOSE  BIND-RESULTS
   1 N-FUSED +! ;

\ ---- the block ---------------------------------------------------------------
\ The value map is NOT cleared here: a value defined in one block is read in the
\ blocks it dominates, so the map belongs to the function.
: OPEN-BLOCK ( IR-ID:ir-block-id -- )
   {: bk:IR-ID:ir-block-id :}
   CTX BLD IR-BUILD:BEGIN-BLOCK
   CTX BLD bk BLOCK-SPAN IR-BUILD:SET-BLOCK-SPAN
   bk ARG-COUNT {: n:n :}
   n 0 ?do
      bk i ARG-AT {: a:IR-ID:ir-value-id :}
      a
      CTX BLD  a TYPE-OF  IR-BUILD:ADD-BLOCK-ARG
      VBIND
   loop ;

\ One block: every operation in order, except that a folded multiply is not
\ written at all and the addition that folded it is written as the multiply-add.
: WALK-BLOCK ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id b:n :}
   f b BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk OP-COUNT {: n:n :}
   n 1 < if E-A64COMB-SHAPE throw then
   f bk PLAN-BLOCK
   bk OPEN-BLOCK
   n 0 ?do
      i FOLDED? 0= if
         i FOLD-OF {: d:n :}
         i IMM-OF {: e:n :}
         i MASK-OF {: g:n :}
         i CMP-OF {: h:n :}
         d 0 >= if bk d OP-AT  bk i OP-AT  EMIT-MADD else
         e 0 >= if bk e OP-AT  bk i OP-AT  EMIT-ADDI else
         g 0 >= if bk g OP-AT  bk i OP-AT  EMIT-MASKI else
         h 0 >= if bk h OP-AT  bk i OP-AT  EMIT-CMPI else
                   bk i OP-AT COPY-OP
         then then then then
      then
   loop
   CTX BLD IR-BUILD:END-BLOCK drop ;

: FUN-NAME ( IR-ID:ir-fun-id -- IR-ID:ir-symbol-id )
   {: f:IR-ID:ir-fun-id :}
   V-SYMP VW V-SYMR VW  V-FUNR VW MKEY f IR-FUN:FSYMBOL@  NAMEBUF NAME-CAP
   IR-SYM:FCOPY {: u:n :}
   CTX BLD NAMEBUF u IR-BUILD:INTERN-SYMBOL ;

\ The routine's signature, restated in the new module: one virtual register per
\ input and one per output, exactly as the old module has them.
: FUN-SIG ( IR-ID:ir-fun-id -- IR-ID:ir-type-id )
   {: f:IR-ID:ir-fun-id :}
   V-TYPR VW  V-FUNR VW MKEY f IR-FUN:FSIGNATURE@  IR-TYPE:FARITY@
   {: in:n out:n :}
   CTX BLD A64IR:GPR-TYPE {: t:IR-ID:ir-type-id :}
   IR-TYPE:FN-BEGIN
   in 0 ?do t IR-TYPE:FN-PARAM loop
   out 0 ?do t IR-TYPE:FN-RESULT loop
   CTX BLD IR-BUILD:INTERN-CODE-REF ;

: WALK-FUN ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   CTX BLD f FUN-NAME IR-BUILD:BEGIN-FUN
   CTX BLD f FUN-SIG IR-BUILD:SET-SIGNATURE
   CTX BLD  V-FUNR VW f IR-FUN:FLINKAGE@  IR-BUILD:SET-LINKAGE
   CTX BLD  V-FUNR VW f IR-FUN:FVISIBILITY@  IR-BUILD:SET-VISIBILITY
   CTX BLD  V-FUNR VW f IR-FUN:FCONVENTION@  IR-BUILD:SET-CONVENTION
   CTX BLD f FUN-SPAN IR-BUILD:SET-FUN-SPAN
   VCLEAR
   f BLOCK-COUNT 0 ?do f i WALK-BLOCK loop
   CTX BLD IR-BUILD:END-FUN drop ;

\ ---- what one rewrite is told ------------------------------------------------
: SOURCE! ( IR-CTX:ctx IR-BUILD:builder ptr u8 n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder p u:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   V-SRC VW IR-SOURCE:FSOURCES 1 <> if E-A64COMB-SHAPE throw then
   V-SRC VW  MKEY 0 IR-ID:PACK-SOURCE  IR-SOURCE:FDIGEST@
   p u CDIGEST:COMPUTE
   CDIGEST-DIGEST:EQ 0= if E-A64COMB-SOURCE throw then
   c b p u IR-BUILD:ADD-SOURCE 0 S-SID ! ;

\ The binding is taken whatever the outcome, so neither a rewrite without a
\ binding nor a refused rewrite can leave one behind for the next caller.
: BND-TAKE ( -- )
   BND-MODE @ {: have:n :}
   BOUND-NO BND-MODE !
   have BOUND-YES <> if E-A64COMB-BIND throw then ;

: BND-MODULE-CK ( IR-BUILD:module -- )
   IR-BUILD:FMODULE  0 BND-MOD @  IR-ID:MODULE-SAME?
   0= if E-A64COMB-BIND throw then ;

: BIND1 ( IR-CTX:ctx IR-BUILD:builder A64IR:opcode -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder o:A64IR:opcode :}
   c b o A64IR:OPCODE  o SLOT-OF BND-OP ! ;

: DIALECT-CK ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b  c b IR-BUILD:DIALECT@  A64IR:NAME IR-BUILD:SYMBOL-IS?
   0= if E-A64COMB-BIND throw then
   c b IR-BUILD:SCHEMA-MAJOR@ A64IR:MAJOR <> if E-A64COMB-BIND throw then
   c b IR-BUILD:SCHEMA-MINOR@ A64IR:MINOR <> if E-A64COMB-BIND throw then ;

public

\ ---- binding the dialect -----------------------------------------------------
\ The only moment a module can be asked its operation, key and type identities,
\ because its symbols and types are its own ordinals.
: BIND-DIALECT ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   BND-MODE @ BOUND-YES = if E-A64COMB-BIND throw then
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
   c b A64IR:KEY-IMM    K-IMM BND-KEY !
   c b A64IR:KEY-SHIFT  K-SHIFT BND-KEY !
   c b A64IR:KEY-ADDR   K-ADDR  BND-KEY !
   c b A64IR:KEY-SLOT   K-SLOT BND-KEY !
   c b A64IR:KEY-FRAME  K-FRAME BND-KEY !
   c b A64IR:KEY-DSLOT  K-DSLOT BND-KEY !
   c b A64IR:KEY-DBYTES K-DBYTES BND-KEY !
   c b A64IR:KEY-COND   K-COND BND-KEY !
   c b A64IR:KEY-DBACK  K-DBACK BND-KEY !
   c b A64IR:KEY-ENTRY  K-ENTRY BND-KEY !
   c b A64IR:KEY-OFF    K-OFF BND-KEY !
   c b A64IR:KEY-MASK   K-MASK BND-KEY !
   c b A64IR:KEY-TRAP-ENTRY K-TRAP-ENTRY BND-KEY !
   c b A64IR:KEY-FUN    K-FUN BND-KEY !
   c b A64IR:GPR-TYPE 0 BND-GPR !
   c b A64IR:MEM-TYPE 0 BND-MEM !
   c b A64IR:FPR-TYPE 0 BND-FPR !
   BOUND-YES BND-MODE ! ;

: BOUND? ( -- bool )
   BND-MODE @ BOUND-YES = ;

\ Give up a binding without rewriting against it: what a caller does when the
\ scan below finds no pair, and what one does when a later stage refuses.
: RELEASE ( -- )
   BND-TAKE ;

\ ---- what the module holds ---------------------------------------------------
\ Asked before anything is built. A caller that gets zero keeps the module it
\ has, which is what keeps every routine without the pattern byte-for-byte.
: FUSIONS ( IR-BUILD:module -- n )
   {: m:IR-BUILD:module :}
   BOUND? 0= if E-A64COMB-BIND throw then
   m BND-MODULE-CK
   m VIEWS!
   0
   FUN-COUNT 0 ?do
      MKEY i IR-ID:PACK-FUN {: f:IR-ID:ir-fun-id :}
      f BLOCK-COUNT 0 ?do
         f i BLOCK-AT {: bk:IR-ID:ir-block-id :}
         bk OP-COUNT 0 ?do
            f bk i FOLD-FOR 0 >= if 1+ then
            f bk i IMM-FOLD-FOR 0 >= if 1+ then
            f bk i MASK-FOLD-FOR 0 >= if 1+ then
            f bk i CMP-FOLD-FOR 0 >= if 1+ then
         loop
      loop
   loop ;

\ ---- the pass ----------------------------------------------------------------
\ The bytes are the source text the old module was compiled from, proved by
\ digest before any span is carried across.
: REWRITE ( IR-CTX:ctx IR-BUILD:module IR-BUILD:builder ptr u8 n -- IR-BUILD:module )
   {: c:IR-CTX:ctx m:IR-BUILD:module b:IR-BUILD:builder p u:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   BND-TAKE
   m BND-MODULE-CK
   0 N-FUSED !
   c b A64IR:REGISTER
   c 0 S-CTX !
   b 0 S-BLD !
   m VIEWS!
   c b p u SOURCE!
   FUN-COUNT 0 ?do MKEY i IR-ID:PACK-FUN WALK-FUN loop
   c b IR-BUILD:FREEZE ;

\ A caller compares it with what the scan promised, so a walk that folded a
\ different number is a refusal rather than a module nobody checked.
: FUSED ( -- n )
   N-FUSED @ ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;using
;package

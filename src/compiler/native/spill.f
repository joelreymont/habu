\ spill.f - build the machine module in which the register allocator's spill
\ decisions are real store and load operations.
\
\ A frozen module cannot gain an operation and a builder cannot gain one in the
\ middle, so this reads a frozen module and writes a new one. The alternative -
\ the emitter materialising the stores out of the allocator's claims - would
\ leave the validator checking the allocator's belief against itself.
\
\ A routine that CALLS arrives with a frame the selector reserved, so it gets no
\ second reserve; a module with neither shape has been through this pass already
\ and is refused. There is one frame per module and it is the first function's.
\
\ The dialect's frame forms carry a memory token, and an operation of the OLD
\ module that reaches the frame is re-threaded onto the order as it stands here.
\ Which operations those are is read off the attribute KEYS the dialect declares,
\ never off an opcode name, so a data-stack access is never threaded onto it.
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
require src/compiler/native/frame.f
require src/compiler/native/frozen.f
require src/compiler/native/regalloc.f

package A64SPILL
using NFROZEN
private

\ One slot per member of the machine operation family, so a member added to
\ A64IR:opcode fails to compile here until it has a slot and a rebuild rule.
\ ---- the bound dialect -------------------------------------------------------
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

\ One slot per attribute key the dialect declares.
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

here CELL 1- and CELL swap - CELL 1- and allot
variable BND-MODE
BOUND-NO BND-MODE !
variable N-CUR                       \ how far through the plan the walk has read
variable FRAME-N
variable G-AT                        \ operations of the whole function copied so far
variable PRO-N                       \ 1 when the module arrived with its own prologue
variable N-RES                       \ frame reserves, releases, link saves and
variable N-REL                       \ link restores the old module already holds
variable N-SAV
variable N-LDL

1 TYPED-BUFFER BND-MOD IR-ID:ir-module-id
OPCODES-N TYPED-BUFFER BND-OP IR-ID:ir-symbol-id
KEYS-N TYPED-BUFFER BND-KEY IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-GPR IR-ID:ir-type-id
1 TYPED-BUFFER BND-MEM IR-ID:ir-type-id
1 TYPED-BUFFER BND-FPR IR-ID:ir-type-id

1 TYPED-BUFFER S-CTX IR-CTX:ctx
1 TYPED-BUFFER S-BLD IR-BUILD:builder
1 TYPED-BUFFER S-SID IR-ID:ir-source-id
1 TYPED-BUFFER S-TOK IR-ID:ir-value-id
VMAX TYPED-BUFFER VMAP IR-ID:ir-value-id
VMAX TYPED-BUFFER RMAP IR-ID:ir-value-id
create VSET VMAX cells allot
\ A value marked for re-emission is written again where it is read, out of its
\ defining operation's own immediate, so the pass has to reach it from the value.
VMAX TYPED-BUFFER DOP IR-ID:ir-op-id
create RPOS VMAX cells allot
create NAMEBUF NAME-CAP allot

\ ---- the slots, read back ----------------------------------------------------
: CTX ( -- IR-CTX:ctx )              0 S-CTX @ ;
: BLD ( -- IR-BUILD:builder )        0 S-BLD @ ;
: SID ( -- IR-ID:ir-source-id )      0 S-SID @ ;
: TOK ( -- IR-ID:ir-value-id )       0 S-TOK @ ;
: TOK! ( IR-ID:ir-value-id -- )      0 S-TOK ! ;

\ ---- the machine operation family --------------------------------------------
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
   dup 0 < if E-A64SPILL-OPCODE throw then ;

\ A frozen module carries no attribute under a key its schema did not declare,
\ so this refusal is fail-closed rather than reachable.
: KEY-SLOT-OF ( IR-ID:ir-symbol-id -- n )
   {: sym:IR-ID:ir-symbol-id :}
   -1
   KEYS-N 0 ?do
      sym i BND-KEY @ SAME-SYM? if drop i leave then
   loop
   dup 0 < if E-A64SPILL-OPCODE throw then ;

\ ---- which operations reach the routine's own frame --------------------------
: FRAME-TOUCH? ( IR-ID:ir-op-id -- bool )
   {: id:IR-ID:ir-op-id :}
   false
   id ATTRS-OF 0 ?do
      id i ATTR-KEY-AT KEY-SLOT-OF {: k:n :}
      k K-SLOT = k K-FRAME = or if drop true leave then
   loop ;

\ Found by TYPE, because that is what tells a memory order apart from the
\ registers beside it.
: MEM-VALUE? ( IR-ID:ir-value-id -- bool )
   VALUE-TYPE-AT 0 BND-MEM @ SAME-TYPE? ;

\ ---- the value map -----------------------------------------------------------
: VCLEAR ( -- )
   VMAX 0 ?do
      0 i cells VSET + !
      -1 i cells RPOS + !
   loop ;

: VSLOT ( IR-ID:ir-value-id -- n )
   IR-ID:VALUE-LOCAL
   dup 0 < over VMAX >= or if E-A64SPILL-CAP throw then ;

: VBIND ( IR-ID:ir-value-id IR-ID:ir-value-id -- )
   {: src:IR-ID:ir-value-id new:IR-ID:ir-value-id :}
   src VSLOT {: k:n :}
   new k VMAP !
   1 k cells VSET + ! ;

: VOF ( IR-ID:ir-value-id -- IR-ID:ir-value-id )
   VSLOT {: k:n :}
   k cells VSET + @ 0= if E-A64SPILL-SHAPE throw then
   k VMAP @ ;

\ The position counts operations of the whole FUNCTION rather than of one block,
\ because the same index inside two blocks would cross their loads.
: RBIND ( n n IR-ID:ir-value-id -- )
   {: k:n pos:n new:IR-ID:ir-value-id :}
   new k RMAP !
   pos k cells RPOS + ! ;

: READ-AS ( IR-ID:ir-value-id n -- IR-ID:ir-value-id )
   {: id:IR-ID:ir-value-id pos:n :}
   id VSLOT {: k:n :}
   k cells RPOS + @ pos = if k RMAP @ exit then
   id VOF ;

\ ---- reading the frozen module -----------------------------------------------
: SRC-CK ( IR-ID:ir-source-id -- )
   IR-ID:SOURCE-LOCAL 0<> if E-A64SPILL-SHAPE throw then ;

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
   E-A64SPILL-SHAPE throw ;

\ A value put away and brought back has to travel through the file it lives in:
\ the same eight bytes stored by the general form come back in a general register.
: FPR-VALUE? ( IR-ID:ir-value-id -- bool )
   VALUE-TYPE-AT 0 BND-FPR @ SAME-TYPE? ;

: FPR-SLOT? ( n -- bool )
   {: k:n :}
   MKEY k IR-ID:PACK-VALUE FPR-VALUE? ;

\ The general pair is a64.str/a64.ldr and the floating pair a64.fstr/a64.fldr;
\ nothing else about an insert depends on where the eight bytes live.
: STORE-FORM ( n -- A64IR:opcode )
   FPR-SLOT? if A64IR-OPCODE:FSTORE exit then A64IR-OPCODE:STORE ;

: LOAD-FORM ( n -- A64IR:opcode )
   FPR-SLOT? if A64IR-OPCODE:FLOAD exit then A64IR-OPCODE:LOAD ;

\ ---- staging one operation in the new module ---------------------------------
: OPEN ( IR-ID:ir-op-id A64IR:opcode -- )
   {: id:IR-ID:ir-op-id o:A64IR:opcode :}
   CTX BLD  CTX BLD o A64IR:OPCODE  IR-BUILD:BEGIN-OP
   CTX BLD  id OP-SPAN  IR-BUILD:SET-OP-SPAN ;

: OPERAND+ ( IR-ID:ir-value-id -- )
   CTX BLD rot IR-BUILD:ADD-OPERAND ;

: GPR-RESULT+ ( -- )
   CTX BLD  CTX BLD A64IR:GPR-TYPE  IR-BUILD:ADD-RESULT ;

: FPR-RESULT+ ( -- )
   CTX BLD  CTX BLD A64IR:FPR-TYPE  IR-BUILD:ADD-RESULT ;

\ A value comes back into the class of register it left, which is what makes the
\ reload of a double land where the operation below it looks.
: FILE-RESULT+ ( n -- )
   FPR-SLOT? if FPR-RESULT+ exit then GPR-RESULT+ ;

: MEM-RESULT+ ( -- )
   CTX BLD  CTX BLD A64IR:MEM-TYPE  IR-BUILD:ADD-RESULT ;

: SLOT-ATTR+ ( n -- )
   {: off:n :}
   CTX BLD  CTX BLD A64IR:KEY-SLOT  CTX BLD off A64IR:SLOT-ATTR  IR-BUILD:ADD-ATTR ;

: FRAME-ATTR+ ( n -- )
   {: size:n :}
   CTX BLD  CTX BLD A64IR:KEY-FRAME  CTX BLD size A64IR:FRAME-ATTR  IR-BUILD:ADD-ATTR ;

\ This pass introduces no immediate but copies the ones the combine pass built,
\ and one copied under the wrong key would compare against the wrong number.
: OFF-ATTR+ ( n -- )
   {: imm:n :}
   CTX BLD  CTX BLD A64IR:KEY-OFF  CTX BLD imm A64IR:OFF-ATTR  IR-BUILD:ADD-ATTR ;

: MASK-ATTR+ ( n -- )
   {: m:n :}
   CTX BLD  CTX BLD A64IR:KEY-MASK  CTX BLD m A64IR:MASK-ATTR  IR-BUILD:ADD-ATTR ;

\ This pass inserts no data-stack operation but copies the selector's, and a
\ field copied under the wrong key would read arguments out of the frame.
: DSLOT-ATTR+ ( n -- )
   {: off:n :}
   CTX BLD  CTX BLD A64IR:KEY-DSLOT  CTX BLD off A64IR:DSLOT-ATTR  IR-BUILD:ADD-ATTR ;

: DBYTES-ATTR+ ( n -- )
   {: size:n :}
   CTX BLD  CTX BLD A64IR:KEY-DBYTES  CTX BLD size A64IR:DBYTES-ATTR  IR-BUILD:ADD-ATTR ;

\ Copied unchanged: this pass decides nothing about where a call goes.
: ENTRY-ATTR+ ( n -- )
   {: entry:n :}
   CTX BLD  CTX BLD A64IR:KEY-ENTRY  CTX BLD entry A64IR:ENTRY-ATTR
   IR-BUILD:ADD-ATTR ;

\ Under a key of its own, so a reader cannot mistake it for a callee this
\ routine comes back from.
: TRAP-ENTRY-ATTR+ ( n -- )
   {: entry:n :}
   CTX BLD  CTX BLD A64IR:KEY-TRAP-ENTRY  CTX BLD entry A64IR:ENTRY-ATTR
   IR-BUILD:ADD-ATTR ;

\ An ordinal in a module this pass rebuilds function for function and in order,
\ so the number means the same thing on both sides.
: FUN-ATTR+ ( n -- )
   {: k:n :}
   CTX BLD  CTX BLD A64IR:KEY-FUN  CTX BLD k A64IR:FUN-ATTR  IR-BUILD:ADD-ATTR ;

: DBACK-ATTR+ ( n -- )
   {: size:n :}
   CTX BLD  CTX BLD A64IR:KEY-DBACK  CTX BLD size A64IR:DBACK-ATTR  IR-BUILD:ADD-ATTR ;

\ Decoded back into the dialect's vocabulary, so a stored code the dialect has
\ no condition for is refused rather than copied through.
: COND-ATTR+ ( n -- )
   {: v:n :}
   CTX BLD  CTX BLD A64IR:KEY-COND  CTX BLD v A64IR:N>COND A64IR:COND-ATTR
   IR-BUILD:ADD-ATTR ;

: CLOSE ( -- IR-ID:ir-op-id )
   CTX BLD IR-BUILD:END-OP ;

: RESULT@ ( IR-ID:ir-op-id n -- IR-ID:ir-value-id )
   {: id:IR-ID:ir-op-id i:n :}
   CTX BLD id i IR-BUILD:OP-RESULT@ ;

\ Two callers want it: the copier, and the re-emission that rebuilds a move-wide
\ out of the immediate its original carried.
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
      k K-SLOT = if v SLOT-ATTR+ then
      k K-FRAME = if v FRAME-ATTR+ then
      k K-DSLOT = if v DSLOT-ATTR+ then
      k K-DBYTES = if v DBYTES-ATTR+ then
      k K-COND = if v COND-ATTR+ then
      k K-DBACK = if v DBACK-ATTR+ then
      k K-ENTRY = if v ENTRY-ATTR+ then
      k K-OFF = if v OFF-ATTR+ then
      k K-MASK = if v MASK-ATTR+ then
      k K-TRAP-ENTRY = if v TRAP-ENTRY-ATTR+ then
      k K-FUN = if v FUN-ATTR+ then
   loop ;

\ ---- the four operations this pass inserts -----------------------------------
: EMIT-RESERVE ( IR-ID:ir-op-id -- )
   A64IR-OPCODE:RESERVE OPEN
   MEM-RESULT+
   FRAME-N @ FRAME-ATTR+
   CLOSE 0 RESULT@ TOK! ;

: EMIT-RELEASE ( IR-ID:ir-op-id -- )
   A64IR-OPCODE:RELEASE OPEN
   TOK OPERAND+
   FRAME-N @ FRAME-ATTR+
   CLOSE drop ;

\ The value is read here for the last time as a register value.
: EMIT-STORE ( IR-ID:ir-op-id n -- )
   {: at:IR-ID:ir-op-id k:n :}
   at k STORE-FORM OPEN
   MKEY k IR-ID:PACK-VALUE VOF OPERAND+
   TOK OPERAND+
   MEM-RESULT+
   k A64RA:SLOT@ SLOT-ATTR+
   CLOSE 0 RESULT@ TOK! ;

\ A load DEFINES a register rather than reviving one, so the operation below it
\ reads this value and not the old one.
: EMIT-LOAD ( IR-ID:ir-op-id n n -- )
   {: at:IR-ID:ir-op-id k:n pos:n :}
   at k LOAD-FORM OPEN
   TOK OPERAND+
   k FILE-RESULT+
   MEM-RESULT+
   k A64RA:SLOT@ SLOT-ATTR+
   CLOSE {: id:IR-ID:ir-op-id :}
   id 1 RESULT@ TOK!
   k pos  id 0 RESULT@  RBIND ;

\ Reads the value as it stands here, which is the reloaded one when it spent
\ part of its life in a slot. Contracts state only general result placements,
\ so a double is never the subject of a move.
: EMIT-MOVE ( IR-ID:ir-op-id n n -- )
   {: at:IR-ID:ir-op-id k:n pos:n :}
   at A64IR-OPCODE:MOV OPEN
   MKEY k IR-ID:PACK-VALUE pos READ-AS OPERAND+
   GPR-RESULT+
   CLOSE {: id:IR-ID:ir-op-id :}
   k pos  id 0 RESULT@  RBIND ;

\ Names no slot, takes no memory token and answers none: it joins no memory
\ order at all, which is why the allocator may choose it for a frameless class.
: EMIT-REMAT ( IR-ID:ir-op-id n n -- )
   {: at:IR-ID:ir-op-id k:n pos:n :}
   k DOP @ {: d:IR-ID:ir-op-id :}
   at A64IR-OPCODE:MOVZ OPEN
   k FILE-RESULT+
   d COPY-ATTRS
   CLOSE {: id:IR-ID:ir-op-id :}
   k pos  id 0 RESULT@  RBIND ;

\ The plan is already in anchor order, so this reads it with a cursor rather
\ than searching. A cursor that did not reach the end is refused by REWRITE.
: INSERT-ONE ( IR-ID:ir-op-id n n -- )
   {: at:IR-ID:ir-op-id j:n pos:n :}
   j A64RA:PLAN-VALUE@ {: k:n :}
   j A64RA:PLAN-STORE? if at k EMIT-STORE exit then
   j A64RA:PLAN-MOVE? if at k pos EMIT-MOVE exit then
   j A64RA:PLAN-REMAT? if at k pos EMIT-REMAT exit then
   at k pos EMIT-LOAD ;

\ Both the block and the index have to agree; matching the index alone would put
\ one block's store in front of another block's operation of the same number.
\ The function is not carried, which holds only while no function after the
\ first contributes a row (habu-give-each-fn-c1fd7c5a).
: HERE? ( n n -- bool )
   {: b:n at:n :}
   N-CUR @ A64RA:PLAN-N >= if false exit then
   N-CUR @ A64RA:PLAN-BLOCK@ b = if N-CUR @ A64RA:PLAN-POS@ at = else false then ;

: INSERT-AT ( IR-ID:ir-op-id n n n -- )
   {: at:IR-ID:ir-op-id b:n ord:n g:n :}
   begin
      b ord HERE?
   while
      N-CUR @ {: j:n :}
      at j g INSERT-ONE
      j 1+ N-CUR !
   repeat ;

\ ---- copying one operation of the old block ----------------------------------
: COPY-SUCCS ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id SUCCS-OF {: n:n :}
   n 0 ?do
      CTX BLD
      BLD IR-BUILD:MODULE-KEY  id i SUCC-AT IR-ID:BLOCK-LOCAL  IR-ID:PACK-BLOCK
      IR-BUILD:ADD-SUCCESSOR
   loop ;

: COPY-OPERANDS ( IR-ID:ir-op-id n bool -- )
\ An operand that is the frame's own memory order is replaced by the order as it
\ stands HERE, because this pass may have put inserts between two neighbours.
   {: id:IR-ID:ir-op-id pos:n frame:bool :}
   id OPERANDS-OF {: n:n :}
   n 0 ?do
      id i OPERAND-AT {: v:IR-ID:ir-value-id :}
      frame  v MEM-VALUE?  and if
         TOK OPERAND+
      else
         v pos READ-AS OPERAND+
      then
   loop ;

: COPY-RESULTS ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id RESULTS-OF {: n:n :}
   n 0 ?do
      CTX BLD  id i RESULT-AT TYPE-OF  IR-BUILD:ADD-RESULT
   loop ;

: BIND-RESULTS ( IR-ID:ir-op-id IR-ID:ir-op-id bool -- )
\ The order a frame access answers becomes the order this pass threads its own
\ stores and loads onto from here on.
   {: old:IR-ID:ir-op-id new:IR-ID:ir-op-id frame:bool :}
   old RESULTS-OF {: n:n :}
   n 0 ?do
      old i RESULT-AT {: v:IR-ID:ir-value-id :}
      new i RESULT@ {: nv:IR-ID:ir-value-id :}
      v nv VBIND
      old  v VSLOT DOP !
      frame  v MEM-VALUE?  and if nv TOK! then
   loop ;

: COPY-OP ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id pos:n :}
   id OPCODE-AT OPCODE-SLOT SLOT-OPCODE {: o:A64IR:opcode :}
   id FRAME-TOUCH? {: frame:bool :}
   id o OPEN
   id pos frame COPY-OPERANDS
   id COPY-RESULTS
   id COPY-SUCCS
   id COPY-ATTRS
   id  CLOSE  frame BIND-RESULTS ;

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

: FRAMES? ( n -- bool )
\ Only when the plan really needs a slot AND the module did not arrive with a
\ frame of its own; and only in the first function, because there is one frame.
   {: k:n :}
   k 0<> if false exit then
   A64RA:SPILLS 0<> PRO-N @ 0= and ;

: WALK-BLOCK ( IR-ID:ir-fun-id n n n -- )
\ The reserve opens the ENTRY block and the release stands in front of the
\ terminator control leaves through - the only pair passed once, in that order.
   {: f:IR-ID:ir-fun-id k:n b:n rb:n :}
   f b BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk OP-COUNT {: n:n :}
   n 1 < if E-A64SPILL-SHAPE throw then
   bk OPEN-BLOCK
   b 0= k FRAMES? and if bk 0 OP-AT EMIT-RESERVE then
   n 0 ?do
      bk i OP-AT {: id:IR-ID:ir-op-id :}
      id b i G-AT @ INSERT-AT
      i n 1- =  b rb =  and  k FRAMES?  and if id EMIT-RELEASE then
      id G-AT @ COPY-OP
      G-AT @ 1+ G-AT !
   loop
   CTX BLD IR-BUILD:END-BLOCK drop ;

: FUN-NAME ( IR-ID:ir-fun-id -- IR-ID:ir-symbol-id )
   {: f:IR-ID:ir-fun-id :}
   V-SYMP VW V-SYMR VW  V-FUNR VW MKEY f IR-FUN:FSYMBOL@  NAMEBUF NAME-CAP
   IR-SYM:FCOPY {: u:n :}
   CTX BLD NAMEBUF u IR-BUILD:INTERN-SYMBOL ;

: FUN-SIG ( IR-ID:ir-fun-id -- IR-ID:ir-type-id )
\ One virtual register per input and one per output, as the old module has them.
   {: f:IR-ID:ir-fun-id :}
   V-TYPR VW  V-FUNR VW MKEY f IR-FUN:FSIGNATURE@  IR-TYPE:FARITY@
   {: in:n out:n :}
   CTX BLD A64IR:GPR-TYPE {: t:IR-ID:ir-type-id :}
   IR-TYPE:FN-BEGIN
   in 0 ?do t IR-TYPE:FN-PARAM loop
   out 0 ?do t IR-TYPE:FN-RESULT loop
   CTX BLD IR-BUILD:INTERN-CODE-REF ;

-1 constant NO-RET
\ The rule, and why a trap block is not that block, is written once in
\ regalloc.f MB-RET-ORD; this asks it again of this pass's own view.

: RET-ORD ( IR-ID:ir-fun-id -- n )
   {: f:IR-ID:ir-fun-id :}
   NO-RET
   f BLOCK-COUNT 0 ?do
      f i BLOCK-AT TERM-AT {: t:IR-ID:ir-op-id :}
      t SUCCS-OF 0=  t OPCODE-AT OPCODE-SLOT O-TRAP = 0=  and if
         dup NO-RET <> if E-A64SPILL-SHAPE throw then
         drop i
      then
   loop ;

: WALK-FUN ( IR-ID:ir-fun-id n -- )
\ Both are the FUNCTION's: a value is read in the blocks its definition
\ dominates and in no other function, and the counter separates two blocks.
   {: f:IR-ID:ir-fun-id k:n :}
   CTX BLD f FUN-NAME IR-BUILD:BEGIN-FUN
   CTX BLD f FUN-SIG IR-BUILD:SET-SIGNATURE
   CTX BLD  V-FUNR VW f IR-FUN:FLINKAGE@  IR-BUILD:SET-LINKAGE
   CTX BLD  V-FUNR VW f IR-FUN:FVISIBILITY@  IR-BUILD:SET-VISIBILITY
   CTX BLD  V-FUNR VW f IR-FUN:FCONVENTION@  IR-BUILD:SET-CONVENTION
   CTX BLD f FUN-SPAN IR-BUILD:SET-FUN-SPAN
   f RET-ORD {: rb:n :}
   VCLEAR
   0 G-AT !
   f BLOCK-COUNT 0 ?do f k i rb WALK-BLOCK loop
   CTX BLD IR-BUILD:END-FUN drop ;

\ ---- what one rewrite is told ------------------------------------------------
: SOURCE! ( IR-CTX:ctx IR-BUILD:builder ptr u8 n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder p u:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   V-SRC VW IR-SOURCE:FSOURCES 1 <> if E-A64SPILL-SHAPE throw then
   V-SRC VW  MKEY 0 IR-ID:PACK-SOURCE  IR-SOURCE:FDIGEST@
   p u CDIGEST:COMPUTE
   CDIGEST-DIGEST:EQ 0= if E-A64SPILL-SOURCE throw then
   c b p u IR-BUILD:ADD-SOURCE 0 S-SID ! ;

\ The binding is taken whatever the outcome, so neither a rewrite without a
\ binding nor a refused rewrite can leave one behind for the next caller.
: BND-TAKE ( -- )
   BND-MODE @ {: have:n :}
   BOUND-NO BND-MODE !
   have BOUND-YES <> if E-A64SPILL-BIND throw then ;

: BND-MODULE-CK ( IR-BUILD:module -- )
   IR-BUILD:FMODULE  0 BND-MOD @  IR-ID:MODULE-SAME?
   0= if E-A64SPILL-PLAN throw then ;

: PLAN-CK ( IR-BUILD:module -- )
\ Sealed, about this module, and it has to have decided something: a module that
\ needs no spill needs no rewrite.
   {: m:IR-BUILD:module :}
   A64RA:SEALED? 0= if E-A64SPILL-PLAN throw then
   m IR-BUILD:FMODULE A64RA:MODULE@ IR-ID:MODULE-SAME?
   0= if E-A64SPILL-PLAN throw then
   A64RA:PLAN-N 0= if E-A64SPILL-PLAN throw then ;

\ ---- whose frame the module arrives with -------------------------------------
\ Two lowerable shapes, told apart by counting the four frame forms by NAME: none
\ at all, or exactly a selector's prologue with its reserve opening the entry block.
: COUNT-FRAME-OP ( IR-ID:ir-op-id -- )
   OPCODE-AT OPCODE-SLOT {: k:n :}
   k O-RESERVE  = if N-RES @ 1+ N-RES ! then
   k O-RELEASE  = if N-REL @ 1+ N-REL ! then
   k O-LINKSAVE = if N-SAV @ 1+ N-SAV ! then
   k O-LINKLOAD = if N-LDL @ 1+ N-LDL ! then ;

: COUNT-FRAME ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   0 N-RES ! 0 N-REL ! 0 N-SAV ! 0 N-LDL !
   f BLOCK-COUNT 0 ?do
      f i BLOCK-AT {: bk:IR-ID:ir-block-id :}
      bk OP-COUNT 0 ?do bk i OP-AT COUNT-FRAME-OP loop
   loop ;

: NO-FRAME-CK ( -- )
   N-RES @ N-REL @ or  N-SAV @ or  N-LDL @ or
   0<> if E-A64SPILL-SHAPE throw then ;

: PROLOGUE-CK ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   N-RES @ 1 <>  N-REL @ 1 <> or  N-SAV @ 1 <> or  N-LDL @ 1 <> or
   if E-A64SPILL-SHAPE throw then
   f 0 BLOCK-AT 0 OP-AT OPCODE-AT OPCODE-SLOT O-RESERVE <>
   if E-A64SPILL-SHAPE throw then ;

: ONCE-CK ( IR-ID:ir-fun-id n -- )
\ Every function is held to the frame rule, because lowering any of them twice
\ builds a second frame inside the first. PRO-N is the FIRST function's answer.
   {: f:IR-ID:ir-fun-id k:n :}
   f COUNT-FRAME
   N-SAV @ 0= if
      NO-FRAME-CK
      k 0= if 0 PRO-N ! then
      exit
   then
   f PROLOGUE-CK
   k 0= if 1 PRO-N ! then ;

: SHAPE-CK ( -- n )
\ A module with no function is not a routine at all.
   FUN-COUNT {: n:n :}
   n 1 < if E-A64SPILL-SHAPE throw then
   n NFROZEN:FMAX > if E-A64SPILL-CAP throw then
   n 0 ?do MKEY i IR-ID:PACK-FUN i ONCE-CK loop
   n ;

: BIND1 ( IR-CTX:ctx IR-BUILD:builder A64IR:opcode -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder o:A64IR:opcode :}
   c b o A64IR:OPCODE  o SLOT-OF BND-OP ! ;

: DIALECT-CK ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b  c b IR-BUILD:DIALECT@  A64IR:NAME IR-BUILD:SYMBOL-IS?
   0= if E-A64SPILL-PLAN throw then
   c b IR-BUILD:SCHEMA-MAJOR@ A64IR:MAJOR <> if E-A64SPILL-PLAN throw then
   c b IR-BUILD:SCHEMA-MINOR@ A64IR:MINOR <> if E-A64SPILL-PLAN throw then ;

public

\ ---- binding the dialect -----------------------------------------------------
\ The only moment a module can be asked its operation, key and type identities,
\ because its symbols and types are its own ordinals.
: BIND-DIALECT ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   BND-MODE @ BOUND-YES = if E-A64SPILL-BIND throw then
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
   c b A64IR-OPCODE:MADD      BIND1
   c b A64IR-OPCODE:ADDI      BIND1
   c b A64IR-OPCODE:SUBI      BIND1
   c b A64IR-OPCODE:MOVN      BIND1
   c b A64IR-OPCODE:ANDI      BIND1
   c b A64IR-OPCODE:ORRI      BIND1
   c b A64IR-OPCODE:EORI      BIND1
   c b A64IR-OPCODE:TRAP      BIND1
   c b A64IR-OPCODE:CODEADDR  BIND1
   c b A64IR-OPCODE:FLAGI     BIND1
   c b A64IR-OPCODE:CMPBRI    BIND1
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

\ Whether a binding is live, for a caller cleaning up after a refused run. See
\ Each pass answers for itself; this one needs it because whether its binding was
\ spent depends on whether the walk decided a spill.
: BOUND? ( -- bool )
   BND-MODE @ BOUND-YES = ;

\ Give up a binding without rewriting against it.
\ Give up a binding without rewriting against it.
: RELEASE ( -- )
   BND-TAKE ;

\ ---- the pass ----------------------------------------------------------------
\ The bytes are the source text the old module was compiled from, proved by
\ digest before any span is carried across.
: REWRITE ( IR-CTX:ctx IR-BUILD:module IR-BUILD:builder ptr u8 n -- IR-BUILD:module )
   {: c:IR-CTX:ctx m:IR-BUILD:module b:IR-BUILD:builder p u:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   BND-TAKE
   m BND-MODULE-CK
   m PLAN-CK
   A64RA:FRAME FRAME-N !
   0 N-CUR !
   c b A64IR:REGISTER
   c 0 S-CTX !
   b 0 S-BLD !
   m VIEWS!
   c b p u SOURCE!
   SHAPE-CK {: nf:n :}
   nf 0 ?do MKEY i IR-ID:PACK-FUN i WALK-FUN loop
   N-CUR @ A64RA:PLAN-N <> if E-A64SPILL-PLAN throw then
   c b IR-BUILD:FREEZE ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;using
;package

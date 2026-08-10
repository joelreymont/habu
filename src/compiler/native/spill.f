\ spill.f - build the machine module in which the register allocator's spill
\ decisions are real store and load operations.
\
\ docs/compiler-ir-design.md section 7.9. src/compiler/native/regalloc.f decides
\ which value loses its register, where, and which slot of the routine's frame it
\ goes into; those decisions are claims about a module that does not contain them
\ yet. This file is the step that makes them operations, and it is the only place
\ in the native chain that reads a machine module and writes another one.
\
\ WHY THIS IS A SECOND MODULE AND NOT AN EDIT. A frozen module cannot gain an
\ operation - that is what frozen means, and every reader downstream depends on
\ it - and a builder cannot gain one in the middle either, because a block's
\ operations are a window that only grows at its end. A spill store belongs in
\ front of the operation that took the register, so there is no way to add one to
\ a module that already ran to its terminator. The honest shape is therefore the
\ one every other stage of this chain already has: read a frozen module, build a
\ new one, freeze it, and let the ordinary verifier decide it. The alternative -
\ leaving the module alone and having the emitter materialise the stores and
\ loads out of the allocator's claims - was rejected deliberately: it would make
\ the instruction stream something no module contains, so the independent
\ validator would have nothing to re-derive the spills from and would end up
\ checking the allocator's belief against the allocator's belief. Here the
\ validator reads operations.
\
\ WHAT THE REWRITE IS, IN ONE SENTENCE. Every block of the old function appears
\ in the new one, with the same arguments, the same successors and the same
\ operations in the same order, reading the same values - except that a value
\ that lost its register is read out of its slot instead, by a load placed in
\ front of the operation that reads it.
\
\ AN ANCHOR IS A BLOCK AND A POSITION INSIDE IT. A plan row used to name a
\ position alone, which names one operation only while there is one block; with
\ more than one block the same index names one operation per block, and a store
\ would land in whichever block the cursor reached first. The row carries its
\ block now (src/compiler/native/regalloc.f, PLAN-BLOCK@), and the walk below
\ matches on both.
\
\ THE FIVE THINGS THIS PASS INSERTS. A reserve at the top of the entry block and
\ a release in front of the terminator of the block control leaves through,
\ because a routine that uses a slot has to take its frame and give it back; a
\ store in front of the operation the plan anchors it to; a load in front of each
\ operation that reads a value out of a slot; and a copy in front of the return,
\ for a returned value that is not in the register the routine's contract says it
\ leaves in. The order inside one anchor is the order the allocator decided them,
\ which is what makes the register the allocator counted on free exactly where it
\ counted on it.
\
\ THE FRAME MAY ALREADY BE THE ROUTINE'S OWN, AND THEN THIS PASS TAKES NONE. A
\ routine that calls has a frame before this pass ever sees it: the selector
\ reserved it and put the caller's return address in the slot
\ src/compiler/native/frame.f names, and the allocator placed its own slots above
\ that one out of the same declaration. So a module that arrives with a prologue
\ keeps it and gets no second reserve, and a module that arrives without one gets
\ a reserve and a release only when the plan really needs a slot. A frame
\ reserved and given back with nothing written into it would be two instructions
\ for nothing and a stack pointer no reader could account for.
\
\ THE MEMORY TOKEN IS THREADED HERE, AND RE-THREADED THROUGH WHAT WAS ALREADY
\ THERE. The dialect's frame forms carry a memory token so their order is a
\ dependency the module holds rather than a property of the printed order. The
\ reserve mints it, each store and load takes the last one and answers a new one,
\ and the release consumes the last. An operation of the OLD module that reaches
\ the frame - a prologue's save, an epilogue's restore and release - is part of
\ that same chain, so its token operand is rewritten to the order as it stands
\ where the operation now sits rather than carried across. Which operations those
\ are is read off the attribute keys the dialect declares for the frame, never
\ off an opcode name, so a data-stack access can never be threaded onto the
\ frame's order by mistake.
\
\ WHAT IT REFUSES, AND WHY EACH ONE IS ITS OWN JUDGEMENT.
\   - an unbound dialect, or a second binding over a live one. A module's symbols
\     are its own ordinals, so "is this operation a store" has no answer from
\     outside without the dialect's authority; this pass asks A64IR while the old
\     module is still being built, exactly as the allocator and the emitter do.
\   - a frozen module that is not the one the binding was taken over.
\   - a plan that is not about this module, or a walk that decided nothing at
\     all: rewriting a module that needs no store, no load and no copy would be a
\     duplicate, and a duplicate with a new identity is a module nobody asked for.
\   - a module whose frame operations are neither nothing nor exactly a selector's
\     prologue. Spill lowering runs once; a module that has been through it holds
\     a reserve of its own with no link save under it, and lowering that again
\     would build a second frame inside the first, so it stops here by name.
\   - a shape this pass cannot rebuild: more than one function, an empty block, a
\     span naming a source the old module does not have, or a value read before it
\     is defined.
\   - source text whose digest is not the one the old module recorded. Every
\     operation carries a span into that text, and the new module needs the same
\     source registered in it, so the bytes are proved to be the same bytes
\     before a single span is rebuilt.
\
\ WHERE THE CALLER STILL HAS TO KNOW THE ORDER. Reaching bytes for a program
\ that spills is four stages in one order - allocate, ask how many spills, lower,
\ allocate the lowered module - and every caller spells it out today. One word
\ that answers "the module that was emitted" would make the branch unforgettable
\ (dot habu-give-the-native-f9a7eb36). The one thing that cannot go wrong quietly
\ is the branch being skipped: an allocation that decided a spill is not an
\ assignment for the module it read, and the validator refuses it.
\
\ ONE REWRITE AT A TIME. The value map is a fixed package-owned slot rather than a
\ heap object, and the old module is read through the one cursor
\ src/compiler/native/frozen.f owns, so this pass rewrites one module at a time -
\ the single-task compilation discipline the rest of the native chain keeps. The
\ whole walk is one call, and the binding is taken at entry whatever the outcome,
\ so a refused rewrite leaves nothing behind for the next caller.

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

\ ---- the bound dialect -------------------------------------------------------
\ One slot per member of the machine operation family, so the family stays
\ exhaustive: a member added to A64IR:opcode makes this fail to compile until it
\ has a slot and a rule for rebuilding it.
73 constant OPCODES-N
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

\ One slot per attribute key the dialect declares.
11 constant KEYS-N
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

0 constant BOUND-NO
1 constant BOUND-YES

\ The longest function name this pass can carry across. A name is copied out of
\ the old module's interner and interned into the new one, because the two
\ modules number their symbols separately.
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

\ Which member of the family this symbol names. An operation of a form outside it
\ has no rule here and is refused rather than copied blind.
: OPCODE-SLOT ( IR-ID:ir-symbol-id -- n )
   {: sym:IR-ID:ir-symbol-id :}
   -1
   OPCODES-N 0 ?do
      sym i BND-OP @ SAME-SYM? if drop i leave then
   loop
   dup 0 < if E-A64SPILL-OPCODE throw then ;

\ Which declared key this symbol is. A frozen module carries no attribute under a
\ key its opcode's schema did not declare - the freeze verifier decides that - so
\ this refusal is fail-closed rather than reachable.
: KEY-SLOT-OF ( IR-ID:ir-symbol-id -- n )
   {: sym:IR-ID:ir-symbol-id :}
   -1
   KEYS-N 0 ?do
      sym i BND-KEY @ SAME-SYM? if drop i leave then
   loop
   dup 0 < if E-A64SPILL-OPCODE throw then ;

\ ---- which operations reach the routine's own frame --------------------------
\ The dialect carries a frame size and a frame slot offset under two of its keys
\ and the caller's data stack under three others, so the key an operation carries
\ says which region it reaches. Reading the key rather than the opcode is what
\ keeps the frame's memory order from ever being threaded through a data-stack
\ access - the same reading src/compiler/native/regalloc-verify.f makes.
: FRAME-TOUCH? ( IR-ID:ir-op-id -- bool )
   {: id:IR-ID:ir-op-id :}
   false
   id ATTRS-OF 0 ?do
      id i ATTR-KEY-AT KEY-SLOT-OF {: k:n :}
      k K-SLOT = k K-FRAME = or if drop true leave then
   loop ;

\ Is this value of the old module a memory order rather than a register? The
\ token operand of a frame access is the order it follows and its token result is
\ the order it leaves, and both are found by type because that is what tells them
\ apart from the registers beside them.
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

\ Which value carries an old value in front of the operation at this position: the
\ load placed there if there is one, and the value itself otherwise. The position
\ counts operations of the whole FUNCTION rather than of one block, because the
\ same index inside two blocks would make a load in one of them answer for a read
\ in the other.
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

\ The type of one value of the old module, restated in the new one. The two
\ modules number their types separately, so a value's class is carried across by
\ identity and not by ordinal; a value of neither class is a value this pass has
\ no type for.
: TYPE-OF ( IR-ID:ir-value-id -- IR-ID:ir-type-id )
   {: id:IR-ID:ir-value-id :}
   id VALUE-TYPE-AT {: t:IR-ID:ir-type-id :}
   t 0 BND-GPR @ SAME-TYPE? if CTX BLD A64IR:GPR-TYPE exit then
   t 0 BND-FPR @ SAME-TYPE? if CTX BLD A64IR:FPR-TYPE exit then
   t 0 BND-MEM @ SAME-TYPE? if CTX BLD A64IR:MEM-TYPE exit then
   E-A64SPILL-SHAPE throw ;

\ Is this value of the old module one of the floating file's? Every insert below
\ asks, because a value put away and brought back has to travel through the file
\ it lives in: the same eight bytes stored by the general form come back in a
\ general register, which is not where the operation that reads them looks.
: FPR-VALUE? ( IR-ID:ir-value-id -- bool )
   VALUE-TYPE-AT 0 BND-FPR @ SAME-TYPE? ;

: FPR-SLOT? ( n -- bool )
   {: k:n :}
   MKEY k IR-ID:PACK-VALUE FPR-VALUE? ;

\ WHICH FORM PUTS A VALUE AWAY AND WHICH BRINGS IT BACK IS THE ONE QUESTION THIS
\ PASS ASKS ABOUT A VALUE'S FILE, and it is these two words. The general file's
\ pair is a64.str and a64.ldr; the floating file's is a64.fstr and a64.fldr,
\ which are the same two forms with the same operands over the other register
\ file. Nothing else about an insert changes - same slot, same anchor, same
\ token, same order - because nothing else about it depends on where the eight
\ bytes live.
\
\ THE PASS USED TO REFUSE A DOUBLE HERE, AND WHAT THAT REFUSAL PROTECTED IS NOW
\ STRUCTURAL. While the dialect had no D-file access, putting a double away could
\ only have been done with the GENERAL store - the same eight bytes written by
\ the right instruction and read back into the wrong register file - so refusing
\ by name was the only fail-closed answer. The two words below make the file a
\ property of the value the plan names, and the substrate checks the answer: a
\ double handed to a64.str is an operand whose type is not the one that schema
\ declares, which IR-BUILD refuses as E-IR-VERIFY-OPTYPE.
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

\ The class of the register a value comes back into, which is the class of the
\ register it left. Reading it off the value rather than writing GPR everywhere is
\ what makes the reload of a double land where the operation below it looks.
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

\ The arithmetic immediate. This pass introduces no add or subtract immediate of
\ its own - folding a constant into one is the combine pass's - but it copies the
\ ones that pass built, and an immediate copied under the wrong key would be a
\ routine adding the wrong number.
: OFF-ATTR+ ( n -- )
   {: imm:n :}
   CTX BLD  CTX BLD A64IR:KEY-OFF  CTX BLD imm A64IR:OFF-ATTR  IR-BUILD:ADD-ATTR ;

: MASK-ATTR+ ( n -- )
   {: m:n :}
   CTX BLD  CTX BLD A64IR:KEY-MASK  CTX BLD m A64IR:MASK-ATTR  IR-BUILD:ADD-ATTR ;

\ The two data-stack fields. This pass inserts no data-stack operation of its
\ own - the convention is the selector's - but it copies the ones the selector
\ built, and a field copied under the wrong key would be a routine reading its
\ arguments out of its own frame.
: DSLOT-ATTR+ ( n -- )
   {: off:n :}
   CTX BLD  CTX BLD A64IR:KEY-DSLOT  CTX BLD off A64IR:DSLOT-ATTR  IR-BUILD:ADD-ATTR ;

: DBYTES-ATTR+ ( n -- )
   {: size:n :}
   CTX BLD  CTX BLD A64IR:KEY-DBYTES  CTX BLD size A64IR:DBYTES-ATTR  IR-BUILD:ADD-ATTR ;

\ The callee address a call to another word carries. It is copied across
\ unchanged: this pass moves values into and out of frame slots and decides
\ nothing about where a call goes.
: ENTRY-ATTR+ ( n -- )
   {: entry:n :}
   CTX BLD  CTX BLD A64IR:KEY-ENTRY  CTX BLD entry A64IR:ENTRY-ATTR
   IR-BUILD:ADD-ATTR ;

: DBACK-ATTR+ ( n -- )
   {: size:n :}
   CTX BLD  CTX BLD A64IR:KEY-DBACK  CTX BLD size A64IR:DBACK-ATTR  IR-BUILD:ADD-ATTR ;

\ The condition a comparison was made under. It is decoded back into the
\ dialect's own vocabulary before it is rebuilt, so a stored code the dialect has
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

\ ---- the four operations this pass inserts -----------------------------------
\ The routine takes its frame, and the memory order starts.
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

\ The value leaves its register for its slot. It is read here for the last time
\ as a register value; everything after this reads it out of the slot.
: EMIT-STORE ( IR-ID:ir-op-id n -- )
   {: at:IR-ID:ir-op-id k:n :}
   at k STORE-FORM OPEN
   MKEY k IR-ID:PACK-VALUE VOF OPERAND+
   TOK OPERAND+
   MEM-RESULT+
   k A64RA:SLOT@ SLOT-ATTR+
   CLOSE 0 RESULT@ TOK! ;

\ The value comes back into a register, in front of the operation that reads it.
\ The loaded value is a value of its own - a load defines a register, it does not
\ revive one - so the operation below it reads this value and not the old one.
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

\ The value is put into the register the routine's contract says it leaves in, in
\ front of the return. The copy reads the value as it stands at this position -
\ which is the reloaded one when the value spent part of its life in a slot - and
\ its result is a value of its own, so the return below it carries the copy and
\ not the original. Which register the copy lands in is not decided here and is
\ not in the plan: the lowered module is allocated again, and that walk reads the
\ declaration off the same contract.
\
\ AND IT IS THE ONE INSERT THAT STAYS IN THE GENERAL FILE, WHICH IS NOT AN
\ OMISSION BESIDE THE TWO ABOVE. A copy is planned for a returned value the
\ CONTRACT names a register for, and the only result placements a contract of
\ this chain states are general ones: A64EFF:ROUTINE carries a floating result
\ set, and nothing in the chain reads it - src/compiler/native/regalloc.f plans
\ its moves against the general out list alone (MB-PLAN-MOVES over OUTS-N). So a
\ double cannot be the subject of a move, and a64.fmovdd here would be a form
\ with no producer. The day a contract states where a returned double leaves,
\ this word wants the same two-armed question the store and the load ask - and
\ until then IR-BUILD refuses the mismatch by name rather than this pass copying
\ a double with a general move.
: EMIT-MOVE ( IR-ID:ir-op-id n n -- )
   {: at:IR-ID:ir-op-id k:n pos:n :}
   at A64IR-OPCODE:MOV OPEN
   MKEY k IR-ID:PACK-VALUE pos READ-AS OPERAND+
   GPR-RESULT+
   CLOSE {: id:IR-ID:ir-op-id :}
   k pos  id 0 RESULT@  RBIND ;

\ Every decision the allocator anchored to this position, in the order it made
\ them. The plan is in that order already - a walk decides one operation's spills
\ before the next one's - so this reads it with a cursor rather than searching
\ it, and the block below refuses a plan the cursor did not reach the end of: a
\ decision anchored to a position this block does not have would otherwise be
\ dropped in silence, and a dropped store is a value that never reaches its
\ slot.
: INSERT-ONE ( IR-ID:ir-op-id n n -- )
   {: at:IR-ID:ir-op-id j:n pos:n :}
   j A64RA:PLAN-VALUE@ {: k:n :}
   j A64RA:PLAN-STORE? if at k EMIT-STORE exit then
   j A64RA:PLAN-MOVE? if at k pos EMIT-MOVE exit then
   at k pos EMIT-LOAD ;

\ Does the row the cursor stands on belong in front of this operation? Both the
\ block and the index inside it have to agree; matching the index alone would put
\ a store meant for one block in front of the operation of the same number in
\ another.
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
      k K-SLOT = if v SLOT-ATTR+ then
      k K-FRAME = if v FRAME-ATTR+ then
      k K-DSLOT = if v DSLOT-ATTR+ then
      k K-DBYTES = if v DBYTES-ATTR+ then
      k K-COND = if v COND-ATTR+ then
      k K-DBACK = if v DBACK-ATTR+ then
      k K-ENTRY = if v ENTRY-ATTR+ then
      k K-OFF = if v OFF-ATTR+ then
      k K-MASK = if v MASK-ATTR+ then
   loop ;

\ The blocks a terminator hands control to. Blocks are copied one for one and in
\ order, so block b of the old module is block b of the new one and a successor
\ is carried across by its ordinal.
: COPY-SUCCS ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id SUCCS-OF {: n:n :}
   n 0 ?do
      CTX BLD
      BLD IR-BUILD:MODULE-KEY  id i SUCC-AT IR-ID:BLOCK-LOCAL  IR-ID:PACK-BLOCK
      IR-BUILD:ADD-SUCCESSOR
   loop ;

\ An operand that is the frame's own memory order is replaced by the order as it
\ stands here, because this pass may have put stores and loads between the two
\ operations that used to be neighbours on that chain. Every other operand is the
\ value the old one maps to.
: COPY-OPERANDS ( IR-ID:ir-op-id n bool -- )
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

\ The memory order a frame access answers becomes the order this pass threads its
\ own stores and loads onto from here on.
: BIND-RESULTS ( IR-ID:ir-op-id IR-ID:ir-op-id bool -- )
   {: old:IR-ID:ir-op-id new:IR-ID:ir-op-id frame:bool :}
   old RESULTS-OF {: n:n :}
   n 0 ?do
      old i RESULT-AT {: v:IR-ID:ir-value-id :}
      new i RESULT@ {: nv:IR-ID:ir-value-id :}
      v nv VBIND
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
\ The old block's arguments are the new block's arguments, one for one, so a
\ routine's inputs arrive the same way whether or not anything spilled. The value
\ map is NOT cleared here: a value defined in one block is read in the blocks it
\ dominates, so the map belongs to the function.
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

\ Does this pass take the frame itself? Only when the plan really needs a slot
\ AND the module did not arrive with a frame of its own. A plan of nothing but
\ moves needs no slot, and a routine that reserved a frame it never wrote into
\ would be two instructions and a stack pointer nobody can account for; a routine
\ that calls already took its frame in the selector, and a second reserve inside
\ the first is not a shape any reader of this chain has.
: FRAMES? ( -- bool )
   A64RA:SPILLS 0<> PRO-N @ 0= and ;

\ One block. The reserve opens the ENTRY block and the release stands in front of
\ the terminator of the block control leaves through, which is the same pair of
\ places the selector's own prologue uses - and the only pair whose two ends the
\ routine passes exactly once, in that order, on every run.
: WALK-BLOCK ( IR-ID:ir-fun-id n n -- )
   {: f:IR-ID:ir-fun-id b:n rb:n :}
   f b BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk OP-COUNT {: n:n :}
   n 1 < if E-A64SPILL-SHAPE throw then
   bk OPEN-BLOCK
   b 0= FRAMES? and if bk 0 OP-AT EMIT-RESERVE then
   n 0 ?do
      bk i OP-AT {: id:IR-ID:ir-op-id :}
      id b i G-AT @ INSERT-AT
      i n 1- =  b rb =  and  FRAMES?  and if id EMIT-RELEASE then
      id G-AT @ COPY-OP
      G-AT @ 1+ G-AT !
   loop
   CTX BLD IR-BUILD:END-BLOCK drop ;

: FUN-NAME ( IR-ID:ir-fun-id -- IR-ID:ir-symbol-id )
   {: f:IR-ID:ir-fun-id :}
   V-SYMP VW V-SYMR VW  V-FUNR VW MKEY f IR-FUN:FSYMBOL@  NAMEBUF NAME-CAP
   IR-SYM:FCOPY {: u:n :}
   CTX BLD NAMEBUF u IR-BUILD:INTERN-SYMBOL ;

\ The signature is the old one, restated in the new module's own types: one
\ virtual register per input and one per output, exactly as the old module has
\ them.
: FUN-SIG ( IR-ID:ir-fun-id -- IR-ID:ir-type-id )
   {: f:IR-ID:ir-fun-id :}
   V-TYPR VW  V-FUNR VW MKEY f IR-FUN:FSIGNATURE@  IR-TYPE:FARITY@
   {: in:n out:n :}
   CTX BLD A64IR:GPR-TYPE {: t:IR-ID:ir-type-id :}
   IR-TYPE:FN-BEGIN
   in 0 ?do t IR-TYPE:FN-PARAM loop
   out 0 ?do t IR-TYPE:FN-RESULT loop
   CTX BLD IR-BUILD:INTERN-CODE-REF ;

\ THE BLOCK THE ROUTINE'S RESULTS LEAVE THROUGH, and NO-RET when there is none.
\ The rule and the reason a trap block is not that block are written once, in
\ src/compiler/native/regalloc.f MB-RET-ORD; this is the same question asked of
\ this pass's own view, which is why it is asked again rather than taken from the
\ allocator.
-1 constant NO-RET

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

: WALK-FUN ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   CTX BLD f FUN-NAME IR-BUILD:BEGIN-FUN
   CTX BLD f FUN-SIG IR-BUILD:SET-SIGNATURE
   CTX BLD  V-FUNR VW f IR-FUN:FLINKAGE@  IR-BUILD:SET-LINKAGE
   CTX BLD  V-FUNR VW f IR-FUN:FVISIBILITY@  IR-BUILD:SET-VISIBILITY
   CTX BLD  V-FUNR VW f IR-FUN:FCONVENTION@  IR-BUILD:SET-CONVENTION
   CTX BLD f FUN-SPAN IR-BUILD:SET-FUN-SPAN
   f RET-ORD {: rb:n :}
   VCLEAR
   0 G-AT !
   f BLOCK-COUNT 0 ?do f i rb WALK-BLOCK loop
   N-CUR @ A64RA:PLAN-N <> if E-A64SPILL-PLAN throw then
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

\ The plan this rewrite is about. It has to be sealed, it has to be about the
\ module being read, and it has to have decided something: a module that needs no
\ spill needs no rewrite.
: PLAN-CK ( IR-BUILD:module -- )
   {: m:IR-BUILD:module :}
   A64RA:SEALED? 0= if E-A64SPILL-PLAN throw then
   m IR-BUILD:FMODULE A64RA:MODULE@ IR-ID:MODULE-SAME?
   0= if E-A64SPILL-PLAN throw then
   A64RA:PLAN-N 0= if E-A64SPILL-PLAN throw then ;

\ ---- whose frame the module arrives with -------------------------------------
\ Two shapes are lowerable and they are told apart by counting the four frame
\ forms of the dialect. A module with NONE of them has no frame yet, so this pass
\ takes one when the plan needs a slot. A module with exactly one reserve, one
\ link save, one link restore and one release, the reserve opening the entry
\ block, is a routine that CALLS: the selector built that prologue out of the
\ contract and the allocator placed its slots above the link slot out of the same
\ contract, so the frame is already the right size and this pass adds nothing to
\ its ends. Anything else has been through this pass already - a reserve with no
\ save under it is the one it emitted last time - and lowering it again would
\ build a second frame inside the first.
\
\ THE TEST IS THE FRAME FORMS BY NAME AND NOT "ANY OPERATION THAT TOUCHES
\ MEMORY": a routine that reads its arguments off the caller's data stack touches
\ memory in every one of its entry loads and has never been through this pass.
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

: ONCE-CK ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   f COUNT-FRAME
   N-SAV @ 0= if NO-FRAME-CK 0 PRO-N ! exit then
   f PROLOGUE-CK
   1 PRO-N ! ;

: SHAPE-CK ( -- )
   FUN-COUNT 1 <> if E-A64SPILL-SHAPE throw then
   MKEY 0 IR-ID:PACK-FUN ONCE-CK ;

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
\ Learn the operation, key and type identities of the module that is about to be
\ rewritten, while it is still being built. A module's symbols and types are its
\ own ordinals, so this is the only moment the dialect can be asked which one
\ each of them is; the answers stay valid after the module freezes because
\ freezing keeps the module's identity. The binding is spent by the next REWRITE.
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
   c b A64IR:KEY-IMM    K-IMM BND-KEY !
   c b A64IR:KEY-SHIFT  K-SHIFT BND-KEY !
   c b A64IR:KEY-SLOT   K-SLOT BND-KEY !
   c b A64IR:KEY-FRAME  K-FRAME BND-KEY !
   c b A64IR:KEY-DSLOT  K-DSLOT BND-KEY !
   c b A64IR:KEY-DBYTES K-DBYTES BND-KEY !
   c b A64IR:KEY-COND   K-COND BND-KEY !
   c b A64IR:KEY-DBACK  K-DBACK BND-KEY !
   c b A64IR:KEY-ENTRY  K-ENTRY BND-KEY !
   c b A64IR:KEY-OFF    K-OFF BND-KEY !
   c b A64IR:KEY-MASK   K-MASK BND-KEY !
   c b A64IR:GPR-TYPE 0 BND-GPR !
   c b A64IR:MEM-TYPE 0 BND-MEM !
   c b A64IR:FPR-TYPE 0 BND-FPR !
   BOUND-YES BND-MODE ! ;

\ Whether a binding is live, for a caller cleaning up after a refused run. See
\ src/compiler/native/select.f BOUND? for why each pass answers for itself. This
\ pass needs one for a reason the other three do not have: whether its binding was
\ ever spent depends on whether the walk decided a spill, and a caller cleaning up
\ after a refusal cannot know how far the run got.
: BOUND? ( -- bool )
   BND-MODE @ BOUND-YES = ;

\ Give up a binding without rewriting against it.
: RELEASE ( -- )
   BND-TAKE ;

\ ---- the pass ----------------------------------------------------------------
\ Build the module in which the sealed spill plan is real operations, and answer
\ it frozen. The builder is a fresh one from A64IR:NEW-BUILDER - this pass
\ registers the machine operation family into it, so a builder that already holds
\ them, or one of another dialect, is refused by A64IR. The bytes are the source
\ text the old module was compiled from, and they are proved to be by digest
\ before any span is carried across.
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
   SHAPE-CK
   MKEY 0 IR-ID:PACK-FUN WALK-FUN
   c b IR-BUILD:FREEZE ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;using
;package

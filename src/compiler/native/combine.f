\ combine.f - the module in which a multiply and the addition that reads its
\ product are one multiply-add. One concern: finding those pairs and writing the
\ module that holds the combined form.
\
\ WHY THE PASS EXISTS, AND WHY IT IS THIS PATTERN AND NOT A LIBRARY OF THEM.
\ ARM64 has a multiply-add: `madd rd, rn, rm, ra` computes ra + rn*rm in one
\ instruction and one cycle's worth of encoding, where this chain emits a
\ multiply and then an addition. Which of the shapes ARM64 rewards actually
\ OCCUR in the code this chain emits is a measurement, not a matter of taste, and
\ tools/codegen-combine-inventory.f is that measurement: over the 723
\ instructions of the 54 migrated corpus rows it finds 17 multiply-then-add
\ pairs, no multiply-then-subtract pair at all, no immediate shift at all - so no
\ shifted-operand fold and no bitfield extract - and 25 pairable loads whose
\ instruction this tree does not model. This pass is therefore the ONE pattern
\ the measurement found a consumer for. The others are recorded as measured
\ zeroes rather than written on the chance that some other corpus would want
\ them.
\
\ WHY IT IS A PASS AND NOT A RULE INSIDE THE SELECTOR. src/compiler/native/
\ select.f already fuses one pair - a comparison and the branch that reads it -
\ and the obvious economy would be to fuse this pair there too. The obstacle is
\ that a selector which emits fewer operations than it walks has to keep its own
\ accounting of how many instructions each block became (select.f's FUSED-GPR?
\ and the residency and placement code around it), because the block's operation
\ count no longer says. A pass that writes a module in which the multiply is
\ simply not there needs none of that: the new module's operation count IS the
\ instruction count, exactly as it is for every module this chain emits, and
\ src/compiler/native/emit.f counts it the way it counts every other. One fewer
\ place where a number is kept by hand.
\
\ AND BECAUSE THE RESULT IS VALIDATED RATHER THAN TRUSTED. docs/compiler-ir-
\ design.md section 9.2 puts fusion among the passes to treat as untrusted
\ producers with a small validator, and that is what this is: the module this
\ pass writes goes through the ordinary src/compiler/native/regalloc-verify.f,
\ which re-derives every register fact from the operand and result windows of the
\ module it is handed and knows nothing about this pass. A combine that got a
\ register wrong is caught by something that does not share its reasoning.
\
\ WHAT IS FUSED, IN ONE SENTENCE. A multiply whose result is read by exactly one
\ operation in the whole function, where that operation is an addition in the
\ same block, becomes the addition's multiply-add: the multiply is not copied,
\ and the addition is written as `madd` taking the multiply's two factors and the
\ addition's OTHER operand as its addend.
\
\ THE FOUR CONDITIONS, AND WHY EACH ONE IS THERE.
\
\   THE PRODUCT HAS EXACTLY ONE USE. This is the condition that makes the fusion
\   pay rather than cost. A product read twice still has to be computed into a
\   register, so folding one of its readers would leave the multiply where it was
\   and ADD an instruction. It is the same test src/compiler/native/select.f
\   makes before it fuses a comparison into a branch (USES-OF ... 1 <>), and it
\   is exact here for the reason it is exact there: this dialect is SSA, so a
\   value's uses are the operands that name it and there is nothing else to
\   count.
\
\   THAT USE IS AN ADDITION. Nothing else has a multiply-add. A subtraction has
\   `msub`, which the shipped assembler and formal/Common/Insn.v both carry - but
\   the inventory counts ZERO multiply-then-subtract pairs in this corpus, so the
\   form has no consumer and this pass does not write one. When a corpus grows
\   one, `msub` is a second arm here and the model row is already waiting.
\
\   THE TWO ARE IN ONE BLOCK. The multiply-add stands where the ADDITION stood,
\   so the multiply moves forward to that point. Within a block that is always
\   sound - the factors are SSA values that are still what they were, and a
\   multiply cannot trap, which its schema says by being TOTAL - and a rule that
\   reached across blocks would have to ask whether the multiply's block always
\   reaches the addition's, which is a dominance question this pass has no reason
\   to open for a pattern the measurement finds adjacent.
\
\   AND THE ADDEND IS NOT THE PRODUCT ITSELF. `x*y + x*y` reads the product
\   twice, so the use count already refuses it; the case is named because it is
\   the one shape where an addition's two operands are one value and a rewriter
\   that counted USES PER OPERATION rather than per operand would call it a
\   single use and fold a value it still needs.
\
\ WHAT THIS PASS DOES NOT DECIDE. Which register anything ends up in - that is
\ the allocator's, and this pass runs before it, which is the whole reason it can
\ see the pattern at all. The same two instructions AFTER allocation are often
\ unfusable: the allocator reuses registers, so the corpus row C-MAD (`3 * 5 +`)
\ emits a multiply and an addition whose addend sits in the very register the
\ multiply read, and no rewriter of finished code may touch it. Running here,
\ before registers exist, is what makes those rows reachable.
\
\ ONE REWRITE AT A TIME, for the reason src/compiler/native/spill.f gives: the
\ value map is a package-owned slot and the old module is read through the one
\ cursor src/compiler/native/frozen.f owns.

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
\ One slot per member of the machine operation family, so the family stays
\ exhaustive: a member added to A64IR:opcode makes this fail to compile until it
\ has a slot and a rule for rebuilding it here too.
62 constant OPCODES-N

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
\ One slot per attribute key the dialect declares. This pass writes no attribute
\ of its own - the form it introduces carries none - but it COPIES every one the
\ selector built, and a field copied under the wrong key would be a routine
\ reading its arguments out of its own frame.
10 constant KEYS-N
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

0 constant BOUND-NO
1 constant BOUND-YES

\ The longest function name this pass can carry across. A name is copied out of
\ the old module's interner and interned into the new one, because the two
\ modules number their symbols separately.
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

\ The block's plan, one cell per operation: which operation of this block folds
\ into the one at this position, or -1 when none does. A multiply named here is
\ not copied, and the operation it is named at is written as the multiply-add.
create FOLD-AT OPS-MAX cells allot
create FOLDED OPS-MAX cells allot

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
      madd      OF O-MADD      ENDOF
      addi      OF O-ADDI      ENDOF
      subi      OF O-SUBI      ENDOF
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
      E-A64SPILL-OPCODE throw
   endcase ;

\ Which member of the family this symbol names. An operation of a form outside it
\ Which member of the family this symbol names. An operation of a form outside it
\ has no rule here and is refused rather than copied blind.
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
\ Old value to new value. A multiply that was folded away binds NOTHING here, so
\ a reader of its product that this pass failed to account for does not quietly
\ get some other value - it reaches an unset slot and the rewrite is refused.
\ That is the safety net under the use count below.
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

\ The type of one value of the old module, restated in the new one. The two
\ modules number their types separately, so a value's class is carried across by
\ identity and not by ordinal; a value of neither class is one this pass has no
\ type for.
: TYPE-OF ( IR-ID:ir-value-id -- IR-ID:ir-type-id )
   {: id:IR-ID:ir-value-id :}
   id VALUE-TYPE-AT {: t:IR-ID:ir-type-id :}
   t 0 BND-GPR @ SAME-TYPE? if CTX BLD A64IR:GPR-TYPE exit then
   t 0 BND-FPR @ SAME-TYPE? if CTX BLD A64IR:FPR-TYPE exit then
   t 0 BND-MEM @ SAME-TYPE? if CTX BLD A64IR:MEM-TYPE exit then
   E-A64COMB-SHAPE throw ;

\ ---- how many operands of the function name a value --------------------------
\ The same count src/compiler/native/select.f takes before it fuses a comparison
\ into a branch, and it is counted per OPERAND rather than per operation: an
\ addition whose two operands are one value uses it twice, and a count that said
\ once would fold a product that is still needed.
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
\ Where in this block the operation defining a value is, or -1 when the value is
\ not defined by an operation of this block at all - a block argument, or a value
\ from another block. Only a definition IN THIS BLOCK may be folded, because the
\ combined form stands where the reader stands.
: DEF-INDEX ( IR-ID:ir-block-id IR-ID:ir-value-id -- n )
   {: bk:IR-ID:ir-block-id v:IR-ID:ir-value-id :}
   -1
   bk OP-COUNT 0 ?do
      bk i OP-AT {: id:IR-ID:ir-op-id :}
      id RESULTS-OF 1 = if
         id 0 RESULT-AT v SAME-VALUE? if drop i leave then
      then
   loop ;

\ Whether the operation at this position is a multiply this pass may fold into
\ the one reading it: a multiply, defining one value, and that value read by
\ exactly one operand of the whole function.
: FOLDABLE-MUL? ( IR-ID:ir-fun-id IR-ID:ir-block-id n -- bool )
   {: f:IR-ID:ir-fun-id bk:IR-ID:ir-block-id k:n :}
   k 0 < if false exit then
   bk k OP-AT {: id:IR-ID:ir-op-id :}
   id OP-SLOT O-MUL <> if false exit then
   id RESULTS-OF 1 <> if false exit then
   f  id 0 RESULT-AT  USES-OF 1 = ;

\ And whether it stands BEFORE the addition that would fold it. A frozen module
\ defines every value before it is read, so a multiply behind an addition's
\ operand is always above it in the block and this is never false - but the
\ combined form is written where the ADDITION stands, and if the multiply were
\ below it the pass would be moving a computation backwards past its own inputs.
\ That is the one thing this rewrite must not do, so it is asked here rather than
\ inherited from the verifier that ran before it.
: FOLDS-HERE? ( IR-ID:ir-fun-id IR-ID:ir-block-id n n -- bool )
   {: f:IR-ID:ir-fun-id bk:IR-ID:ir-block-id d:n k:n :}
   d k >= if false exit then
   f bk d FOLDABLE-MUL? ;

\ The multiply this addition folds, or -1. The addition names two operands; a
\ multiply behind EITHER of them will do, and the first one asked wins so that a
\ hypothetical `x*y + x*y` - which the use count has already refused - could
\ never be read as folding both.
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

\ The whole block's plan, read once before a single operation of it is copied,
\ so the walk and the operation it reaches later agree about what was decided.
: PLAN-BLOCK ( IR-ID:ir-fun-id IR-ID:ir-block-id -- )
   {: f:IR-ID:ir-fun-id bk:IR-ID:ir-block-id :}
   bk OP-COUNT {: n:n :}
   n OPS-MAX > if E-A64COMB-CAP throw then
   n 0 ?do
      -1 i cells FOLD-AT + !
      0 i cells FOLDED + !
   loop
   n 0 ?do
      f bk i FOLD-FOR {: d:n :}
      d 0 >= if
         d i cells FOLD-AT + !
         1 d cells FOLDED + !
      then
   loop ;

: FOLD-OF ( n -- n )
   cells FOLD-AT + @ ;

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
\ The addition's operand that is NOT the folded multiply's product: the addend.
\ It is found by identity against the product rather than by position, because
\ either operand of an addition may be the one that carries it.
: ADDEND-OF ( IR-ID:ir-op-id IR-ID:ir-value-id -- IR-ID:ir-value-id )
   {: id:IR-ID:ir-op-id prod:IR-ID:ir-value-id :}
   id 0 OPERAND-AT prod SAME-VALUE? if id 1 OPERAND-AT exit then
   id 0 OPERAND-AT ;

\ The multiply-add itself, written where the addition stood. Its operands are the
\ multiply's two factors and then the addend, which is the order
\ src/compiler/native/a64ir.f's schema declares and the order
\ `madd rd, rn, rm, ra` names them in. The ADDITION's result is what the new
\ operation defines, so everything that read the sum still reads it; the
\ multiply's product is bound to nothing, and the value map refuses any reader of
\ it that this pass did not account for.
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

\ ---- the block ---------------------------------------------------------------
\ The old block's arguments are the new block's arguments, one for one. The value
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
         d 0 <
         if   bk i OP-AT COPY-OP
         else bk d OP-AT  bk i OP-AT  EMIT-MADD
         then
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
\ Learn the operation, key and type identities of the module that is about to be
\ read, while it is still being built - the only moment a module can be asked
\ them, because its symbols and types are its own ordinals. The binding is spent
\ by the next REWRITE, or given back by RELEASE when the scan finds nothing.
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
   c b A64IR-OPCODE:MADD      BIND1
   c b A64IR-OPCODE:ADDI      BIND1
   c b A64IR-OPCODE:SUBI      BIND1
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
\ How many pairs this module would fold, asked before anything is built. A caller
\ that gets zero keeps the module it has, which is what keeps every routine
\ WITHOUT the pattern byte-for-byte what it was: no second module is built, no
\ value is renumbered, and the register allocator sees exactly what it saw
\ before. It reads through the bound module's own cursor, so it is asked between
\ the binding and the rewrite.
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
         loop
      loop
   loop ;

\ ---- the pass ----------------------------------------------------------------
\ Build the module in which each of those pairs is one multiply-add, and answer
\ it frozen. The builder is a fresh one from A64IR:NEW-BUILDER - this pass
\ registers the machine operation family into it - and the bytes are the source
\ text the old module was compiled from, proved by digest before any span is
\ carried across.
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

\ How many pairs the last rewrite really folded. A caller compares it with what
\ the scan promised, so a walk that quietly folded a different number than the
\ scan counted is a refusal at the caller rather than a module nobody checked.
: FUSED ( -- n )
   N-FUSED @ ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;using
;package

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
require src/compiler/native/prof.f

package A64COMB
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
A64IR-OPCODE:MOVZ  A64IR:ORD constant O-MOVZ
A64IR-OPCODE:ADD   A64IR:ORD constant O-ADD
A64IR-OPCODE:SUB   A64IR:ORD constant O-SUB
A64IR-OPCODE:MUL   A64IR:ORD constant O-MUL
A64IR-OPCODE:FLAG  A64IR:ORD constant O-FLAG
A64IR-OPCODE:CMPBR A64IR:ORD constant O-CMPBR
A64IR-OPCODE:AND   A64IR:ORD constant O-AND
A64IR-OPCODE:ORR   A64IR:ORD constant O-ORR
A64IR-OPCODE:EOR   A64IR:ORD constant O-EOR

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
: OPS-MAX ( -- n ) OMAX ;

here CELL 1- and CELL swap - CELL 1- and allot
variable BND-MODE
BOUND-NO BND-MODE !
variable N-FUSED                     \ pairs this rewrite folded, counted as it goes
variable N-REMOVED                   \ unused data-stack reads removed
variable B-BASE                      \ operations of the module before the current block
variable PLAN-OPS                    \ operations the sealed plan covers
variable PLAN-FUSED                  \ pairs that plan names
variable PLAN-REMOVED                \ unused data-stack reads that plan removes
variable PLAN-SET                    \ a plan is sealed

1 TYPED-BUFFER BND-MOD IR-ID:ir-module-id
1 TYPED-BUFFER PLAN-MOD IR-ID:ir-module-id
A64IR:OPCODES TYPED-BUFFER BND-OP IR-ID:ir-symbol-id
KEYS-N TYPED-BUFFER BND-KEY IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-GPR IR-ID:ir-type-id
1 TYPED-BUFFER BND-MEM IR-ID:ir-type-id
1 TYPED-BUFFER BND-FPR IR-ID:ir-type-id

1 TYPED-BUFFER S-CTX IR-CTX:ctx
1 TYPED-BUFFER S-BLD IR-BUILD:builder
1 TYPED-BUFFER S-SID IR-ID:ir-source-id
DYNAMIC-BUFFER VMAP IR-ID:ir-value-id
DYNAMIC-BUFFER VSET-BUF n
: VSET ( -- ptr n ) 0 VSET-BUF ;
DYNAMIC-BUFFER USE-COUNTS n

: RESERVE-SCRATCH ( -- )
   SCRATCH-SIZES!
   VMAX VMAP-RESERVE
   VMAX VSET-BUF-RESERVE
   VMAX USE-COUNTS-RESERVE
   ;
create NAMEBUF NAME-CAP allot

\ One cell per operation: which operation of this block folds into the one at
\ this position, or -1. A multiply named here is not copied.
DYNAMIC-BUFFER FOLD-AT-BUF n
: FOLD-AT ( -- ptr n ) 0 FOLD-AT-BUF ;
DYNAMIC-BUFFER FOLDED-BUF n
: FOLDED ( -- ptr n ) 0 FOLDED-BUF ;
DYNAMIC-BUFFER IMM-AT-BUF n
: IMM-AT ( -- ptr n ) 0 IMM-AT-BUF ;
DYNAMIC-BUFFER MASK-AT-BUF n
: MASK-AT ( -- ptr n ) 0 MASK-AT-BUF ;
DYNAMIC-BUFFER CMP-AT-BUF n
: CMP-AT ( -- ptr n ) 0 CMP-AT-BUF ;

: RESERVE-FOLDS ( -- )
   OPS-MAX FOLD-AT-BUF-RESERVE
   OPS-MAX FOLDED-BUF-RESERVE
   OPS-MAX IMM-AT-BUF-RESERVE
   OPS-MAX MASK-AT-BUF-RESERVE
   OPS-MAX CMP-AT-BUF-RESERVE
   ;

\ ---- the slots, read back ----------------------------------------------------
: CTX ( -- IR-CTX:ctx )              0 S-CTX @ ;
: BLD ( -- IR-BUILD:builder )        0 S-BLD @ ;
: SID ( -- IR-ID:ir-source-id )      0 S-SID @ ;

\ An operation of a form outside the family has no rule here and is refused
\ rather than copied blind.
: OPCODE-SLOT ( IR-ID:ir-symbol-id -- n )
   {: sym:IR-ID:ir-symbol-id :}
   -1
   A64IR:OPCODES 0 ?do
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

\ Count each source operand once. Freeze rejects cross-function references, so
\ these module-wide value counts are also each value's function-local counts.
\ Duplicate operands count twice: folding one would leave the producer live.
: COUNT-OP-USES ( IR-ID:ir-op-id -- )
   {: id:IR-ID:ir-op-id :}
   id OPERANDS-OF 0 ?do
      1 id i OPERAND-AT VSLOT USE-COUNTS +!
   loop ;

: COUNT-BLOCK-USES ( IR-ID:ir-block-id -- )
   {: bk:IR-ID:ir-block-id :}
   bk OP-COUNT 0 ?do
      bk i OP-AT COUNT-OP-USES
   loop ;

: COUNT-FUN-USES ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   f BLOCK-COUNT 0 ?do
      f i BLOCK-AT COUNT-BLOCK-USES
   loop ;

: COUNT-USES ( -- )
   VMAX 0 ?do 0 i USE-COUNTS ! loop
   FUN-COUNT 0 ?do MKEY i IR-ID:PACK-FUN COUNT-FUN-USES loop ;

: USES-OF ( IR-ID:ir-value-id -- n ) VSLOT USE-COUNTS @ ;

\ ---- which pairs this block folds --------------------------------------------
\ Only a definition IN THIS BLOCK may be folded, because the combined form
\ stands where the reader stands.
: DEF-INDEX ( IR-ID:ir-block-id IR-ID:ir-value-id -- n )
   {: bk:IR-ID:ir-block-id v:IR-ID:ir-value-id :}
   V-VALR VW v IR-OP:FVALUE-KIND@ IR--OP-DEF--KIND:OP-RESULT
   IR--OP-DEF--KIND:EQ 0= if -1 exit then
   V-VALR VW V-OPR VW MKEY v IR-OP:FVALUE-OP@ {: id:IR-ID:ir-op-id :}
   id RESULTS-OF 1 <> if -1 exit then
   bk OP-COUNT {: n:n :}
   n 0= if -1 exit then
   id IR-ID:OP-LOCAL bk 0 OP-AT IR-ID:OP-LOCAL - {: at:n :}
   at 0 < at n >= or if -1 else at then ;

\ A multiply defining one value, that value read by exactly one operand of the
\ whole function.
: FOLDABLE-MUL? ( IR-ID:ir-fun-id IR-ID:ir-block-id n -- bool )
   {: f:IR-ID:ir-fun-id bk:IR-ID:ir-block-id k:n :}
   k 0 < if false exit then
   bk k OP-AT {: id:IR-ID:ir-op-id :}
   id OP-SLOT O-MUL <> if false exit then
   id RESULTS-OF 1 <> if false exit then
   id 0 RESULT-AT USES-OF 1 = ;

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
   id 0 RESULT-AT USES-OF 1 = ;

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
   id 0 RESULT-AT USES-OF 1 = ;

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
\ Selection can eliminate every use of a branch condition. An unread, total
\ data-stack load can be removed while forwarding its memory-order result.
: UNUSED-DLOAD? ( IR-ID:ir-op-id -- bool ) {: id:IR-ID:ir-op-id :}
   id OP-SLOT {: s:n :}
   s A64IR-OPCODE:DLOAD A64IR:ORD =
   s A64IR-OPCODE:FDLOAD A64IR:ORD = or 0= if false exit then
   id 0 RESULT-AT USES-OF 0= ;

\ FOLDED distinguishes a consumed producer (1) from a removed load (2).
2 constant REMOVE-LOAD

: PLAN-BLOCK ( IR-ID:ir-fun-id IR-ID:ir-block-id -- )
   {: f:IR-ID:ir-fun-id bk:IR-ID:ir-block-id :}
   bk OP-COUNT {: n:n :}
   B-BASE @ {: g:n :}
   g 0 < g OPS-MAX > or if E-A64COMB-CAP throw then
   n 0 < n OPS-MAX g - > or if E-A64COMB-CAP throw then
   n 0 ?do
      -1 g i + cells FOLD-AT + !
      -1 g i + cells IMM-AT + !
      -1 g i + cells MASK-AT + !
      -1 g i + cells CMP-AT + !
      0 g i + cells FOLDED + !
   loop
   n 0 ?do
      f bk i FOLD-FOR {: d:n :}
      d 0 >= if
         d g i + cells FOLD-AT + !
         1 g d + cells FOLDED + !
         1 PLAN-FUSED +!
      then
   loop
   n 0 ?do
      f bk i IMM-FOLD-FOR {: d:n :}
      d 0 >= if
         d g i + cells IMM-AT + !
         1 g d + cells FOLDED + !
         1 PLAN-FUSED +!
      then
   loop
   n 0 ?do
      f bk i MASK-FOLD-FOR {: d:n :}
      d 0 >= if
         d g i + cells MASK-AT + !
         1 g d + cells FOLDED + !
         1 PLAN-FUSED +!
      then
   loop
   n 0 ?do
      f bk i CMP-FOLD-FOR {: d:n :}
      d 0 >= if
         d g i + cells CMP-AT + !
         1 g d + cells FOLDED + !
         1 PLAN-FUSED +!
      then
   loop
   n 0 ?do
      bk i OP-AT UNUSED-DLOAD? if
         REMOVE-LOAD g i + cells FOLDED + !
         1 PLAN-REMOVED +!
      then
   loop
   g n + B-BASE ! ;

\ Read at the same module-wide position the plan was written at: the walk visits
\ functions, blocks and operations in the order the plan pass did.
: FOLD-OF ( n -- n )
   B-BASE @ + cells FOLD-AT + @ ;

: IMM-OF ( n -- n )
   B-BASE @ + cells IMM-AT + @ ;

: MASK-OF ( n -- n )
   B-BASE @ + cells MASK-AT + @ ;

: CMP-OF ( n -- n )
   B-BASE @ + cells CMP-AT + @ ;

: FOLDED? ( n -- bool )
   B-BASE @ + cells FOLDED + @ 0<> ;

: REMOVED? ( n -- bool )
   B-BASE @ + cells FOLDED + @ REMOVE-LOAD = ;

\ ---- staging one operation in the new module ---------------------------------
: OPEN ( IR-ID:ir-op-id A64IR:opcode -- )
   {: id:IR-ID:ir-op-id o:A64IR:opcode :}
   CTX BLD  CTX BLD o A64IR:ENSURE-OP  IR-BUILD:BEGIN-OP
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
   id OP-SLOT A64IR:NTH {: o:A64IR:opcode :}
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

: REMOVE-DLOAD ( IR-ID:ir-op-id -- ) {: id:IR-ID:ir-op-id :}
   id 1 RESULT-AT id 0 OPERAND-AT VOF VBIND
   1 N-REMOVED +! ;

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
   bk OPEN-BLOCK
   n 0 ?do
      i REMOVED? if
         bk i OP-AT REMOVE-DLOAD
      else i FOLDED? 0= if
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
      then then
   loop
   B-BASE @ n + B-BASE !
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
: SOURCE! ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   V-SRC VW IR-SOURCE:FSOURCES 1 <> if E-A64COMB-SHAPE throw then
   c b  V-SRC VW  MKEY 0 IR-ID:PACK-SOURCE  IR-BUILD:CARRY-SOURCE 0 S-SID ! ;

\ The binding is taken whatever the outcome, so neither a rewrite without a
\ binding nor a refused rewrite can leave one behind for the next caller.
: BND-TAKE ( -- )
   BND-MODE @ {: have:n :}
   BOUND-NO BND-MODE !
   have BOUND-YES <> if E-A64COMB-BIND throw then ;

: BND-MODULE-CK ( IR-BUILD:module -- )
   IR-BUILD:FMODULE  0 BND-MOD @  IR-ID:MODULE-SAME?
   0= if E-A64COMB-BIND throw then ;

\ The plan the scan sealed, and about this module: a rewrite that planned
\ nothing would walk stale decisions, and one planned for another module would
\ fold operations that are not there.
: PLAN-TAKE ( IR-BUILD:module -- )
   {: m:IR-BUILD:module :}
   PLAN-SET @ {: planned:n :}
   0 PLAN-SET !
   BND-TAKE
   m BND-MODULE-CK
   planned 0= if E-A64COMB-PLAN throw then
   m IR-BUILD:FMODULE  0 PLAN-MOD @  IR-ID:MODULE-SAME?
   0= if E-A64COMB-PLAN throw then ;

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
   0 PLAN-SET !
   c b 0 BND-OP A64IR:OPCODES A64IR:BIND-OPCODES! 0 BND-MOD !
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
   0 PLAN-SET !
   BND-TAKE ;

\ ---- what the module holds ---------------------------------------------------
\ Asked before anything is built. A caller that gets zero keeps the module it
\ has, which is what keeps every routine without the pattern byte-for-byte.
\
\ The search that answers it IS the plan the rewrite walks, so it is made once
\ and sealed for that module: asking and then rewriting used to fold every
\ operation's pair twice. Sealing it is this pass's own step - a caller that
\ sealed a plan and did not rewrite against it would leave one standing - so it
\ is not part of the package's surface.
private
: PLAN! ( IR-BUILD:module -- n )
   {: m:IR-BUILD:module :}
   0 PLAN-SET !
   BOUND? 0= if E-A64COMB-BIND throw then
   m BND-MODULE-CK
   m VIEWS!
   RESERVE-SCRATCH RESERVE-FOLDS
   COUNT-USES
   0 PLAN-FUSED ! 0 PLAN-REMOVED !
   0 B-BASE !
   FUN-COUNT 0 ?do
      MKEY i IR-ID:PACK-FUN {: f:IR-ID:ir-fun-id :}
      f BLOCK-COUNT 0 ?do
         f  f i BLOCK-AT  PLAN-BLOCK
      loop
   loop
   B-BASE @ PLAN-OPS !
   m IR-BUILD:FMODULE 0 PLAN-MOD !
   1 PLAN-SET !
   PLAN-FUSED @ PLAN-REMOVED @ + ;

public
: REWRITES ( IR-BUILD:module -- n )
   PLAN! ;

\ ---- the pass ----------------------------------------------------------------
\ The source is carried from the old module, so this pass is never handed the
\ text and cannot be handed the wrong text.
: REWRITE ( IR-CTX:ctx IR-BUILD:module IR-BUILD:builder -- IR-BUILD:module )
   {: c:IR-CTX:ctx m:IR-BUILD:module b:IR-BUILD:builder :}
   m PLAN-TAKE
   0 N-FUSED ! 0 N-REMOVED !
   c 0 S-CTX !
   b 0 S-BLD !
   m VIEWS!
   NFROZEN:TOTAL-OPS NPROF-PHASE:COMBINE-OPS NPROF:ADD
   c b SOURCE!
   0 B-BASE !
   FUN-COUNT 0 ?do MKEY i IR-ID:PACK-FUN WALK-FUN loop
   B-BASE @ PLAN-OPS @ <> if E-A64COMB-SHAPE throw then
   c b IR-BUILD:FREEZE ;

\ A caller compares it with what the scan promised, so a walk that folded a
\ different number is a refusal rather than a module nobody checked.
: FUSED ( -- n )
   N-FUSED @ ;

: REWRITTEN ( -- n )
   N-FUSED @ N-REMOVED @ + ;

public
: RESET-SCRATCH ( -- )
   0 PLAN-SET !
   0 SCRATCH-VALUES ! 0 SCRATCH-BLOCKS ! 0 SCRATCH-FUNS ! 0 SCRATCH-OPS ! ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;using
;package

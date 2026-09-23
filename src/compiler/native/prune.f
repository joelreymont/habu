\ prune.f - the module in which a data-stack load nothing reads is not there.
\ One concern: finding those loads and writing the module without them.
\
\ WHY THE COMPILER CANNOT SIMPLY NOT WRITE ONE. Selection decides which cells a
\ block enters holding before its if-conversion decides which comparisons it
\ will write, and a conversion whose arms all hand the join the same value
\ writes no comparison at all - so a cell loaded for that comparison is loaded
\ for a reader that is never written. By then the load is in a module, and a
\ module is immutable, so removing it means writing the module again. It is not
\ an optimisation: src/compiler/native/regalloc-verify.f refuses a data-stack
\ access the emission had no reason to make (E-A64RAV-DKEEP), so a routine that
\ keeps such a load does not compile.
\
\ THE REAL FIX IS IN SELECTION and is not this file: a residency that knew which
\ readers the conversion will elide would not load the cell, and then nothing
\ would have to be written twice. Until it does, this pass stands between the
\ two, and it does nothing at all to a module that has no such load - which is
\ every module in the 1,772-word corpus.
\
\ One rewrite at a time: the value map is a package-owned slot and the old
\ module is read through the one cursor src/compiler/native/frozen.f owns.

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

package A64PRUNE
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
17 constant KEYS-N
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
14 constant K-DWB                      \ the pointer move a fused transfer carries in its own encoding
15 constant K-THROW-ENTRY              \ the refusal's `throw`, under a key of its own
16 constant K-DATA-OFFSET

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
variable N-REMOVED                   \ unused data-stack reads removed
variable B-BASE                      \ operations of the module before the current block
variable PLAN-OPS                    \ operations the sealed plan covers
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

\ One cell per operation of the module: whether the walk writes it, and what a
\ removed transfer hands back to the operation in front of it.
DYNAMIC-BUFFER REMOVED-BUF n
: REMOVED ( -- ptr n ) 0 REMOVED-BUF ;
DYNAMIC-BUFFER ABSORB-BUF n
: ABSORB ( -- ptr n ) 0 ABSORB-BUF ;
: RESERVE-PLAN ( -- )
   OPS-MAX REMOVED-BUF-RESERVE
   OPS-MAX ABSORB-BUF-RESERVE ;

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
   dup 0 < if E-A64PRUNE-OPCODE throw then ;

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
   dup 0 < if E-A64PRUNE-OPCODE throw then ;

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
   dup 0 < over VMAX >= or if E-A64PRUNE-CAP throw then ;

: VBIND ( IR-ID:ir-value-id IR-ID:ir-value-id -- )
   {: src:IR-ID:ir-value-id new:IR-ID:ir-value-id :}
   src VSLOT {: k:n :}
   new k VMAP !
   1 k cells VSET + ! ;

: VOF ( IR-ID:ir-value-id -- IR-ID:ir-value-id )
   VSLOT {: k:n :}
   k cells VSET + @ 0= if E-A64PRUNE-SHAPE throw then
   k VMAP @ ;

\ ---- reading the frozen module -----------------------------------------------
: SRC-CK ( IR-ID:ir-source-id -- )
   IR-ID:SOURCE-LOCAL 0<> if E-A64PRUNE-SHAPE throw then ;

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
   E-A64PRUNE-SHAPE throw ;

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

\ The whole block's plan, read once before a single operation of it is copied,
\ so the walk and the operation it reaches later agree about what was decided.
\
\ A data-stack load whose loaded cell nothing reads is the one operation this
\ pass removes. The residency pass decides which cells a block enters holding,
\ before the if-conversion decides which comparisons it will write, and a
\ conversion whose arms all hand the join the same value writes no comparison at
\ all - so the cell that comparison would have read was loaded for a reader that
\ was never written. The load's memory-order result IS read, by the access after
\ it, so the row is not dropped: its order result is bound to the order it took,
\ and the chain closes over it.
: UNUSED-DLOAD? ( IR-ID:ir-op-id -- bool ) {: id:IR-ID:ir-op-id :}
   id OP-SLOT {: s:n :}
   s A64IR-OPCODE:DLOAD A64IR:ORD =
   s A64IR-OPCODE:FDLOAD A64IR:ORD = or 0= if false exit then
   id 0 RESULT-AT USES-OF 0= ;

\ The same load with a pointer move folded into it. Removing it may not remove
\ the move, and a fused form has nowhere to keep one it does not transfer for -
\ so the operation in FRONT of it takes the move back, which is where selection
\ found it. That operation is the take this load rode on or the call it came
\ back from, and each of those is the first operation of its own sequence, so
\ there is always one in front.
: UNUSED-DPOP? ( IR-ID:ir-op-id -- bool ) {: id:IR-ID:ir-op-id :}
   id OP-SLOT {: s:n :}
   s A64IR-OPCODE:DPOP A64IR:ORD =
   s A64IR-OPCODE:FDPOP A64IR:ORD = or 0= if false exit then
   id 0 RESULT-AT USES-OF 0= ;

: DWB-OF ( IR-ID:ir-op-id -- n ) {: id:IR-ID:ir-op-id :}
   0
   id ATTRS-OF 0 ?do
      id i ATTR-KEY-AT KEY-SLOT-OF K-DWB = if
         drop id i ATTR-INT-AT leave
      then
   loop ;

: PLAN-REMOVE ( n n -- )
   {: g:n at:n :}
   1 g at + cells REMOVED + !
   1 PLAN-REMOVED +! ;

: PLAN-BLOCK ( IR-ID:ir-block-id -- )
   {: bk:IR-ID:ir-block-id :}
   bk OP-COUNT {: n:n :}
   B-BASE @ {: g:n :}
   g 0 < g OPS-MAX > or if E-A64PRUNE-CAP throw then
   n 0 < n OPS-MAX g - > or if E-A64PRUNE-CAP throw then
   n 0 ?do
      0 g i + cells REMOVED + !
      0 g i + cells ABSORB + !
   loop
   n 0 ?do
      bk i OP-AT UNUSED-DLOAD? if g i PLAN-REMOVE then
      bk i OP-AT UNUSED-DPOP? if
         i 0= if E-A64PRUNE-SHAPE throw then
         g i PLAN-REMOVE
         bk i OP-AT DWB-OF  g i + 1- cells ABSORB + !
      then
   loop
   g n + B-BASE ! ;

\ Read at the same module-wide position the plan was written at: the walk visits
\ functions, blocks and operations in the order the plan pass did.
: REMOVED? ( n -- bool )
   B-BASE @ + cells REMOVED + @ 0<> ;

: ABSORB-AT ( n -- n )
   B-BASE @ + cells ABSORB + @ ;

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
\ Which key a handed-back move lands under: a call makes its take under
\ `a64.dback` and everything else under `a64.dbytes`, and both count the same
\ way, so the one number goes to whichever of the two the operation holds.
: HAS-KEY? ( IR-ID:ir-op-id n -- bool )
   {: id:IR-ID:ir-op-id want:n :}
   false
   id ATTRS-OF 0 ?do
      id i ATTR-KEY-AT KEY-SLOT-OF want = if drop true leave then
   loop ;

: COPY-ATTRS ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id back:n :}
   id K-DBACK HAS-KEY? if 0 back else back 0 then {: bytes+:n back+:n :}
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
         CTX BLD  CTX BLD A64IR:KEY-DBYTES  CTX BLD v bytes+ + A64IR:DBYTES-ATTR
         IR-BUILD:ADD-ATTR
      then
      k K-DWB = if
         CTX BLD  CTX BLD A64IR:KEY-DWB  CTX BLD v A64IR:DWB-ATTR
         IR-BUILD:ADD-ATTR
      then
      k K-COND = if
         CTX BLD  CTX BLD A64IR:KEY-COND  CTX BLD v A64IR:N>COND A64IR:COND-ATTR
         IR-BUILD:ADD-ATTR
      then
      k K-DBACK = if
         CTX BLD  CTX BLD A64IR:KEY-DBACK  CTX BLD v back+ + A64IR:DBACK-ATTR
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
      k K-THROW-ENTRY = if
         CTX BLD  CTX BLD A64IR:KEY-THROW-ENTRY  CTX BLD v A64IR:ENTRY-ATTR
         IR-BUILD:ADD-ATTR
      then
      k K-FUN = if
         CTX BLD  CTX BLD A64IR:KEY-FUN  CTX BLD v A64IR:FUN-ATTR
         IR-BUILD:ADD-ATTR
      then
      k K-DATA-OFFSET = if
         CTX BLD CTX BLD A64IR:KEY-DATA-OFFSET CTX BLD v A64IR:DATA-OFFSET-ATTR
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

: COPY-OP ( IR-ID:ir-op-id n -- )
   {: id:IR-ID:ir-op-id back:n :}
   id OP-SLOT A64IR:NTH {: o:A64IR:opcode :}
   id o OPEN
   id COPY-OPERANDS
   id COPY-RESULTS
   id COPY-SUCCS
   id back COPY-ATTRS
   id  CLOSE  BIND-RESULTS ;

\ Both removable forms are shaped the same way here - the order arrives as the
\ one operand and leaves as the second result - so the chain closes over the
\ operation whether it was a plain load or a fused one.
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
   n 1 < if E-A64PRUNE-SHAPE throw then
   bk OPEN-BLOCK
   n 0 ?do
      i REMOVED? if
         bk i OP-AT REMOVE-DLOAD
      else
         bk i OP-AT  i ABSORB-AT  COPY-OP
      then
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
   V-SRC VW IR-SOURCE:FSOURCES 1 <> if E-A64PRUNE-SHAPE throw then
   c b  V-SRC VW  MKEY 0 IR-ID:PACK-SOURCE  IR-BUILD:CARRY-SOURCE 0 S-SID ! ;

\ The binding is taken whatever the outcome, so neither a rewrite without a
\ binding nor a refused rewrite can leave one behind for the next caller.
: BND-TAKE ( -- )
   BND-MODE @ {: have:n :}
   BOUND-NO BND-MODE !
   have BOUND-YES <> if E-A64PRUNE-BIND throw then ;

: BND-MODULE-CK ( IR-BUILD:module -- )
   IR-BUILD:FMODULE  0 BND-MOD @  IR-ID:MODULE-SAME?
   0= if E-A64PRUNE-BIND throw then ;

\ The plan the scan sealed, and about this module: a rewrite that planned
\ nothing would walk stale decisions, and one planned for another module would
\ fold operations that are not there.
: PLAN-TAKE ( IR-BUILD:module -- )
   {: m:IR-BUILD:module :}
   PLAN-SET @ {: planned:n :}
   0 PLAN-SET !
   BND-TAKE
   m BND-MODULE-CK
   planned 0= if E-A64PRUNE-PLAN throw then
   m IR-BUILD:FMODULE  0 PLAN-MOD @  IR-ID:MODULE-SAME?
   0= if E-A64PRUNE-PLAN throw then ;

: DIALECT-CK ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b  c b IR-BUILD:DIALECT@  A64IR:NAME IR-BUILD:SYMBOL-IS?
   0= if E-A64PRUNE-BIND throw then
   c b IR-BUILD:SCHEMA-MAJOR@ A64IR:MAJOR <> if E-A64PRUNE-BIND throw then
   c b IR-BUILD:SCHEMA-MINOR@ A64IR:MINOR <> if E-A64PRUNE-BIND throw then ;

public

\ ---- binding the dialect -----------------------------------------------------
\ The only moment a module can be asked its operation, key and type identities,
\ because its symbols and types are its own ordinals.
: BIND-DIALECT ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   BND-MODE @ BOUND-YES = if E-A64PRUNE-BIND throw then
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
   c b A64IR:KEY-DWB    K-DWB BND-KEY !
   c b A64IR:KEY-COND   K-COND BND-KEY !
   c b A64IR:KEY-DBACK  K-DBACK BND-KEY !
   c b A64IR:KEY-ENTRY  K-ENTRY BND-KEY !
   c b A64IR:KEY-OFF    K-OFF BND-KEY !
   c b A64IR:KEY-MASK   K-MASK BND-KEY !
   c b A64IR:KEY-TRAP-ENTRY K-TRAP-ENTRY BND-KEY !
   c b A64IR:KEY-THROW-ENTRY K-THROW-ENTRY BND-KEY !
   c b A64IR:KEY-FUN    K-FUN BND-KEY !
   c b A64IR:KEY-DATA-OFFSET K-DATA-OFFSET BND-KEY !
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
   BOUND? 0= if E-A64PRUNE-BIND throw then
   m BND-MODULE-CK
   m VIEWS!
   RESERVE-SCRATCH RESERVE-PLAN
   COUNT-USES
   0 PLAN-REMOVED !
   0 B-BASE !
   FUN-COUNT 0 ?do
      MKEY i IR-ID:PACK-FUN {: f:IR-ID:ir-fun-id :}
      f BLOCK-COUNT 0 ?do
         f i BLOCK-AT PLAN-BLOCK
      loop
   loop
   B-BASE @ PLAN-OPS !
   m IR-BUILD:FMODULE 0 PLAN-MOD !
   1 PLAN-SET !
   PLAN-REMOVED @ ;

public
: REWRITES ( IR-BUILD:module -- n )
   PLAN! ;

\ ---- the pass ----------------------------------------------------------------
\ The source is carried from the old module, so this pass is never handed the
\ text and cannot be handed the wrong text.
: REWRITE ( IR-CTX:ctx IR-BUILD:module IR-BUILD:builder -- IR-BUILD:module )
   {: c:IR-CTX:ctx m:IR-BUILD:module b:IR-BUILD:builder :}
   m PLAN-TAKE
   0 N-REMOVED !
   c 0 S-CTX !
   b 0 S-BLD !
   m VIEWS!
   NFROZEN:TOTAL-OPS NPROF-PHASE:PRUNE-OPS NPROF:ADD
   c b SOURCE!
   0 B-BASE !
   FUN-COUNT 0 ?do MKEY i IR-ID:PACK-FUN WALK-FUN loop
   B-BASE @ PLAN-OPS @ <> if E-A64PRUNE-SHAPE throw then
   c b IR-BUILD:FREEZE ;

\ A caller compares it with what the scan promised, so a walk that removed a
\ different number is a refusal rather than a module nobody checked.
: REWRITTEN ( -- n )
   N-REMOVED @ ;

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

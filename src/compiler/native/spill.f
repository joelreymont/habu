\ spill.f - build the machine module in which the register allocator's spill
\ decisions are real store and load operations.
\
\ A frozen module cannot gain an operation and a builder cannot gain one in the
\ middle, so this reads a frozen module and writes a new one. The alternative -
\ the emitter materialising the stores out of the allocator's claims - would
\ leave the validator checking the allocator's belief against itself.
\
\ Each function keeps its own invocation frame. An existing frame is resized;
\ a frameless function gains a reserve and, when it returns, a release. The
\ frame-size contract remains common to all functions in the module.
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
require src/compiler/native/prof.f

package A64SPILL
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
A64IR-OPCODE:RESERVE  A64IR:ORD constant O-RESERVE
A64IR-OPCODE:RELEASE  A64IR:ORD constant O-RELEASE
A64IR-OPCODE:LINKSAVE A64IR:ORD constant O-LINKSAVE
A64IR-OPCODE:LINKLOAD A64IR:ORD constant O-LINKLOAD
A64IR-OPCODE:TRAP     A64IR:ORD constant O-TRAP
A64IR-OPCODE:MOV      A64IR:ORD constant O-MOV

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
-1 constant NO-SLOT

\ A name is copied out of the old module's interner and interned into the new
\ one, because the two modules number their symbols separately.
128 constant NAME-CAP

here CELL 1- and CELL swap - CELL 1- and allot
variable BND-MODE
BOUND-NO BND-MODE !
variable N-CUR                       \ how far through the plan the walk has read
variable FRAME-N
variable G-AT                        \ operations of the whole function copied so far
variable PRO-N                       \ nonzero when this function has a prologue
variable N-RES                       \ frame reserves, releases, link saves and
variable N-REL                       \ link restores the old module already holds
variable N-SAV
variable N-LDL
variable OLD-BBASE
variable CUR-B

1 TYPED-BUFFER BND-MOD IR-ID:ir-module-id
A64IR:OPCODES TYPED-BUFFER BND-OP IR-ID:ir-symbol-id
KEYS-N TYPED-BUFFER BND-KEY IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-GPR IR-ID:ir-type-id
1 TYPED-BUFFER BND-MEM IR-ID:ir-type-id
1 TYPED-BUFFER BND-FPR IR-ID:ir-type-id

1 TYPED-BUFFER S-CTX IR-CTX:ctx
1 TYPED-BUFFER S-BLD IR-BUILD:builder
1 TYPED-BUFFER S-SID IR-ID:ir-source-id
1 TYPED-BUFFER S-TOK IR-ID:ir-value-id
DYNAMIC-BUFFER F-ORDER IR-ID:ir-value-id
DYNAMIC-BUFFER F-ORDER-SET-BUF n
: F-ORDER-SET ( -- ptr n ) 0 F-ORDER-SET-BUF ;
DYNAMIC-BUFFER F-NEED-BUF n
: F-NEED ( -- ptr n ) 0 F-NEED-BUF ;

\ One flag per VALUE of the module: a frame access of the old module reads it.
\ Module-wide, because value identities are.
DYNAMIC-BUFFER F-READ-BUF n
: F-READ ( -- ptr n ) 0 F-READ-BUF ;

\ One flag per BLOCK of the module: the sealed plan puts a store or a reload in
\ it. Read by block, so the plan is walked once instead of once per block.
DYNAMIC-BUFFER P-FRAME-BUF n
: P-FRAME ( -- ptr n ) 0 P-FRAME-BUF ;

\ Two flags per block of the FUNCTION being rewritten: whether anything reaches
\ it, and whether every terminator that does leaves through one edge. Both are
\ read per block and per edge, so the terminators are walked once.
DYNAMIC-BUFFER PRED-ANY-BUF n
: PRED-ANY ( -- ptr n ) 0 PRED-ANY-BUF ;
DYNAMIC-BUFFER PRED-ONE-BUF n
: PRED-ONE ( -- ptr n ) 0 PRED-ONE-BUF ;

\ The frame lane each block of the function carries, or none. The walk that
\ answers it reads only the old module, so it is answered once per block rather
\ than once per ask.
DYNAMIC-BUFFER F-LANE-BUF n
: F-LANE ( -- ptr n ) 0 F-LANE-BUF ;
variable PRED-FUN                    \ the function the four per-function maps hold
variable PRED-SET                    \ those maps are built
DYNAMIC-BUFFER VMAP IR-ID:ir-value-id
DYNAMIC-BUFFER RMAP IR-ID:ir-value-id
DYNAMIC-BUFFER VSET-BUF n
: VSET ( -- ptr n ) 0 VSET-BUF ;
\ A value marked for re-emission is written again where it is read, out of its
\ defining operation's own immediate, so the pass has to reach it from the value.
DYNAMIC-BUFFER DOP IR-ID:ir-op-id
DYNAMIC-BUFFER RPOS-BUF n
: RPOS ( -- ptr n ) 0 RPOS-BUF ;
DYNAMIC-BUFFER RBLK-BUF n
: RBLK ( -- ptr n ) 0 RBLK-BUF ;

: RESERVE-SCRATCH ( -- )
   SCRATCH-SIZES!
   BMAX F-ORDER-RESERVE
   BMAX F-ORDER-SET-BUF-RESERVE
   BMAX F-NEED-BUF-RESERVE
   VMAX VMAP-RESERVE
   VMAX RMAP-RESERVE
   VMAX VSET-BUF-RESERVE
   VMAX DOP-RESERVE
   VMAX RPOS-BUF-RESERVE
   VMAX RBLK-BUF-RESERVE
   VMAX F-READ-BUF-RESERVE
   BMAX P-FRAME-BUF-RESERVE
   BMAX PRED-ANY-BUF-RESERVE
   BMAX PRED-ONE-BUF-RESERVE
   BMAX F-LANE-BUF-RESERVE
   ;
create NAMEBUF NAME-CAP allot

\ ---- the slots, read back ----------------------------------------------------
: CTX ( -- IR-CTX:ctx )              0 S-CTX @ ;
: BLD ( -- IR-BUILD:builder )        0 S-BLD @ ;
: SID ( -- IR-ID:ir-source-id )      0 S-SID @ ;
: TOK ( -- IR-ID:ir-value-id )       0 S-TOK @ ;
: TOK! ( IR-ID:ir-value-id -- )      0 S-TOK ! ;

: F-ORDER-CLEAR ( -- )
   BMAX 0 ?do 0 i cells F-ORDER-SET + ! loop ;

: F-ORDER@ ( n -- IR-ID:ir-value-id )
   dup 0 < over BMAX >= or if E-A64SPILL-SHAPE throw then
   F-ORDER @ ;

: F-ORDER! ( IR-ID:ir-value-id n -- )
   dup 0 < over BMAX >= or if E-A64SPILL-SHAPE throw then
   {: v:IR-ID:ir-value-id b:n :}
   v b F-ORDER !
   1 b cells F-ORDER-SET + ! ;

: F-ORDER-SET? ( n -- bool )
   dup 0 < over BMAX >= or if E-A64SPILL-SHAPE throw then
   cells F-ORDER-SET + @ 0<> ;

\ One flag per block of the function being rewritten: set when a frame access is
\ reachable from that block, so an order threaded into it is one somebody reads.
: F-NEED? ( n -- bool )
   dup 0 < over BMAX >= or if E-A64SPILL-SHAPE throw then
   cells F-NEED + @ 0<> ;

: F-NEED! ( n -- )
   dup 0 < over BMAX >= or if E-A64SPILL-SHAPE throw then
   1 swap cells F-NEED + ! ;

: F-NEED-CLEAR ( -- )
   BMAX 0 ?do 0 i cells F-NEED + ! loop ;

\ ---- the machine operation family --------------------------------------------
\ An operation of a form outside the family has no rule here and is refused
\ rather than copied blind.
: OPCODE-SLOT ( IR-ID:ir-symbol-id -- n )
   {: sym:IR-ID:ir-symbol-id :}
   -1
   A64IR:OPCODES 0 ?do
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
      -1 i cells RBLK + !
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
   pos k cells RPOS + !
   CUR-B @ k cells RBLK + ! ;

: READ-AS ( IR-ID:ir-value-id n -- IR-ID:ir-value-id )
   {: id:IR-ID:ir-value-id pos:n :}
   id VSLOT {: k:n :}
   k cells RBLK + @ CUR-B @ =
   k cells RPOS + @ pos <= and if k RMAP @ exit then
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
   CTX BLD  CTX BLD o A64IR:ENSURE-OP  IR-BUILD:BEGIN-OP
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
      k K-FRAME = if FRAME-N @ FRAME-ATTR+ then
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
\ Plan blocks are module ordinals; the walk uses function-local block numbers.
: HERE? ( n n -- bool )
   {: b:n at:n :}
   N-CUR @ A64RA:PLAN-N >= if false exit then
   N-CUR @ A64RA:PLAN-BLOCK@ b OLD-BBASE @ + = if
      N-CUR @ A64RA:PLAN-POS@ at =
   else false then ;

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
: SUCC-ORD ( IR-ID:ir-op-id n -- n )
   SUCC-AT IR-ID:BLOCK-LOCAL  OLD-BBASE @ -
   dup 0 < over BMAX >= or if E-A64SPILL-SHAPE throw then ;

\ Every edge of the function, read once: which blocks anything reaches, and
\ which are reached only by terminators that leave through one edge. Asking a
\ block instead walked every terminator of the function again.
: PREDS! ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   f BLOCK-COUNT {: n:n :}
   n 0 ?do
      0 i cells PRED-ANY + !
      1 i cells PRED-ONE + !
   loop
   n 0 ?do
      f i BLOCK-AT TERM-AT {: t:IR-ID:ir-op-id :}
      t SUCCS-OF {: sn:n :}
      sn 0 ?do
         t i SUCC-ORD {: b:n :}
         1 b cells PRED-ANY + !
         sn 1 <> if 0 b cells PRED-ONE + ! then
      loop
   loop
   f IR-ID:FUN-LOCAL PRED-FUN !
   1 PRED-SET ! ;

\ The two maps are the function's, so a block of another one is a refusal rather
\ than a flag read at that block's ordinal.
: PRED-CK ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   PRED-SET @ 0= if E-A64SPILL-SHAPE throw then
   f IR-ID:FUN-LOCAL PRED-FUN @ <> if E-A64SPILL-SHAPE throw then ;

\ A single-successor edge can carry an order value as a block argument. A
\ multi-successor edge cannot, so its destination must use the side-table order.
: ONE-SUCC-IN? ( IR-ID:ir-fun-id IR-ID:ir-block-id -- bool )
   {: f:IR-ID:ir-fun-id want:IR-ID:ir-block-id :}
   f PRED-CK
   want IR-ID:BLOCK-LOCAL OLD-BBASE @ - {: b:n :}
   b 0 < b BMAX >= or if E-A64SPILL-SHAPE throw then
   b cells PRED-ANY + @ 0<>  b cells PRED-ONE + @ 0<>  and ;

: PLAN-FRAME? ( n -- bool )
   dup A64RA:PLAN-STORE? if drop true exit then
   dup A64RA:PLAN-MOVE? if drop false exit then
   A64RA:PLAN-REMAT? 0= ;            \ the remaining plan kind is reload

\ The whole plan, read once: which blocks it puts a store or a reload in. Asking
\ a block instead walked the whole plan again, once per block of every function.
: P-FRAMES! ( -- )
   BMAX 0 ?do 0 i cells P-FRAME + ! loop
   A64RA:PLAN-N 0 ?do
      i PLAN-FRAME? if
         i A64RA:PLAN-BLOCK@ {: b:n :}
         b 0 < b BMAX >= or if E-A64SPILL-PLAN throw then
         1 b cells P-FRAME + !
      then
   loop ;

: PLAN-FRAME-IN? ( n -- bool )
   OLD-BBASE @ +
   dup 0 < over BMAX >= or if E-A64SPILL-SHAPE throw then
   cells P-FRAME + @ 0<> ;

\ A block nothing reaches the frame from carries no order, so none is minted for
\ it: the lane a no-return arm would be handed is one no operation ever reads.
: SYNTH-FRAME-ARG? ( IR-ID:ir-fun-id IR-ID:ir-block-id n -- bool )
   {: f:IR-ID:ir-fun-id bk:IR-ID:ir-block-id b:n :}
   b F-NEED? 0= if false exit then
   f bk ONE-SUCC-IN? ;

-1 constant NO-FRAME-ARG

\ Every operand of every frame access of the module, read once: the values a
\ frame access consumes directly. Asking a value instead walked every operation
\ of the module again, once per block argument of every block.
: F-READS! ( -- )
   VMAX 0 ?do 0 i cells F-READ + ! loop
   TOTAL-OPS 0 ?do
      MKEY i IR-ID:PACK-OP {: id:IR-ID:ir-op-id :}
      id FRAME-TOUCH? if
         id OPERANDS-OF 0 ?do
            1 id i OPERAND-AT VSLOT cells F-READ + !
         loop
      then
   loop ;

\ An argument may dominate a frame access after a conditional edge without
\ being forwarded as another block argument. Value identities are module-wide,
\ so this asks the module-wide map of direct frame consumers.
: DIRECT-FRAME-ARG? ( IR-ID:ir-value-id -- bool )
   {: a:IR-ID:ir-value-id :}
   a MEM-VALUE? 0= if false exit then
   a VSLOT cells F-READ + @ 0<> ;

: FRAME-ARG-PATH? ( IR-ID:ir-block-id IR-ID:ir-value-id n -- bool )
   {: bk:IR-ID:ir-block-id a:IR-ID:ir-value-id fuel:n :}
   a DIRECT-FRAME-ARG? if true exit then
   fuel 0= if false exit then
   bk TERM-AT {: id:IR-ID:ir-op-id :}
   id SUCCS-OF 1 <> if false exit then
   id 0 SUCC-AT {: sb:IR-ID:ir-block-id :}
   false
   id OPERANDS-OF 0 ?do
      id i OPERAND-AT a SAME-VALUE?  i sb ARG-COUNT < and if
         sb sb i ARG-AT fuel 1- recurse or
      then
   loop ;

: FRAME-ARG? ( IR-ID:ir-block-id IR-ID:ir-value-id -- bool )
   BMAX FRAME-ARG-PATH? ;

\ A prior lowering's frame lane is identified by a frame consumer or by the
\ terminator that forwards it, never by its position among unrelated arguments.
: FRAME-LANE ( IR-ID:ir-block-id -- n )
   {: bk:IR-ID:ir-block-id :}
   NO-FRAME-ARG
   bk ARG-COUNT 0 ?do
      bk i ARG-AT {: a:IR-ID:ir-value-id :}
      bk a FRAME-ARG? if
         a IR-ID:VALUE-LOCAL {: x:n :}
         dup NO-FRAME-ARG <> over x <> and if E-A64SPILL-SHAPE throw then
         drop x
      then
   loop ;

\ The lane is read off the old module, which this pass does not write, so every
\ block of the function is asked once and the walk reads the answer. The block
\ opening, both edge tests and the operand copy all ask the same blocks.
: F-LANES! ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   f BLOCK-COUNT 0 ?do
      f i BLOCK-AT FRAME-LANE  i cells F-LANE + !
   loop ;

: FRAME-ARG ( IR-ID:ir-block-id -- n )
   {: bk:IR-ID:ir-block-id :}
   bk IR-ID:BLOCK-LOCAL OLD-BBASE @ - {: b:n :}
   b 0 < b BMAX >= or if E-A64SPILL-SHAPE throw then
   b cells F-LANE + @ ;

: F-ORDER-SAME? ( n -- bool )
   F-ORDER@ IR-ID:VALUE-LOCAL  TOK IR-ID:VALUE-LOCAL = ;

: F-ORDER-EDGE! ( IR-ID:ir-fun-id IR-ID:ir-op-id n -- )
   {: f:IR-ID:ir-fun-id id:IR-ID:ir-op-id i:n :}
   id i SUCC-AT {: sb:IR-ID:ir-block-id :}
   id i SUCC-ORD {: b:n :}
   sb FRAME-ARG NO-FRAME-ARG <>  f sb b SYNTH-FRAME-ARG? or if exit then
   b F-NEED? 0= if exit then
   b F-ORDER-SET? 0= if TOK b F-ORDER! exit then
   b F-ORDER-SAME? 0= if E-A64SPILL-SHAPE throw then ;

: F-ORDER-ENTER ( n -- )
   {: b:n :}
   b 0= if exit then
   b F-ORDER-SET? 0= if E-A64SPILL-SHAPE throw then
   b F-ORDER@ TOK! ;

: COPY-SUCCS ( IR-ID:ir-fun-id IR-ID:ir-op-id bool -- )
   {: f:IR-ID:ir-fun-id id:IR-ID:ir-op-id carry:bool :}
   id SUCCS-OF {: n:n :}
   n 0 ?do
      carry if f id i F-ORDER-EDGE! then
      CTX BLD
      BLD IR-BUILD:MODULE-KEY  id i SUCC-AT IR-ID:BLOCK-LOCAL  IR-ID:PACK-BLOCK
      IR-BUILD:ADD-SUCCESSOR
   loop ;

: SPILLED? ( IR-ID:ir-value-id -- bool )
   VSLOT A64RA:SLOT@ NO-SLOT <> ;

: SPILL-SLOT ( IR-ID:ir-value-id -- n )
   VSLOT A64RA:SLOT@ ;

\ The copy A64RA planned nothing for. Coalescing put its two ends in one class
\ and the class went to the frame, so operand and result name one slot and the
\ copy moves that slot onto itself. Every operation that reads the result reads
\ it out of that slot through a reload of its own, so the copy carries nothing
\ across and is left out of the rewritten module; carrying it would need the
\ reload and the store-back the plan no longer holds.
: IDENTITY-COPY? ( IR-ID:ir-op-id -- bool )
   {: id:IR-ID:ir-op-id :}
   id OPCODE-AT  O-MOV BND-OP @  SAME-SYM? 0= if false exit then
   id 0 OPERAND-AT SPILL-SLOT {: k:n :}
   k NO-SLOT = if false exit then
   k  id 0 RESULT-AT SPILL-SLOT  = ;

: FRAME-EDGE-OPERAND? ( IR-ID:ir-op-id n -- bool )
   {: id:IR-ID:ir-op-id i:n :}
   id SUCCS-OF 1 <> if false exit then
   id 0 SUCC-AT {: sb:IR-ID:ir-block-id :}
   i sb ARG-COUNT >= if false exit then
   sb FRAME-ARG {: fa:n :}
   fa NO-FRAME-ARG = if false exit then
   sb i ARG-AT IR-ID:VALUE-LOCAL fa = ;

\ A one-successor terminator's operands are only its destination arguments.
\ A spilled lane already lives in the shared frame slot on every path, so the
\ rewritten CFG carries neither that operand nor its matching block argument.
: SPILLED-EDGE? ( IR-ID:ir-op-id n -- bool )
   {: id:IR-ID:ir-op-id i:n :}
   id SUCCS-OF 1 <> if false exit then
   id 0 SUCC-AT {: sb:IR-ID:ir-block-id :}
   i sb ARG-COUNT >= if false exit then
   sb i ARG-AT SPILL-SLOT {: as:n :}
   id i OPERAND-AT SPILL-SLOT {: os:n :}
   as NO-SLOT = if
      os NO-SLOT <> if E-A64SPILL-SHAPE throw then
      false exit
   then
   os NO-SLOT =  os as <> or if E-A64SPILL-SHAPE throw then
   true ;

: COPY-OPERANDS ( IR-ID:ir-op-id n bool -- )
\ An operand that is the frame's own memory order is replaced by the order as it
\ stands HERE, because this pass may have put inserts between two neighbours.
   {: id:IR-ID:ir-op-id pos:n frame:bool :}
   id OPERANDS-OF {: n:n :}
   n 0 ?do
      id i OPERAND-AT {: v:IR-ID:ir-value-id :}
      id i SPILLED-EDGE? 0= if
         frame v MEM-VALUE? and  id i FRAME-EDGE-OPERAND? or if
            TOK OPERAND+
         else
            v pos READ-AS OPERAND+
         then
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

: FRAME-EDGE+ ( IR-ID:ir-fun-id IR-ID:ir-op-id bool -- )
   {: f:IR-ID:ir-fun-id id:IR-ID:ir-op-id carry:bool :}
   carry  id SUCCS-OF 1 =  and 0= if exit then
   id 0 SUCC-AT {: sb:IR-ID:ir-block-id :}
   sb FRAME-ARG NO-FRAME-ARG <> if exit then
   id 0 SUCC-ORD {: b:n :}
   f sb b SYNTH-FRAME-ARG? 0= if exit then
   TOK OPERAND+ ;

: COPY-OP ( IR-ID:ir-fun-id IR-ID:ir-op-id n bool -- )
   {: f:IR-ID:ir-fun-id id:IR-ID:ir-op-id pos:n carry:bool :}
   id OPCODE-AT OPCODE-SLOT A64IR:NTH {: o:A64IR:opcode :}
   id FRAME-TOUCH? {: frame:bool :}
   id o OPEN
   id pos frame COPY-OPERANDS
   f id carry FRAME-EDGE+
   id COPY-RESULTS
   f id carry COPY-SUCCS
   id COPY-ATTRS
   id  CLOSE  frame BIND-RESULTS ;

\ ---- the block ---------------------------------------------------------------
\ The value map is NOT cleared here: a value defined in one block is read in the
\ blocks it dominates, so the map belongs to the function.
: OPEN-BLOCK ( IR-ID:ir-fun-id IR-ID:ir-block-id n bool -- )
   {: f:IR-ID:ir-fun-id bk:IR-ID:ir-block-id b:n carry:bool :}
   CTX BLD IR-BUILD:BEGIN-BLOCK
   CTX BLD bk BLOCK-SPAN IR-BUILD:SET-BLOCK-SPAN
   bk ARG-COUNT {: n:n :}
   bk FRAME-ARG {: fa:n :}
   n 0 ?do
      bk i ARG-AT {: a:IR-ID:ir-value-id :}
      a SPILLED? 0= if
         CTX BLD  a TYPE-OF  IR-BUILD:ADD-BLOCK-ARG {: na:IR-ID:ir-value-id :}
         a na VBIND
         a IR-ID:VALUE-LOCAL fa = if na TOK! then
      then
   loop
   carry  b 0<> and  fa NO-FRAME-ARG <> f bk b SYNTH-FRAME-ARG? or and if
      fa NO-FRAME-ARG <> if exit then
      CTX BLD  CTX BLD A64IR:MEM-TYPE  IR-BUILD:ADD-BLOCK-ARG TOK!
      exit
   then
   carry if
      b 0<> if b F-NEED? 0= if exit then then
      b F-ORDER-ENTER
   then ;

: FRAMES? ( -- bool )
   A64RA:SPILLS 0<> PRO-N @ 0= and ;

: CARRY-FRAME? ( -- bool )
   A64RA:SPILLS 0<> ;

: WALK-BLOCK ( IR-ID:ir-fun-id n n -- )
\ The reserve opens the ENTRY block and the release stands in front of the
\ terminator control leaves through - the only pair passed once, in that order.
   {: f:IR-ID:ir-fun-id b:n rb:n :}
   f b BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk OP-COUNT {: n:n :}
   CARRY-FRAME? {: carry:bool :}
   b CUR-B !
   n 1 < if E-A64SPILL-SHAPE throw then
   f bk b carry OPEN-BLOCK
   b 0= FRAMES? and if bk 0 OP-AT EMIT-RESERVE then
   n 0 ?do
      bk i OP-AT {: id:IR-ID:ir-op-id :}
      id b i G-AT @ INSERT-AT
      i n 1- =  b rb =  and  FRAMES?  and if id EMIT-RELEASE then
      id IDENTITY-COPY? 0= if f id G-AT @ carry COPY-OP then
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

\ ---- which blocks the frame order has to reach -------------------------------
\ A block consumes an order when a frame access is reachable from it: one the old
\ module already holds, a store or reload the walk planned, or the release this
\ pass puts in front of the return. The need then runs back along the edges.
: F-NEED-SELF? ( IR-ID:ir-fun-id n n -- bool )
   {: f:IR-ID:ir-fun-id b:n rb:n :}
   b PLAN-FRAME-IN? if true exit then
   b rb =  FRAMES? and if true exit then
   f b BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk OP-COUNT 0 ?do
      bk i OP-AT FRAME-TOUCH? if true unloop exit then
   loop false ;

: F-NEED-SUCC? ( IR-ID:ir-op-id -- bool )
   {: t:IR-ID:ir-op-id :}
   t SUCCS-OF 0 ?do
      t i SUCC-ORD F-NEED? if true unloop exit then
   loop false ;

\ One block: needed already, or newly needed because an edge out of it leads to a
\ block that is.
: F-NEED-STEP ( IR-ID:ir-fun-id n -- bool )
   {: f:IR-ID:ir-fun-id b:n :}
   b F-NEED? if false exit then
   f b BLOCK-AT TERM-AT F-NEED-SUCC? 0= if false exit then
   b F-NEED! true ;

\ DESCENDING BLOCK ORDINAL, which is what makes the fixpoint below converge in a
\ bounded number of passes instead of one pass per block. The need runs BACKWARD
\ along the edges, and the selector emits a block before the blocks it branches
\ to, so visiting high ordinals first carries a need the whole length of a
\ straight-line region in ONE pass; ascending order moved it exactly one block per
\ pass, so a function of 2k+1 blocks in a chain took k passes over every block.
\ Only a back edge - a successor whose ordinal is not above this block's - can
\ still need another pass, so the count is the loop nesting depth and not the
\ block count.
\
\ The ORDER cannot change the answer, only the number of passes: a pass never
\ clears a flag, so the iteration is monotone and its fixpoint is unique.
: F-NEED-PASS ( IR-ID:ir-fun-id n -- bool )
   {: f:IR-ID:ir-fun-id n:n :}
   false
   n 0 ?do
      f  n 1- i -  F-NEED-STEP if drop true then
   loop ;

: F-NEED-FILL ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id rb:n :}
   F-NEED-CLEAR
   f BLOCK-COUNT {: n:n :}
   n 0 ?do f i rb F-NEED-SELF? if i F-NEED! then loop
   begin f n F-NEED-PASS 0= until ;

: WALK-FUN ( IR-ID:ir-fun-id -- )
\ Both are the FUNCTION's: a value is read in the blocks its definition
\ dominates and in no other function, and the counter separates two blocks.
   {: f:IR-ID:ir-fun-id :}
   CTX BLD f FUN-NAME IR-BUILD:BEGIN-FUN
   CTX BLD f FUN-SIG IR-BUILD:SET-SIGNATURE
   CTX BLD  V-FUNR VW f IR-FUN:FLINKAGE@  IR-BUILD:SET-LINKAGE
   CTX BLD  V-FUNR VW f IR-FUN:FVISIBILITY@  IR-BUILD:SET-VISIBILITY
   CTX BLD  V-FUNR VW f IR-FUN:FCONVENTION@  IR-BUILD:SET-CONVENTION
   CTX BLD f FUN-SPAN IR-BUILD:SET-FUN-SPAN
   f RET-ORD {: rb:n :}
   f 0 BLOCK-AT 0 OP-AT OPCODE-AT OPCODE-SLOT O-RESERVE = PRO-N !
   VCLEAR
   F-ORDER-CLEAR
   f 0 BLOCK-AT IR-ID:BLOCK-LOCAL OLD-BBASE !
   f PREDS!
   f F-LANES!
   f rb F-NEED-FILL
   0 G-AT !
   f BLOCK-COUNT 0 ?do f i rb WALK-BLOCK loop
   CTX BLD IR-BUILD:END-FUN drop ;

\ ---- what one rewrite is told ------------------------------------------------
: SOURCE! ( IR-CTX:ctx IR-BUILD:builder ptr u8 n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder p u:n :}
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
   {: id:IR-ID:ir-op-id :}
   id OPCODE-AT OPCODE-SLOT {: k:n :}
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

: FRAMELESS? ( -- bool )
   N-RES @ N-REL @ or N-SAV @ or N-LDL @ or 0= ;

\ A no-return function has the same prologue and no epilogue. This applies to
\ quotation siblings too, even when the enclosing function returns and spills.
: FRAME-SHAPE-CK ( IR-ID:ir-fun-id -- ) {: f:IR-ID:ir-fun-id :}
   N-RES @ 1 <>  N-SAV @ 1 > or if E-A64SPILL-SHAPE throw then
   f RET-ORD NO-RET = if
      N-REL @ N-LDL @ or 0<> if E-A64SPILL-SHAPE throw then
   else
      N-REL @ 1 <>  N-LDL @ N-SAV @ <> or if E-A64SPILL-SHAPE throw then
   then
   f 0 BLOCK-AT 0 OP-AT OPCODE-AT OPCODE-SLOT O-RESERVE <>
   if E-A64SPILL-SHAPE throw then ;

\ The common frame size describes a separate frame in every invocation.
: ONCE-CK ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   f COUNT-FRAME
   FRAMELESS? if exit then
   f FRAME-SHAPE-CK ;

: SHAPE-CK ( -- n )
\ A module with no function is not a routine at all.
   FUN-COUNT {: n:n :}
   n 1 < if E-A64SPILL-SHAPE throw then
   n FMAX > if E-A64SPILL-CAP throw then
   n 0 ?do MKEY i IR-ID:PACK-FUN ONCE-CK loop
   n ;

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

\ Whether a binding is live, for a caller cleaning up after a refused run.
\ Each pass answers for itself; this one needs it because whether its binding was
\ spent depends on whether the walk decided a spill.
: BOUND? ( -- bool )
   BND-MODE @ BOUND-YES = ;

\ Give up a binding without rewriting against it.
: RELEASE ( -- )
   BND-TAKE ;

\ ---- the pass ----------------------------------------------------------------
\ The bytes are the source text the old module was compiled from, proved by
\ digest before any span is carried across.
: REWRITE ( IR-CTX:ctx IR-BUILD:module IR-BUILD:builder ptr u8 n -- IR-BUILD:module )
   {: c:IR-CTX:ctx m:IR-BUILD:module b:IR-BUILD:builder p u:n :}
   BND-TAKE
   m BND-MODULE-CK
   m PLAN-CK
   A64RA:FRAME FRAME-N !
   0 N-CUR !
   c 0 S-CTX !
   b 0 S-BLD !
   m VIEWS!
   NFROZEN:TOTAL-OPS NPROF-PHASE:SPILL-OPS NPROF:ADD
   A64RA:PLAN-N NPROF-PHASE:SPILL-PLAN NPROF:ADD
   RESERVE-SCRATCH
   0 PRED-SET !
   F-READS! P-FRAMES!
   c b p u SOURCE!
   SHAPE-CK {: nf:n :}
   nf 0 ?do MKEY i IR-ID:PACK-FUN WALK-FUN loop
   N-CUR @ A64RA:PLAN-N <> if E-A64SPILL-PLAN throw then
   c b BIND-DIALECT
   c b IR-BUILD:FREEZE ;

public
: RESET-SCRATCH ( -- )
   0 PRED-SET !
   0 SCRATCH-VALUES ! 0 SCRATCH-BLOCKS ! 0 SCRATCH-FUNS ! 0 SCRATCH-OPS ! ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;using
;package

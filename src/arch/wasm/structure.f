\ structure.f - WCTL, the Wasm backend's structured control: one function of a
\ frozen WSTRUCT module written as Wasm's block, loop and if, with the label
\ depth of every branch and the copies every branch's arguments need.
\
\ THE GRAPH IS THE INPUT. WSTRUCT is a control-flow graph (wstruct.f), so the
\ nesting Wasm needs is derived here from a module WSTRUCT:FREEZE verified:
\ every successor lies in its own function and every one-successor branch hands
\ its destination exactly that block's arguments.
\
\ REDUCIBLE CONTROL, BY DOMINATORS. The blocks the entry reaches are numbered
\ in reverse postorder and their immediate dominators found by Cooper, Harvey
\ and Kennedy's iteration ("A Simple, Fast Dominance Algorithm", 2001). An edge
\ to a block numbered no later than its source is a back edge only when that
\ block dominates the source; otherwise control enters a cycle at two blocks,
\ which nesting cannot express, and the function is refused with
\ E-WCTL-IRREDUCIBLE, naming it and the span of a block the cycle is entered
\ at (REFUSED-FUN, REFUSED-SPAN). A block a back edge enters heads a loop; a
\ block more than one forward edge enters is a merge.
\
\ THE CONTROL TREE (Ramsey, "Beyond Relooper", ICFP 2022). A block is written
\ inside a loop when it heads one. Each of its dominator-tree children that is a
\ merge becomes the label of a Wasm block whose end that child follows, the one
\ numbered latest outermost, so every forward branch to it leaves an enclosing
\ block. A back edge continues the enclosing loop its destination heads. A
\ forward branch to a block that is no merge is that block's only way in, so
\ the block is written in place. brz becomes an if whose then arm is its
\ nonzero successor. A label is named by the block it stands for until a branch
\ is written; only then is it counted into a depth. Blocks the entry does not
\ reach are written nowhere.
\
\ BRANCH ARGUMENTS ARE PARALLEL COPIES. Only br carries arguments, so the
\ copies go right before its branch. Every destination is a block argument of
\ its own, so a copy may be written once no waiting copy still reads the local
\ it overwrites; what is left after that is cycles, each broken by saving one
\ destination into a temporary first. A cycle's values share one type and a
\ cycle is written out before the next is broken, so a function needs one
\ temporary per type. A memory token has no local and is not copied, and
\ neither is an argument handed back to itself.
\
\ LOOP ARITHMETIC IS THE FRONTEND'S. A loop here is a cycle the selector built:
\ its counter, limit and exit test are ordinary operations and branches, so the
\ meaning of DO, ?DO, +LOOP and LEAVE arrives already decided.

require lib/prelude.f
require lib/errors.f
require src/compiler/ir/id.f
require src/compiler/ir/source.f
require src/compiler/ir/type.f
require src/compiler/ir/fun.f
require src/compiler/ir/build.f
require src/compiler/native/frozen.f

\ WCTL's codes, -9815..-9819, in the Wasm backend's block -9800..-9829.
-9815 constant E-WCTL-FIRST
-9819 constant E-WCTL-LAST
-9815 constant E-WCTL-IRREDUCIBLE  \ a function whose control enters a cycle at more than one block
-9816 constant E-WCTL-RANGE        \ a block, step or temporary outside the last function structured

package WCTL
public

\ One step of the linear form the encoder writes. open-block's label is the
\ block its end is followed by, open-loop's the block heading it; an if opened
\ on brz's i32 condition has no label a branch names. body is a block's
\ operations before its terminator, final a terminator with no successor
\ (return or unreachable). copy, save and restore set a destination's local
\ from a source, a temporary from a source, and a destination from a
\ temporary. branch leaves for the enclosing label of its block, depth labels
\ out. Neither a close nor the function's end is reached by falling through:
\ every path ends in a branch or a final.
ENUM step 0
   VARIANT open-block FIELD label IR-ID:ir-block-id ;VARIANT
   VARIANT open-loop FIELD label IR-ID:ir-block-id ;VARIANT
   VARIANT open-if FIELD cond IR-ID:ir-value-id ;VARIANT
   VARIANT else-arm ;VARIANT
   VARIANT close ;VARIANT
   VARIANT body FIELD block IR-ID:ir-block-id ;VARIANT
   VARIANT final FIELD op IR-ID:ir-op-id ;VARIANT
   VARIANT copy FIELD dst IR-ID:ir-value-id FIELD src IR-ID:ir-value-id ;VARIANT
   VARIANT save FIELD temp n FIELD src IR-ID:ir-value-id ;VARIANT
   VARIANT restore FIELD dst IR-ID:ir-value-id FIELD temp n ;VARIANT
   VARIANT branch FIELD label IR-ID:ir-block-id FIELD depth n ;VARIANT
;ENUM

private

\ ---- the function being structured ------------------------------------------
\ Every row is indexed by a block's ordinal within its function.
1 TYPED-BUFFER CUR IR-ID:ir-fun-id
variable ORIGIN                      \ the module ordinal of the function's entry
variable NB                          \ blocks in the function
variable NREACH                      \ blocks the entry reaches
variable READY                       \ blocks of the last function structured whole
variable LABELS                      \ labels open where the next step goes
variable NSTEPS
variable NTEMPS
variable NCOPIES
variable SP                          \ the depth-first walk's stack height
variable NPOST
variable MOVED                       \ did a dominator round change anything

DYNAMIC-BUFFER RPO n                 \ reverse-postorder number, -1 unreached
DYNAMIC-BUFFER ORDER n               \ the block a number names
DYNAMIC-BUFFER DOM n                 \ immediate dominator, -1 unknown
DYNAMIC-BUFFER FWD n                 \ forward edges in from reached blocks
DYNAMIC-BUFFER HEAD bool             \ a back edge enters it
DYNAMIC-BUFFER POS n                 \ where its open label sits, -1 none
DYNAMIC-BUFFER STK-B n               \ the walk's blocks
DYNAMIC-BUFFER STK-K n               \ and the next successor each will try
DYNAMIC-BUFFER STEP-ROW step
DYNAMIC-BUFFER TEMP-ROW IR-ID:ir-type-id

\ One edge's waiting copies.
DYNAMIC-BUFFER CP-DST IR-ID:ir-value-id
DYNAMIC-BUFFER CP-SRC IR-ID:ir-value-id
DYNAMIC-BUFFER CP-VIA bool           \ reads the temporary, not its source
DYNAMIC-BUFFER CP-WAIT bool          \ not written yet
DYNAMIC-BUFFER CP-USES n             \ waiting copies that read its destination

\ What the last refusal named.
1 TYPED-BUFFER REF-FUN IR-ID:ir-symbol-id
1 TYPED-BUFFER REF-SPAN IR-SOURCE:span

-1 constant UNSEEN                   \ RPO before the walk reaches a block
-2 constant WALKING                  \ RPO while the walk is inside it

\ ---- the graph, read through NFROZEN ------------------------------------------
: BLK ( n -- IR-ID:ir-block-id )
   0 CUR @ swap NFROZEN:BLOCK-AT ;

\ A verified module keeps every successor in its own function's window.
: ORD ( IR-ID:ir-block-id -- n )
   IR-ID:BLOCK-LOCAL ORIGIN @ - ;

: TERM ( n -- IR-ID:ir-op-id )
   BLK NFROZEN:TERM-AT ;

: SUCCS ( n -- n )
   TERM NFROZEN:SUCCS-OF ;

: SUCC ( n n -- n )
   {: b:n k:n :}
   b TERM k NFROZEN:SUCC-AT ORD ;

: RESERVE ( n -- )
   {: cnt:n :}
   cnt RPO-RESERVE  cnt ORDER-RESERVE  cnt DOM-RESERVE  cnt FWD-RESERVE
   cnt HEAD-RESERVE  cnt POS-RESERVE  cnt STK-B-RESERVE  cnt STK-K-RESERVE
   cnt 0 ?do
      UNSEEN i RPO !  -1 i DOM !  0 i FWD !  false i HEAD !  -1 i POS !
   loop ;

\ ---- numbering: reverse postorder from the entry -----------------------------
: PUSH ( n -- )
   {: b:n :}
   b SP @ STK-B !
   0 SP @ STK-K !
   1 SP +!
   WALKING b RPO ! ;

\ Try the top block's next successor, or finish the block when none is left.
: ADVANCE ( -- )
   SP @ 1- {: top:n :}
   top STK-B @ {: b:n :}
   top STK-K @ {: k:n :}
   k b SUCCS < if
      k 1+ top STK-K !
      b k SUCC {: y:n :}
      y RPO @ UNSEEN = if y PUSH then
      exit
   then
   b NPOST @ ORDER !
   1 NPOST +!
   -1 SP +! ;

\ The postorder, reversed in place and numbered.
: RENUMBER ( -- )
   NREACH @ 2 / 0 ?do
      NREACH @ 1- i - {: k:n :}
      i ORDER @ {: a:n :}
      k ORDER @ i ORDER !
      a k ORDER !
   loop
   NREACH @ 0 ?do i  i ORDER @ RPO ! loop ;

: WALK ( -- )
   0 SP !
   0 NPOST !
   0 PUSH
   begin SP @ 0 > while ADVANCE repeat
   NPOST @ NREACH !
   RENUMBER ;

\ ---- dominators ----------------------------------------------------------------
\ Walk a up the dominator tree until it is numbered no later than b.
: CLIMB ( n n -- n )
   {: a:n b:n :}
   a begin dup RPO @ b RPO @ > while DOM @ repeat ;

: MEET-STEP ( n n -- n n )
   {: a:n b:n :}
   a b CLIMB {: c:n :}
   c  b c CLIMB ;

: MEET ( n n -- n )
   begin 2dup <> while MEET-STEP repeat drop ;

\ The meet over the predecessors that already have a dominator. A reached
\ block's walk parent is numbered before it, so there is always one.
: PRED-MEET ( n -- n )
   {: b:n :}
   -1
   b BLK NFROZEN:PRED-COUNT 0 ?do
      b BLK i NFROZEN:PRED-AT ORD {: p:n :}
      p DOM @ 0 >= if
         dup 0 < if drop p else p MEET then
      then
   loop ;

: ROUND ( -- )
   NREACH @ 1 ?do
      i ORDER @ {: b:n :}
      b PRED-MEET {: d:n :}
      d b DOM @ <> if d b DOM !  true MOVED ! then
   loop ;

: DOMINATORS ( -- )
   0 0 DOM !
   begin false MOVED ! ROUND MOVED @ 0= until ;

\ ---- back edges, loop headers and merges --------------------------------------
: IRREDUCIBLE ( n -- )
   {: y:n :}
   NFROZEN:V-FUNR NFROZEN:VW NFROZEN:MKEY 0 CUR @ IR-FUN:FSYMBOL@ 0 REF-FUN !
   NFROZEN:V-BLKR NFROZEN:VW NFROZEN:MKEY y BLK IR-FUN:FBLOCK-SPAN@ 0 REF-SPAN !
   E-WCTL-IRREDUCIBLE throw ;

: EDGE ( n n -- )
   {: x:n y:n :}
   y RPO @ x RPO @ > if y FWD @ 1+ y FWD ! exit then
   x y CLIMB y <> if y IRREDUCIBLE then
   true y HEAD ! ;

\ Only reached blocks' edges count: an unreached block constrains nothing.
: EDGES ( -- )
   NREACH @ 0 ?do
      i ORDER @ {: x:n :}
      x SUCCS 0 ?do x  x i SUCC  EDGE loop
   loop ;

: ANALYSE ( IR-BUILD:module IR-ID:ir-fun-id -- )
   {: m:IR-BUILD:module f:IR-ID:ir-fun-id :}
   m NFROZEN:VIEWS!
   f 0 CUR !
   f 0 NFROZEN:BLOCK-AT IR-ID:BLOCK-LOCAL ORIGIN !
   f NFROZEN:BLOCK-COUNT NB !
   NB @ RESERVE
   WALK
   DOMINATORS
   EDGES ;

\ ---- one br's parallel copies ---------------------------------------------------
: PUT ( WCTL:step -- )
   {: s:WCTL:step :}
   NSTEPS @ 1+ STEP-ROW-RESERVE
   s NSTEPS @ STEP-ROW !
   1 NSTEPS +! ;

: TOKEN? ( IR-ID:ir-value-id -- bool )
   NFROZEN:VALUE-TYPE-AT {: t:IR-ID:ir-type-id :}
   NFROZEN:V-TYPR NFROZEN:VW t IR-TYPE:FKIND@
   IR--TYPE-KIND:MEMORY-TOKEN IR--TYPE-KIND:EQ ;

: WAIT ( IR-ID:ir-value-id IR-ID:ir-value-id -- )
   {: d:IR-ID:ir-value-id s:IR-ID:ir-value-id :}
   NCOPIES @ {: c:n :}
   c 1+ CP-DST-RESERVE  c 1+ CP-SRC-RESERVE  c 1+ CP-VIA-RESERVE
   c 1+ CP-WAIT-RESERVE  c 1+ CP-USES-RESERVE
   d c CP-DST !  s c CP-SRC !  false c CP-VIA !  true c CP-WAIT !  0 c CP-USES !
   1 NCOPIES +! ;

\ How many copies read c's destination.
: READERS ( n -- n )
   {: c:n :}
   0
   NCOPIES @ 0 ?do
      i CP-SRC @ c CP-DST @ NFROZEN:SAME-VALUE? if 1+ then
   loop ;

\ The temporary for c's type, made when a cycle of that type is first broken.
: TEMP-OF ( n -- n )
   CP-DST @ NFROZEN:VALUE-TYPE-AT {: t:IR-ID:ir-type-id :}
   NTEMPS @ 0 ?do
      i TEMP-ROW @ t NFROZEN:SAME-TYPE? if i unloop exit then
   loop
   NTEMPS @ {: k:n :}
   k 1+ TEMP-ROW-RESERVE
   t k TEMP-ROW !
   1 NTEMPS +!
   k ;

\ A written copy has read v: the waiting copy that overwrites v has one reader fewer.
: READ-DONE ( IR-ID:ir-value-id -- )
   {: v:IR-ID:ir-value-id :}
   NCOPIES @ 0 ?do
      i CP-WAIT @ if
         i CP-DST @ v NFROZEN:SAME-VALUE? if i CP-USES @ 1- i CP-USES ! then
      then
   loop ;

: WRITE-COPY ( n -- )
   {: c:n :}
   false c CP-WAIT !
   c CP-VIA @ if c CP-DST @  c TEMP-OF  WCTL-STEP:restore PUT exit then
   c CP-DST @ c CP-SRC @ WCTL-STEP:copy PUT
   c CP-SRC @ READ-DONE ;

\ Every waiting copy lies on a cycle: save c's destination, and its reader
\ reads the temporary instead.
: BREAK ( n -- )
   {: c:n :}
   c TEMP-OF  c CP-DST @  WCTL-STEP:save PUT
   NCOPIES @ 0 ?do
      i CP-WAIT @ if
         i CP-SRC @ c CP-DST @ NFROZEN:SAME-VALUE? if true i CP-VIA ! then
      then
   loop
   0 c CP-USES ! ;

: UNREAD ( -- n bool )
   NCOPIES @ 0 ?do
      i CP-WAIT @ if i CP-USES @ 0= if i true unloop exit then then
   loop
   0 false ;

: WAITING ( -- n bool )
   NCOPIES @ 0 ?do i CP-WAIT @ if i true unloop exit then loop
   0 false ;

: SCHEDULE ( -- bool )
   UNREAD if WRITE-COPY true exit then drop
   WAITING if BREAK true exit then drop
   false ;

\ The copies br t hands its destination, a block argument from each operand.
: COPIES ( IR-ID:ir-op-id -- )
   {: t:IR-ID:ir-op-id :}
   t 0 NFROZEN:SUCC-AT {: to:IR-ID:ir-block-id :}
   0 NCOPIES !
   t NFROZEN:OPERANDS-OF 0 ?do
      to i NFROZEN:ARG-AT {: d:IR-ID:ir-value-id :}
      t i NFROZEN:OPERAND-AT {: s:IR-ID:ir-value-id :}
      d TOKEN? 0=  d s NFROZEN:SAME-VALUE? 0=  and if d s WAIT then
   loop
   NCOPIES @ 0 ?do i READERS i CP-USES ! loop
   begin SCHEDULE 0= until ;

\ ---- the control tree, written as steps --------------------------------------
: OPEN-LABEL ( n -- )
   {: y:n :}
   LABELS @ y POS !
   1 LABELS +! ;

: CLOSE-LABEL ( n -- )
   {: y:n :}
   -1 y POS !
   -1 LABELS +!
   WCTL-STEP:close PUT ;

\ How many labels out the open label of block y is from the next step.
: LABEL-DEPTH ( n -- n )
   POS @ LABELS @ 1- swap - ;

: MERGE? ( n -- bool )
   FWD @ 1 > ;

defer TREE ( n -- )

\ A forward edge to a block that is no merge is that block's only way in.
: BRANCH ( n n -- )
   {: x:n y:n :}
   y RPO @ x RPO @ <=  y MERGE?  or if
      y BLK  y LABEL-DEPTH  WCTL-STEP:branch PUT exit
   then
   y TREE ;

\ The then arm is brz's nonzero successor, its second.
: TWO-WAY ( n IR-ID:ir-op-id -- )
   {: x:n t:IR-ID:ir-op-id :}
   t 0 NFROZEN:OPERAND-AT WCTL-STEP:open-if PUT
   1 LABELS +!
   x  t 1 NFROZEN:SUCC-AT ORD  BRANCH
   WCTL-STEP:else-arm PUT
   x  t 0 NFROZEN:SUCC-AT ORD  BRANCH
   -1 LABELS +!
   WCTL-STEP:close PUT ;

: LEAF ( n -- )
   {: x:n :}
   x BLK WCTL-STEP:body PUT
   x TERM {: t:IR-ID:ir-op-id :}
   t NFROZEN:SUCCS-OF {: k:n :}
   k 0= if t WCTL-STEP:final PUT exit then
   k 1 = if t COPIES  x  t 0 NFROZEN:SUCC-AT ORD  BRANCH exit then
   x t TWO-WAY ;

\ x's merge child numbered latest before hi.
: CHILD ( n n -- n bool )
   {: x:n hi:n :}
   hi x RPO @ 1+ - 0 ?do
      hi 1- i - ORDER @ {: y:n :}
      y DOM @ x =  y MERGE?  and if y true unloop exit then
   loop
   0 false ;

\ x's code inside a block for each merge child numbered before hi.
: NODE-WITHIN ( n n -- )
   {: x:n hi:n :}
   x hi CHILD {: y:n found:bool :}
   found 0= if x LEAF exit then
   y BLK WCTL-STEP:open-block PUT
   y OPEN-LABEL
   x  y RPO @  RECURSE
   y CLOSE-LABEL
   y TREE ;

: GROW ( n -- )
   {: y:n :}
   y HEAD @ 0= if y NREACH @ NODE-WITHIN exit then
   y BLK WCTL-STEP:open-loop PUT
   y OPEN-LABEL
   y NREACH @ NODE-WITHIN
   y CLOSE-LABEL ;

: BIND-TREE ( -- )
   [: GROW ;] is TREE ;
BIND-TREE

\ ---- what a reader may ask --------------------------------------------------------
: INDEX-CK ( n n -- n )
   {: k:n cnt:n :}
   k 0 < k cnt >= or if E-WCTL-RANGE throw then
   k ;

: BLOCK-CK ( IR-ID:ir-block-id -- n )
   {: b:IR-ID:ir-block-id :}
   b IR-ID:BLOCK-OWNER  0 CUR @ IR-ID:FUN-OWNER  IR-ID:MODULE-SAME?
   0= if E-WCTL-RANGE throw then
   b ORD READY @ INDEX-CK ;

public

\ Structure one defined function of a module WSTRUCT:FREEZE verified, which the
\ readers below then answer for. A function whose control is irreducible is
\ refused and leaves nothing to read.
: STRUCTURE ( IR-BUILD:module IR-ID:ir-fun-id -- )
   0 READY !  0 NSTEPS !  0 NTEMPS !  0 LABELS !
   ANALYSE
   0 TREE
   NB @ READY ! ;

: STEPS ( -- n )
   NSTEPS @ ;

: STEP@ ( n -- WCTL:step )
   NSTEPS @ INDEX-CK STEP-ROW @ ;

\ The temporaries the copies use, one per type, the n of save and restore.
: TEMPS ( -- n )
   NTEMPS @ ;

: TEMP-TYPE ( n -- IR-ID:ir-type-id )
   NTEMPS @ INDEX-CK TEMP-ROW @ ;

\ The block's immediate dominator. The entry and a block the entry does not
\ reach answer themselves, since neither has another.
: IDOM ( IR-ID:ir-block-id -- IR-ID:ir-block-id )
   {: b:IR-ID:ir-block-id :}
   b BLOCK-CK DOM @ {: d:n :}
   d 0 < if b exit then
   d BLK ;

: HEADER? ( IR-ID:ir-block-id -- bool )
   BLOCK-CK HEAD @ ;

\ What the last refusal named: the function, and the span of one of the blocks
\ its cycle is entered at.
: REFUSED-FUN ( -- IR-ID:ir-symbol-id )
   0 REF-FUN @ ;

: REFUSED-SPAN ( -- IR-SOURCE:span )
   0 REF-SPAN @ ;

;package

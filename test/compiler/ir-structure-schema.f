\ ir-structure-schema.f - Native operation, value, function and block vectors.
\ Cases exercise construction, ownership, bounds and frozen-state checks.

require lib/errors.f
require lib/string.f
require src/compiler/ir/op.f
require src/compiler/ir/fun.f

package COMPILER-STRUCT-PROOF
public

\ ---- the two stores the model covers -----------------------------------------

0 constant KIND-SSA                  \ IR-OP's operation and value store
1 constant KIND-BLK                  \ IR-FUN's block table over IR-OP's operations
2 constant KIND-COUNT

private

\ ---- storage -----------------------------------------------------------------

$40 constant OPD-CAP
$40 constant STEP-CAP
$10 constant SCN-CAP

create OPD OPD-CAP cells allot

create SS-OPB STEP-CAP cells allot
create SS-OPN STEP-CAP cells allot
create SS-RSN STEP-CAP cells allot
create SS-CLASS STEP-CAP cells allot
create SS-ORD STEP-CAP cells allot

create BS-STRAY STEP-CAP cells allot
create BS-OPN STEP-CAP cells allot
create BS-TOFF STEP-CAP cells allot
create BS-CLASS STEP-CAP cells allot
create BS-ORD STEP-CAP cells allot

create SCN-KIND SCN-CAP cells allot
create SCN-BASE SCN-CAP cells allot
create SCN-LEN SCN-CAP cells allot
create SCN-A SCN-CAP cells allot
create SCN-B SCN-CAP cells allot
create SCN-C SCN-CAP cells allot
create SCN-FA SCN-CAP cells allot
create SCN-FB SCN-CAP cells allot

variable OPD-N
variable SS-N
variable BS-N
variable SCN-N
variable OPEN-BASE

: OPD-RANGE ( n -- ) {: i:n :}
   i 0 < i OPD-N @ >= or if E-CIS-ROW throw then ;

: SS-RANGE ( n -- ) {: i:n :}
   i 0 < i SS-N @ >= or if E-CIS-ROW throw then ;

: BS-RANGE ( n -- ) {: i:n :}
   i 0 < i BS-N @ >= or if E-CIS-ROW throw then ;

: SCN-RANGE ( n -- ) {: i:n :}
   i 0 < i SCN-N @ >= or if E-CIS-ROW throw then ;

\ ---- table builders ----------------------------------------------------------

\ One operand ordinal of the step being written.
: OPD+ ( n -- ) {: ord:n :}
   OPD-N @ OPD-CAP >= if E-CIS-ROW throw then
   ord OPD OPD-N @ cells + !
   OPD-N @ 1+ OPD-N ! ;

\ One value-store step: the operands already pushed since the last step, how
\ many results it mints, the ordinal it must answer, and the class - zero for an
\ accepted step, the exact throw code for a refused one.
: SSTEP+ ( n n n n -- ) {: opb:n rsn:n ord:n class:n :}
   SS-N @ STEP-CAP >= if E-CIS-ROW throw then
   opb SS-OPB SS-N @ cells + !
   OPD-N @ opb - SS-OPN SS-N @ cells + !
   rsn SS-RSN SS-N @ cells + !
   ord SS-ORD SS-N @ cells + !
   class SS-CLASS SS-N @ cells + !
   SS-N @ 1+ SS-N ! ;

\ One block step.
: BSTEP+ ( n n n n n -- ) {: stray:n opn:n toff:n ord:n class:n :}
   BS-N @ STEP-CAP >= if E-CIS-ROW throw then
   stray BS-STRAY BS-N @ cells + !
   opn BS-OPN BS-N @ cells + !
   toff BS-TOFF BS-N @ cells + !
   ord BS-ORD BS-N @ cells + !
   class BS-CLASS BS-N @ cells + !
   BS-N @ 1+ BS-N ! ;

: SEQ ( n -- ) {: kind:n :}
   kind KIND-SSA = if SS-N @ else BS-N @ then OPEN-BASE ! ;

: ;SEQ ( n n n n n n -- )
   {: kind:n a:n b:n c:n fa:n fb:n :}
   SCN-N @ SCN-CAP >= if E-CIS-ROW throw then
   kind SCN-KIND SCN-N @ cells + !
   OPEN-BASE @ SCN-BASE SCN-N @ cells + !
   kind KIND-SSA = if SS-N @ else BS-N @ then OPEN-BASE @ -
      SCN-LEN SCN-N @ cells + !
   a SCN-A SCN-N @ cells + !
   b SCN-B SCN-N @ cells + !
   c SCN-C SCN-N @ cells + !
   fa SCN-FA SCN-N @ cells + !
   fb SCN-FB SCN-N @ cells + !
   SCN-N @ 1+ SCN-N ! ;

\ ---- the value-store build sequences -----------------------------------------
\ Read a block as: open, present these operations in this order, close with the
\ store kind, the three committed ceilings, and the operation and
\ value counts the store must hold when the sequence ends.
\
\ An accepted operation answers the operation ordinal it was appended at, which
\ is the operation table's live count before the append. A refused one answers
\ its throw code and must leave both counts exactly as they were.

: CHAIN-SEQ ( -- )
   KIND-SSA SEQ
      OPD-N @ 1 0 0 SSTEP+
      0 OPD+ OPD-N @ 1- 1 1 0 SSTEP+
      0 OPD+ 1 OPD+ OPD-N @ 2 - 1 2 0 SSTEP+
   KIND-SSA 8 8 64 3 3 ;SEQ ;

: FORWARD-SEQ ( -- )
   KIND-SSA SEQ
      OPD-N @ 1 0 0 SSTEP+
      1 OPD+ OPD-N @ 1- 1 -1 E-IR-OP-SSA SSTEP+
      0 OPD+ OPD-N @ 1- 1 1 0 SSTEP+
   KIND-SSA 8 8 64 2 2 ;SEQ ;

: SELF-SEQ ( -- )
   KIND-SSA SEQ
      0 OPD+ OPD-N @ 1- 1 -1 E-IR-OP-SSA SSTEP+
   KIND-SSA 8 8 64 0 0 ;SEQ ;

: CYCLE-SEQ ( -- )
   KIND-SSA SEQ
      1 OPD+ OPD-N @ 1- 1 -1 E-IR-OP-SSA SSTEP+
      0 OPD+ OPD-N @ 1- 1 -1 E-IR-OP-SSA SSTEP+
   KIND-SSA 8 8 64 0 0 ;SEQ ;

: OP-CEIL-SEQ ( -- )
   KIND-SSA SEQ
      OPD-N @ 1 0 0 SSTEP+
      OPD-N @ 1 -1 E-IR-OP-CAP SSTEP+
   KIND-SSA 1 8 64 1 1 ;SEQ ;

: VAL-CEIL-SEQ ( -- )
   KIND-SSA SEQ
      OPD-N @ 1 0 0 SSTEP+
      OPD-N @ 1 -1 E-IR-OP-CAP SSTEP+
      OPD-N @ 0 1 0 SSTEP+
   KIND-SSA 8 1 64 2 1 ;SEQ ;

: POOL-CEIL-SEQ ( -- )
   KIND-SSA SEQ
      OPD-N @ 1 0 0 SSTEP+
      0 OPD+ OPD-N @ 1- 1 -1 E-IR-OP-CAP SSTEP+
      OPD-N @ 1 1 0 SSTEP+
   KIND-SSA 8 8 2 2 2 ;SEQ ;

\ ---- the block build sequences -----------------------------------------------
\ A step is (operations appended while no block is open, operations appended
\ inside the block, which of those is the terminator). A terminator offset at or
\ past the block's operation count means the block holds no terminator at all.
\ The sequence closes with the block count and the operation count the two
\ stores must hold when it ends.

: TILE-SEQ ( -- )
   KIND-BLK SEQ
      0 1 0 0 0 BSTEP+
      0 2 1 1 0 BSTEP+
   KIND-BLK 0 0 0 2 3 ;SEQ ;

: STRAY-SEQ ( -- )
   KIND-BLK SEQ
      0 1 0 0 0 BSTEP+
      1 1 0 -1 E-IR-FUN-WINDOW BSTEP+
   KIND-BLK 0 0 0 1 3 ;SEQ ;

: EMPTY-SEQ ( -- )
   KIND-BLK SEQ
      0 0 0 -1 E-IR-FUN-TERM BSTEP+
   KIND-BLK 0 0 0 0 0 ;SEQ ;

: NOTERM-SEQ ( -- )
   KIND-BLK SEQ
      0 1 1 -1 E-IR-FUN-TERM BSTEP+
   KIND-BLK 0 0 0 0 1 ;SEQ ;

: MIDTERM-SEQ ( -- )
   KIND-BLK SEQ
      0 2 0 -1 E-IR-FUN-TERM BSTEP+
   KIND-BLK 0 0 0 0 2 ;SEQ ;

: WEDGE-SEQ ( -- )
   KIND-BLK SEQ
      0 1 0 0 0 BSTEP+
      1 1 0 -1 E-IR-FUN-WINDOW BSTEP+
      0 1 0 -1 E-IR-FUN-WINDOW BSTEP+
   KIND-BLK 0 0 0 1 4 ;SEQ ;

: BUILD-SEQUENCES ( -- )
   0 OPD-N !
   0 SS-N !
   0 BS-N !
   0 SCN-N !
   CHAIN-SEQ
   FORWARD-SEQ
   SELF-SEQ
   CYCLE-SEQ
   OP-CEIL-SEQ
   VAL-CEIL-SEQ
   POOL-CEIL-SEQ
   TILE-SEQ
   STRAY-SEQ
   EMPTY-SEQ
   NOTERM-SEQ
   MIDTERM-SEQ
   WEDGE-SEQ ;

BUILD-SEQUENCES

public

: SCENARIOS ( -- n )       SCN-N @ ;
: SSA-STEPS ( -- n )       SS-N @ ;
: BLK-STEPS ( -- n )       BS-N @ ;

: SCN-KIND@ ( n -- n )     dup SCN-RANGE cells SCN-KIND + @ ;
: SCN-BASE@ ( n -- n )     dup SCN-RANGE cells SCN-BASE + @ ;
: SCN-LEN@ ( n -- n )      dup SCN-RANGE cells SCN-LEN + @ ;
: SCN-OCAP@ ( n -- n )     dup SCN-RANGE cells SCN-A + @ ;
: SCN-VCAP@ ( n -- n )     dup SCN-RANGE cells SCN-B + @ ;
: SCN-PCAP@ ( n -- n )     dup SCN-RANGE cells SCN-C + @ ;
: SCN-FINAL-A@ ( n -- n )  dup SCN-RANGE cells SCN-FA + @ ;
: SCN-FINAL-B@ ( n -- n )  dup SCN-RANGE cells SCN-FB + @ ;

: SS-OPB@ ( n -- n )       dup SS-RANGE cells SS-OPB + @ ;
: SS-OPN@ ( n -- n )       dup SS-RANGE cells SS-OPN + @ ;
: SS-RSN@ ( n -- n )       dup SS-RANGE cells SS-RSN + @ ;
: SS-ORD@ ( n -- n )       dup SS-RANGE cells SS-ORD + @ ;
: SS-CLASS@ ( n -- n )     dup SS-RANGE cells SS-CLASS + @ ;

: OPD@ ( n -- n )          dup OPD-RANGE cells OPD + @ ;

: BS-STRAY@ ( n -- n )     dup BS-RANGE cells BS-STRAY + @ ;
: BS-OPN@ ( n -- n )       dup BS-RANGE cells BS-OPN + @ ;
: BS-TOFF@ ( n -- n )      dup BS-RANGE cells BS-TOFF + @ ;
: BS-ORD@ ( n -- n )       dup BS-RANGE cells BS-ORD + @ ;
: BS-CLASS@ ( n -- n )     dup BS-RANGE cells BS-CLASS + @ ;

;package

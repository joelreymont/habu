\ ir-structure-schema.f - the shared frozen description of the operation and
\ function structure contract.
\
\ The module lives in `package COMPILER-STRUCT-PROOF`. Its subject is the two
\ newest stores of the compiler substrate - `IR-OP` (src/compiler/ir/op.f) and
\ `IR-FUN` (src/compiler/ir/fun.f) - and the machine-checked model of them in
\ `formal/Common/Structure.v`.
\
\ It holds data and nothing else. Four tables:
\
\   1. The build sequences. A sequence names a store kind, the committed
\      ceilings it runs under, and an ordered list of build steps with the
\      answer each step must receive: an ordinal, or the exact throw code that
\      must reject it, together with the two counts the store must end on.
\      These rows are the one copy. `test/compiler/ir-structure-cases.f` runs
\      each row through the real `IR-OP` and `IR-FUN` builder words, and
\      `test/compiler/ir-structure-obligations.f` turns the very same row into a
\      Rocq obligation about `Habu.Common.Structure`. Neither side carries a
\      copy of the vectors, so weakening a row asks both sides a weaker question
\      and deleting one stops both sides asking.
\
\      A single-value-store step is one operation: the value ordinals it names
\      as operands and how many results it mints. A block step is one block: how
\      many operations were appended while no block was open, how many were
\      appended inside the block, and which of those is the terminator.
\
\   2. The frozen guard bodies. Each is small, each is entirely guard, and each
\      is the reason one of the model's hypotheses is true of the shipped code:
\      `OPERANDS-CK` is the strictly-below operand rule, `WIN-STARTS` and
\      `ROW-ADD` are the append side that lays the four windows down in order,
\      and `TILE-CK` / `OTILE-CK` / `BTILE-CK` / `ATILE-CK` with `STEP-CK` are
\      the read side that revalidates the tiling against one neighbouring row.
\
\   3. The call-closed guard rows. A guard may be several calls deep - `END-OP`
\      reaches the strictly-below rule through `OPERANDS-CK`, and `OP@` reaches
\      the tiling rule through `OTILE-CK` and then `STEP-CK` - so a one-level
\      token scan of either body finds no guard token at all and passes
\      vacuously. Every row here therefore records that the guard token occurs
\      ZERO times in the body directly, and the reader has to reach it by
\      closing the guard relation under calls. A builder row must also write
\      rows and must run every guard before its first write; a reader row must
\      write nothing at all.
\
\   4. The terminator field, which the model finds to be derived rather than
\      recorded: `BROW-ADD` writes `opst opn + 1-` and `TERMINATOR@` recomputes
\      the same number and rejects anything else. Both bodies are frozen here so
\      the finding cannot quietly stop being true.
\
\ Where the two sides are not literally the same shape, and why that is sound:
\
\   - Habu rejects with a named throw and the model rejects with `None`. The row
\     records the throw code, so the Habu diagnostic and the Rocq decision stay
\     bound to each other row by row rather than only agreeing that something
\     failed.
\   - A Habu operation names an opcode whose schema fixes its arity. The
\     sequences use one variadic opcode for the value-store rows, so the schema
\     can never be what rejects a step and the answer is decided by the SSA rule
\     and the ceilings alone - which is what the model describes.
\   - The model has no source spans, no module keys and no attribute rows, so
\     every step declares the one span the store requires and carries no
\     attribute. Those are Habu.Common.Interning's and Habu.Common.IdLaws'
\     subjects.
\
\ Consumers: `test/compiler/ir-structure-cases.f`,
\ `test/compiler/ir-structure-obligations.f`.

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

\ ---- what each build sequence is there to show -------------------------------
\ Every role below must be covered by at least one sequence. The coverage check
\ in the cases file is what stops a sequence being quietly deleted: the table has
\ no digest, so the roles are the freeze.

0 constant ROLE-CHAIN                \ a legal chain, each operand naming an earlier value
1 constant ROLE-FORWARD              \ an operand naming the live count itself
2 constant ROLE-SELF                 \ an operand naming the ordinal this operation would mint
3 constant ROLE-CYCLE                \ two operations each naming the other's result
4 constant ROLE-OP-CEIL              \ the operation table's committed ceiling
5 constant ROLE-VAL-CEIL             \ the value table's committed ceiling, then a legal append
6 constant ROLE-POOL-CEIL            \ the cell pool's committed ceiling, then a legal append
7 constant ROLE-TILE                 \ two blocks whose operation windows tile in a row
8 constant ROLE-STRAY                \ an operation appended while no block is open
9 constant ROLE-EMPTY                \ a block holding no operation at all
10 constant ROLE-NOTERM              \ a block whose last operation is not a terminator
11 constant ROLE-MIDTERM             \ a block with a terminator before its end
12 constant ROLE-WEDGE               \ a refused block leaves its operations behind for good
13 constant ROLE-COUNT

\ The role's name, for the label a failing row reports and for the name the
\ generated Rocq obligation carries.
: ROLE-NAME$ ( n -- ptr u8 n )
   case
      0 of s" chain" endof
      1 of s" forward_ref" endof
      2 of s" self_ref" endof
      3 of s" cycle" endof
      4 of s" op_ceiling" endof
      5 of s" value_ceiling" endof
      6 of s" pool_ceiling" endof
      7 of s" tile" endof
      8 of s" stray_op" endof
      9 of s" empty_block" endof
      10 of s" no_terminator" endof
      11 of s" mid_terminator" endof
      12 of s" wedged" endof
      E-CIS-ROW throw
   endcase ;

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

create SCN-ROLE SCN-CAP cells allot
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

: ;SEQ ( n n n n n n n -- )
   {: role:n kind:n a:n b:n c:n fa:n fb:n :}
   SCN-N @ SCN-CAP >= if E-CIS-ROW throw then
   role SCN-ROLE SCN-N @ cells + !
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
\ role, the store kind, the three committed ceilings, and the operation and
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
   ROLE-CHAIN KIND-SSA 8 8 64 3 3 ;SEQ ;

: FORWARD-SEQ ( -- )
   KIND-SSA SEQ
      OPD-N @ 1 0 0 SSTEP+
      1 OPD+ OPD-N @ 1- 1 -1 E-IR-OP-SSA SSTEP+
      0 OPD+ OPD-N @ 1- 1 1 0 SSTEP+
   ROLE-FORWARD KIND-SSA 8 8 64 2 2 ;SEQ ;

: SELF-SEQ ( -- )
   KIND-SSA SEQ
      0 OPD+ OPD-N @ 1- 1 -1 E-IR-OP-SSA SSTEP+
   ROLE-SELF KIND-SSA 8 8 64 0 0 ;SEQ ;

: CYCLE-SEQ ( -- )
   KIND-SSA SEQ
      1 OPD+ OPD-N @ 1- 1 -1 E-IR-OP-SSA SSTEP+
      0 OPD+ OPD-N @ 1- 1 -1 E-IR-OP-SSA SSTEP+
   ROLE-CYCLE KIND-SSA 8 8 64 0 0 ;SEQ ;

: OP-CEIL-SEQ ( -- )
   KIND-SSA SEQ
      OPD-N @ 1 0 0 SSTEP+
      OPD-N @ 1 -1 E-IR-OP-CAP SSTEP+
   ROLE-OP-CEIL KIND-SSA 1 8 64 1 1 ;SEQ ;

: VAL-CEIL-SEQ ( -- )
   KIND-SSA SEQ
      OPD-N @ 1 0 0 SSTEP+
      OPD-N @ 1 -1 E-IR-OP-CAP SSTEP+
      OPD-N @ 0 1 0 SSTEP+
   ROLE-VAL-CEIL KIND-SSA 8 1 64 2 1 ;SEQ ;

: POOL-CEIL-SEQ ( -- )
   KIND-SSA SEQ
      OPD-N @ 1 0 0 SSTEP+
      0 OPD+ OPD-N @ 1- 1 -1 E-IR-OP-CAP SSTEP+
      OPD-N @ 1 1 0 SSTEP+
   ROLE-POOL-CEIL KIND-SSA 8 8 2 2 2 ;SEQ ;

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
   ROLE-TILE KIND-BLK 0 0 0 2 3 ;SEQ ;

: STRAY-SEQ ( -- )
   KIND-BLK SEQ
      0 1 0 0 0 BSTEP+
      1 1 0 -1 E-IR-FUN-WINDOW BSTEP+
   ROLE-STRAY KIND-BLK 0 0 0 1 3 ;SEQ ;

: EMPTY-SEQ ( -- )
   KIND-BLK SEQ
      0 0 0 -1 E-IR-FUN-TERM BSTEP+
   ROLE-EMPTY KIND-BLK 0 0 0 0 0 ;SEQ ;

: NOTERM-SEQ ( -- )
   KIND-BLK SEQ
      0 1 1 -1 E-IR-FUN-TERM BSTEP+
   ROLE-NOTERM KIND-BLK 0 0 0 0 1 ;SEQ ;

: MIDTERM-SEQ ( -- )
   KIND-BLK SEQ
      0 2 0 -1 E-IR-FUN-TERM BSTEP+
   ROLE-MIDTERM KIND-BLK 0 0 0 0 2 ;SEQ ;

: WEDGE-SEQ ( -- )
   KIND-BLK SEQ
      0 1 0 0 0 BSTEP+
      1 1 0 -1 E-IR-FUN-WINDOW BSTEP+
      0 1 0 -1 E-IR-FUN-WINDOW BSTEP+
   ROLE-WEDGE KIND-BLK 0 0 0 1 4 ;SEQ ;

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

: SCN-ROLE@ ( n -- n )     dup SCN-RANGE cells SCN-ROLE + @ ;
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

\ ---- the frozen guard bodies -------------------------------------------------
\ Each body is read back out of the production source as a normalized token run
\ and compared whole. These are the bodies that make the model's hypotheses true
\ of the shipped code, so a silent edit to any of them is a silent change to
\ what the proofs are about.

16 constant GUARD-COUNT

: GUARD-FILE$ ( n -- ptr u8 n )
   case
      0 of s" src/compiler/ir/op.f" endof
      1 of s" src/compiler/ir/op.f" endof
      2 of s" src/compiler/ir/op.f" endof
      3 of s" src/compiler/ir/op.f" endof
      4 of s" src/compiler/ir/op.f" endof
      5 of s" src/compiler/ir/op.f" endof
      6 of s" src/compiler/ir/op.f" endof
      7 of s" src/compiler/ir/fun.f" endof
      8 of s" src/compiler/ir/fun.f" endof
      9 of s" src/compiler/ir/fun.f" endof
      10 of s" src/compiler/ir/fun.f" endof
      11 of s" src/compiler/ir/fun.f" endof
      12 of s" src/compiler/ir/fun.f" endof
      13 of s" src/compiler/ir/fun.f" endof
      14 of s" src/compiler/ir/fun.f" endof
      15 of s" src/compiler/ir/fun.f" endof
      E-CIS-ROW throw
   endcase ;

: GUARD-WORD$ ( n -- ptr u8 n )
   case
      0 of s" OPERANDS-CK" endof
      1 of s" STEP-CK" endof
      2 of s" TILE-CK" endof
      3 of s" FTILE-CK" endof
      4 of s" WIN-STARTS" endof
      5 of s" ROW-ADD" endof
      6 of s" ROOM-CK" endof
      7 of s" STEP-CK" endof
      8 of s" OTILE-CK" endof
      9 of s" BTILE-CK" endof
      10 of s" ATILE-CK" endof
      11 of s" TERM-CK" endof
      12 of s" TERM-AT-CK" endof
      13 of s" ARGS-CK" endof
      14 of s" PARENTS-CK" endof
      15 of s" BROW-ADD" endof
      E-CIS-ROW throw
   endcase ;

: GUARD-BODY$ ( n -- ptr u8 n )
   case
      0 of s" {: hs:n vcnt:n :} L-OP SN@ 0 ?do hs L-OP SEG i + SO@ OWNED-CK L-OP SEG i + SV@ dup 0 < swap vcnt >= or if E-IR-OP-SSA throw then loop" endof
      1 of s" <> if E-IR-OP-WINDOW throw then" endof
      2 of s" {: p:IR-ARENA:arena r:IR-ARENA:arena l:n :} l 0= if 0 else r l 1- ROW-END then {: at:n :} at r l OFF-OPST RC@ STEP-CK at r l OFF-OPN RC@ LEN-OK + {: a1:n :} a1 r l OFF-RSST RC@ STEP-CK a1 r l OFF-RSN RC@ LEN-OK + {: a2:n :} a2 r l OFF-SCST RC@ STEP-CK a2 r l OFF-SCN RC@ LEN-OK + {: a3:n :} a3 r l OFF-ATST RC@ STEP-CK a3 r l OFF-ATN RC@ LEN-OK + p PCELLS > if E-IR-OP-STATE throw then" endof
      3 of s" {: p:IR-ARENA:view r:IR-ARENA:view l:n :} l 0= if 0 else r l 1- FROW-END then {: at:n :} at r l OFF-OPST FRC@ STEP-CK at r l OFF-OPN FRC@ LEN-OK + {: a1:n :} a1 r l OFF-RSST FRC@ STEP-CK a1 r l OFF-RSN FRC@ LEN-OK + {: a2:n :} a2 r l OFF-SCST FRC@ STEP-CK a2 r l OFF-SCN FRC@ LEN-OK + {: a3:n :} a3 r l OFF-ATST FRC@ STEP-CK a3 r l OFF-ATN FRC@ LEN-OK + p FPCELLS > if E-IR-OP-STATE throw then" endof
      4 of s" {: st:n :} st st L-OP SN@ + st L-OP SN@ + L-RS SN@ + st L-OP SN@ + L-RS SN@ + L-SC SN@ +" endof
      5 of s" {: c:IR-CTX:ctx r:IR-ARENA:arena st:n :} st WIN-STARTS {: sop:n srs:n ssc:n sat:n :} c r STG-OPC @ CELL+ c r sop CELL+ c r L-OP SN@ CELL+ c r srs CELL+ c r L-RS SN@ CELL+ c r ssc CELL+ c r L-SC SN@ CELL+ c r sat CELL+ c r L-AT SN@ CELL+ c r STG-SRC @ CELL+ c r STG-SBEG @ CELL+ c r STG-SLEN @ CELL+" endof
      6 of s" {: p:IR-ARENA:arena v:IR-ARENA:arena r:IR-ARENA:arena :} r CNT r HC-CAP LCELL@ >= if E-IR-OP-CAP throw then v VCNT L-RS SN@ + v HC-CAP LCELL@ > if E-IR-OP-CAP throw then p PCELLS STAGED-CELLS + p HC-CAP LCELL@ > if E-IR-OP-CAP throw then" endof
      7 of s" <> if E-IR-FUN-WINDOW throw then" endof
      8 of s" {: b:IR-ARENA:arena l:n :} l 0= if 0 else b l 1- BOP-END then b l OFF-OPST BC@ STEP-CK" endof
      9 of s" {: f:IR-ARENA:arena l:n :} l 0= if 0 else f l 1- FNBK-END then f l OFF-BST FNC@ STEP-CK" endof
      10 of s" {: p:IR-ARENA:arena f:IR-ARENA:arena l:n :} l 0= if 0 else f l 1- FNAT-END then {: at:n :} at f l OFF-ATST FNC@ STEP-CK f l FNAT-END p PCELLS > if E-IR-FUN-STATE throw then" endof
      11 of s" {: r:IR-ARENA:arena qr:IR-ARENA:arena key:IR-ID:ir-module-key opst:n opn:n :} opn 1 < if E-IR-FUN-TERM throw then opn 0 ?do r qr key opst i opn 1- TERM-AT-CK loop" endof
      12 of s" {: r:IR-ARENA:arena qr:IR-ARENA:arena key:IR-ID:ir-module-key opst:n i:n last:n :} qr r key key opst i + IR-ID:PACK-OP IR-OP:OPCODE@ IR-SCHEMA:TERMINATOR? {: t:bool :} t if i last <> if E-IR-FUN-TERM throw then exit then i last = if E-IR-FUN-TERM throw then" endof
      13 of s" {: v:IR-ARENA:arena key:IR-ID:ir-module-key l:n agst:n agn:n :} agn 0 ?do key agst i + IR-ID:PACK-VALUE {: id:IR-ID:ir-value-id :} v id IR-OP:VALUE-KIND@ IR--OP-DEF--KIND:BLK-ARG IR--OP-DEF--KIND:EQ 0= if E-IR-FUN-ARG throw then v key id IR-OP:VALUE-BLOCK@ IR-ID:BLOCK-LOCAL l <> if E-IR-FUN-ARG throw then v id IR-OP:VALUE-ARG@ i <> if E-IR-FUN-ARG throw then loop" endof
      14 of s" {: b:IR-ARENA:arena l:n bst:n bn:n :} bn 0 ?do b bst i + OFF-PAR BC@ l <> if E-IR-FUN-PARENT throw then loop" endof
      15 of s" {: c:IR-CTX:ctx b:IR-ARENA:arena par:n agst:n opst:n opn:n :} c b par CELL+ c b agst CELL+ c b BSTG-AGN @ CELL+ c b opst CELL+ c b opn CELL+ c b opst opn + 1- CELL+ c b BSTG-SRC @ CELL+ c b BSTG-SBEG @ CELL+ c b BSTG-SLEN @ CELL+" endof
      E-CIS-ROW throw
   endcase ;

\ ---- the call-closed guard rows ----------------------------------------------
\ One row per (word, guard) pair whose guard is reached only through a call.
\ Every row asserts that the guard token occurs zero times in the body itself,
\ so a reader that did not close the guard relation under calls would pass the
\ row for a body that has no guard at all.
\
\ A builder row writes rows into an arena and must run every guard before its
\ first write. A reader row must contain no writer at all, because a reader
\ revalidates and never mutates.

13 constant ORDER-COUNT

: ORDER-FILE$ ( n -- ptr u8 n )
   case
      0 of s" src/compiler/ir/op.f" endof
      1 of s" src/compiler/ir/op.f" endof
      2 of s" src/compiler/ir/op.f" endof
      3 of s" src/compiler/ir/fun.f" endof
      4 of s" src/compiler/ir/fun.f" endof
      5 of s" src/compiler/ir/fun.f" endof
      6 of s" src/compiler/ir/fun.f" endof
      7 of s" src/compiler/ir/op.f" endof
      8 of s" src/compiler/ir/op.f" endof
      9 of s" src/compiler/ir/fun.f" endof
      10 of s" src/compiler/ir/fun.f" endof
      11 of s" src/compiler/ir/fun.f" endof
      12 of s" src/compiler/ir/fun.f" endof
      E-CIS-ROW throw
   endcase ;

: ORDER-WORD$ ( n -- ptr u8 n )
   case
      0 of s" END-OP" endof
      1 of s" END-OP" endof
      2 of s" END-OP" endof
      3 of s" END-BLOCK" endof
      4 of s" END-BLOCK" endof
      5 of s" END-FUN" endof
      6 of s" END-FUN" endof
      7 of s" WIN@" endof
      8 of s" FWIN@" endof
      9 of s" OP@" endof
      10 of s" FOP@" endof
      11 of s" BLOCK@" endof
      12 of s" TERMINATOR@" endof
      E-CIS-ROW throw
   endcase ;

: ORDER-GUARD$ ( n -- ptr u8 n )
   case
      0 of s" E-IR-OP-SSA" endof
      1 of s" E-IR-OP-CAP" endof
      2 of s" E-IR-OP-ARITY" endof
      3 of s" E-IR-FUN-TERM" endof
      4 of s" E-IR-FUN-ARG" endof
      5 of s" E-IR-FUN-PARENT" endof
      6 of s" E-IR-FUN-LINKAGE" endof
      7 of s" E-IR-OP-WINDOW" endof
      8 of s" E-IR-OP-WINDOW" endof
      9 of s" E-IR-FUN-WINDOW" endof
      10 of s" E-IR-FUN-WINDOW" endof
      11 of s" E-IR-FUN-WINDOW" endof
      12 of s" E-IR-FUN-WINDOW" endof
      E-CIS-ROW throw
   endcase ;

\ True for a builder row, false for a reader row.
: ORDER-WRITES? ( n -- bool )
   7 < ;

\ The token that writes a cell into an arena, and the token every guard row is
\ reached through. A definition that carries either, directly or through a word
\ it calls, is a writer or a guard.
: PUSH-TOKEN$ ( -- ptr u8 n )
   s" IR-ARENA:PUSH" ;

: OP-FILE$ ( -- ptr u8 n )
   s" src/compiler/ir/op.f" ;

: FUN-FILE$ ( -- ptr u8 n )
   s" src/compiler/ir/fun.f" ;

: MODEL-FILE$ ( -- ptr u8 n )
   s" formal/Common/Structure.v" ;

;package

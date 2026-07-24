\ maki/model-ir-test.f - checked tests for the model IR node table, operand ref
\ tagging, output descriptor facts, shape/dtype/layout keys, render, fail-closed.
\ Test-local names are MT- prefixed; suites share the MAKI wordlist.

require lib/test.f
require lib/string.f
require test/checker-assert.f
require maki/model-ir.f

\ ---- white-box CAD-NUM role reader (mirrors lib/vector-test.f's VECT-IC>RAW) --
\ Reopen the CAD-NUM package to project a validated item-count's raw cell for the
\ scalar count assertions below. A plain checked word over the already-audited
\ CAD-NUM:ITEM-COUNT>N projection - not a new boundary (refine-lint does not
\ confine the *>N projections, only the MINT-*/RAW>* refinements).
package CAD-NUM
public
: MT-IC>RAW ( CAD-NUM:item-count -- n ) ITEM-COUNT>N ;
;package

package MAKI

\ ---- render containment helper ----------------------------------------------
variable MT-VA  variable MT-VU
: MT-SAVE ( ptr u8 n -- )  MT-VU ! MT-VA ! ;
: MT-IN ( ptr u8 n -- )  MT-VA @ MT-VU @ 2swap CONTAINS? TTRUE ;

\ ---- build a small IR:  LINEAR(x,w) -> GELU  --------------------------------
1 LAYOUT-BUFFER MT-SX MIR:input-slot
1 LAYOUT-BUFFER MT-SW MIR:input-slot
1 LAYOUT-BUFFER MT-SU MIR:input-slot
1 LAYOUT-BUFFER MT-N0 CAD-KIND:node-id
1 LAYOUT-BUFFER MT-N1 CAD-KIND:node-id

: MT-SX! ( MIR:input-slot -- )  0 MT-SX ! ;
: MT-SX@ ( -- MIR:input-slot )  0 MT-SX @ ;
: MT-SW! ( MIR:input-slot -- )  0 MT-SW ! ;
: MT-SW@ ( -- MIR:input-slot )  0 MT-SW @ ;
: MT-SU! ( MIR:input-slot -- )  0 MT-SU ! ;
: MT-SU@ ( -- MIR:input-slot )  0 MT-SU @ ;
: MT-N0! ( CAD-KIND:node-id -- )  0 MT-N0 ! ;
: MT-N0@ ( -- CAD-KIND:node-id )  0 MT-N0 @ ;
: MT-N1! ( CAD-KIND:node-id -- )  0 MT-N1 ! ;
: MT-N1@ ( -- CAD-KIND:node-id )  0 MT-N1 @ ;

: MT-SLOT= ( MIR:input-slot MIR:input-slot -- )
   MIR-SLOT= TTRUE ;

: MT-NODE= ( CAD-KIND:node-id CAD-KIND:node-id -- )
   MIR-NODE= TTRUE ;

: MT-CHECK-YES ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! -1 T= ;

: MT-CHECK-NO ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! 0 T= ;

MIR-RESET
2 3 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ MT-SX!   \ slot 0 = x (2x3)
3 4 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ MT-SW!   \ slot 1 = w (3x4)
0 0 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ MT-SU!   \ slot 2 = unbound (?x?)

MAKI-OPKIND:LINEAR MIR-OP-BEGIN
   MT-SX@ MIR-IN-REF MIR-IN+
   MT-SW@ MIR-IN-REF MIR-IN+
2 4 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ MT-N0!  \ node 0 : y0 = 2x4, materialized

MAKI-OPKIND:GELU MIR-OP-BEGIN
   MT-N0@ MIR-NODE-REF MIR-IN+
2 4 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ MT-N1!  \ node 1 : y1 = 2x4

T-RESET

\ ---- table shape ------------------------------------------------------------
MIR-N@         2 T=
MIR-IN-SLOTS@  3 T=

1 LAYOUT-BUFFER MT-MARK-BUF MIR:mark

: MT-MARK! ( MIR:mark -- )
   0 MT-MARK-BUF ! ;

: MT-MARK@ ( -- MIR:mark )
   0 MT-MARK-BUF @ ;

\ Make the live high-waters pairwise distinct through the production input path,
\ then prove the production marker and generated inverse preserve field order.
: MT-MARK-VALUES ( -- n n n )
   MIR-MARK MT-MARK!
   5 7 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop
   MIR-MARK MIR-MARK:UNMAKE ;
: MT-MARK-RESTORE ( -- )
   MT-MARK@ MIR-RELEASE ;
MT-MARK-VALUES 3 T= 4 T= 2 T=
MT-MARK-RESTORE

\ typed count accessors (CAD-NUM:item-count) reflect the built graph: 2 nodes,
\ 3 input slots, 3 operand refs (linear x,w + gelu n0), both nodes materialized.
NODE-COUNT@        CAD-NUM:MT-IC>RAW 2 T=
SLOT-COUNT@        CAD-NUM:MT-IC>RAW 3 T=
OPERAND-COUNT@     CAD-NUM:MT-IC>RAW 3 T=
MATERIALIZED-COUNT CAD-NUM:MT-IC>RAW 2 T=

\ ---- node facts -------------------------------------------------------------
MT-N0@ MIR-OP@ OPKIND>N OP-LINEAR T=
MT-N1@ MIR-OP@ OPKIND>N OP-GELU   T=
MT-N1@ dup MIR-ROWS@ swap MIR-COLS@ 2 4 SHAPE SHAPE-EQUAL? TTRUE
MT-N1@ MIR-DT@   DTYPE>N DT-F32   T=
MT-N1@ MIR-LAY@  LAYOUT>N LAY-ROW T=
MT-N0@ MIR-IN-COUNT@ 2 T=
MT-N1@ MIR-IN-COUNT@ 1 T=

\ ---- operand ref tagging: input slot vs committed node ----------------------
MT-N0@ 0 MIR-INPUT-IDX MIR-IN@ MIR-REF-INPUT? TTRUE
MT-N0@ 0 MIR-INPUT-IDX MIR-IN@ MIR-REF-SLOT MT-SX@ MT-SLOT=       \ x -> slot 0
MT-N0@ 1 MIR-INPUT-IDX MIR-IN@ MIR-REF-SLOT MT-SW@ MT-SLOT=       \ w -> slot 1
MT-N1@ 0 MIR-INPUT-IDX MIR-IN@ MIR-REF-INPUT? TFALSE
MT-N1@ 0 MIR-INPUT-IDX MIR-IN@ MIR-REF-NODE MT-N0@ MT-NODE=       \ gelu consumes node 0

\ ---- materialization flag + count ------------------------------------------
MT-N0@ MIR-MAT@ TTRUE
MIR-MAT-COUNT 2 T=
0 MT-N0@ MIR-MAT!                                \ fusion clears node 0
MT-N0@ MIR-MAT@ TFALSE
MIR-MAT-COUNT 1 T=
1 MT-N0@ MIR-MAT!                                \ restore for later checks

\ ---- shape / dtype / layout keys -------------------------------------------
MT-N1@ MIR-SHAPE-KEY  s" 2x4" T$=
MT-N1@ MIR-DTYPE-KEY  s" f32" T$=
MT-N1@ MIR-LAYOUT-KEY s" row" T$=
MT-SX@ MIR-SLOT-SHAPE-KEY s" 2x3" T$=
MT-SU@ MIR-SLOT-SHAPE-KEY s" ?x?" T$=           \ unbound extents render "?"

\ ---- serialization render ---------------------------------------------------
MIR-RENDER MT-SAVE
s" ir.nodes: 2"        MT-IN
s" ir.inputs: 3"       MT-IN
s" node.0.op: linear"  MT-IN
s" node.1.op: gelu"    MT-IN
s" node.1.shape: 2x4"  MT-IN
s" node.0.in: i0 i1"   MT-IN
s" node.1.in: n0"      MT-IN
s" input.2.shape: ?x?" MT-IN

\ ---- movement node: attrs carry the verdict; render shows it ----------------
1 LAYOUT-BUFFER MT-MV CAD-KIND:node-id
: MT-MV! ( CAD-KIND:node-id -- )  0 MT-MV ! ;
: MT-MV@ ( -- CAD-KIND:node-id )  0 MT-MV @ ;

MIR-RESET
4 8 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop            \ slot 0 = x (4x8)
MAKI-OPKIND:RESHAPE MIR-OP-BEGIN
   0 MIR-SLOT-ID MIR-IN-REF MIR-IN+
8 4 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW  MV-RESHAPE MVV-FREE 8 4 MV-PACK  0  MIR-OP+ MT-MV!

MT-MV@ MIR-MOVE?          TTRUE
MT-MV@ MIR-OP@ OPKIND>N OP-RESHAPE T=
MT-MV@ MIR-MOVE-VERDICT@  MVV-FREE T=
MT-MV@ MIR-SHAPE-KEY s" 8x4" T$=
MT-MV@ MIR-MAT@           TFALSE                       \ free -> not materialized

MIR-RENDER MT-SAVE
s" node.0.op: reshape"    MT-IN
s" node.0.verdict: free"  MT-IN

\ ---- provenance: reset -> none; a producer adopts; reset clears it again -----
\ (dot habu-cad-f-imported: MIR-RESET clearing this cell atomically with the table
\ is what makes a stale producer's arming unreadable across an ONNX import)
MIR-RESET
MIR-PROV@ MAKI-PROV:NONE MAKI-PROV:EQ TTRUE
MIR-PROV@ PROV-KEY s" none" T$=
MAKI-PROV:CAPTURED MIR-PROV!
MIR-PROV@ MAKI-PROV:CAPTURED MAKI-PROV:EQ TTRUE
MIR-PROV@ PROV-KEY s" captured" T$=
MAKI-PROV:IMPORTED MIR-PROV!
MIR-PROV@ MAKI-PROV:IMPORTED MAKI-PROV:EQ TTRUE
MIR-PROV@ PROV-KEY s" imported" T$=
MIR-RESET
MIR-PROV@ MAKI-PROV:NONE MAKI-PROV:EQ TTRUE

\ ---- fail-closed probes -----------------------------------------------------
: TRY-MIR-IDX     ( -- )  MIR-N@ MIR-NODE-ID MIR-OP@ drop ;
\ (a bad op-kind tag is a checker reject now - pinned by the opkind negatives
\ below - so the old TRY-MIR-OPKIND runtime throw probe is unrepresentable)
: TRY-MIR-REF     ( -- )  MIR-RESET MAKI-OPKIND:GELU MIR-OP-BEGIN 5 RAW>REF MIR-IN+ ;
: TRY-MIR-STATE   ( -- )
   MIR-RESET
   1 1 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ MIR-IN-REF MIR-IN+ ;
: TRY-MIR-INSLOT  ( -- )  MIR-RESET 0 RAW>SLOT MIR-SLOT-ROWS@ drop ;
\ ceiling-pins (dot habu-size-model-proportional): the tables derive their size from the
\ model and grow-to-largest, so the caps are the transactional generous ceilings - a
\ request one past the ceiling throws NAMED before any allot (no 32768-node model built).
: TRY-MIR-CAP     ( -- )  MIR-RESET  MIR-CAP 1+ MIR-BIND-NODES ;
: TRY-MIR-INCAP   ( -- )  MIR-RESET  MIR-INCAP 1+ MIR-BIND-OPERANDS ;
: TRY-MIR-SLOTCAP ( -- )  MIR-RESET  MIR-IN-CAP 1+ MIR-BIND-SLOTS ;

\ a verdict requested from a non-movement (gelu) node fails closed
: TRY-MIR-NOTMOVE ( -- )
   MIR-RESET
   0 0 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ {: s:MIR:input-slot :}
   MAKI-OPKIND:GELU MIR-OP-BEGIN  s MIR-IN-REF MIR-IN+
   0 0 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+  MIR-MOVE-VERDICT@ drop ;

\ a release mark that would GROW the table fails closed
: TRY-MIR-MARK-GROW ( -- )
   MIR-RESET
   1 0 0 MIR-MARK:MAKE MIR-RELEASE ;

' TRY-MIR-IDX     E-MIR-IDX     TTHROWS
' TRY-MIR-REF     E-MIR-REF     TTHROWS
' TRY-MIR-STATE   E-MIR-STATE   TTHROWS
' TRY-MIR-INSLOT  E-MIR-INSLOT  TTHROWS
' TRY-MIR-CAP     E-MIR-CAP     TTHROWS
' TRY-MIR-INCAP   E-MIR-INCAP   TTHROWS
' TRY-MIR-SLOTCAP E-MIR-INSLOT  TTHROWS
' TRY-MIR-NOTMOVE E-MV-NOTMOVE  TTHROWS
' TRY-MIR-MARK-GROW E-MIR-STATE TTHROWS

\ ---- nominal handle acceptance + rejection (Model-CAD V2 R3) -----------------
s" MT-TYPED-NODE ( CAD-KIND:node-id -- opkind ) MIR-OP@" MT-CHECK-YES
s" MT-TYPED-SLOT ( MIR:input-slot -- CAD-KIND:rows ) MIR-SLOT-ROWS@" MT-CHECK-YES
s" MT-TYPED-REF ( MIR:operand-ref -- bool ) MIR-REF-INPUT?" MT-CHECK-YES
s" MT-TYPED-INPUT ( CAD-KIND:node-id MIR:input-index -- MIR:operand-ref ) MIR-IN@" MT-CHECK-YES

s" MT-NODE-AS-SLOT ( CAD-KIND:node-id -- CAD-KIND:rows ) MIR-SLOT-ROWS@" MT-CHECK-NO
s" MT-NODE-AS-REF ( CAD-KIND:node-id -- ) MIR-IN+" MT-CHECK-NO
s" MT-NODE-AS-OBJ ( CAD-KIND:obj-id -- opkind ) MIR-OP@" MT-CHECK-NO
s" MT-N-AS-INPUT ( CAD-KIND:node-id n -- MIR:operand-ref ) MIR-IN@" MT-CHECK-NO
s" MT-N-AS-NODE ( n -- opkind ) MIR-OP@" MT-CHECK-NO
s" MT-RAW-DESC ( n n n layout -- MIR:input-slot ) MIR-INPUT+" MT-CHECK-NO

\ Refinement boundaries are private even though the MAKI test package is open.
s" MAKI:RAW>NODE" 0 search-wl 0= TTRUE
s" MAKI:RAW>SLOT" 0 search-wl 0= TTRUE
s" MAKI:RAW>REF" 0 search-wl 0= TTRUE
s" MAKI:RAW>INPUT-INDEX" 0 search-wl 0= TTRUE
s" MAKI:RAW>REF-POS" 0 search-wl 0= TTRUE

\ ---- typed rollback mark ----------------------------------------------------
s" MT-MARK-MAKE ( n n n -- MIR:mark ) MIR-MARK:MAKE" MT-CHECK-YES
s" MT-MARK-UNMAKE ( MIR:mark -- n n n ) MIR-MARK:UNMAKE" MT-CHECK-YES

: MT-ROLLBACK ( -- CAD-KIND:node-id )
   MIR-RESET
   2 3 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ {: s:MIR:input-slot :}
   MAKI-OPKIND:GELU MIR-OP-BEGIN
   s MIR-IN-REF MIR-IN+
   2 3 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ {: base:CAD-KIND:node-id :}
   MIR-MARK MT-MARK!
   4 5 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop
   MAKI-OPKIND:RELU MIR-OP-BEGIN
   base MIR-NODE-REF MIR-IN+
   2 3 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ drop
   MT-MARK@ MIR-RELEASE
   base ;

MT-ROLLBACK MIR-OP@ OPKIND>N OP-GELU T=
MIR-N@ 1 T=
MIR-IN-SLOTS@ 1 T=

\ ---- typed count accessors: rollback / zero / max ---------------------------
\ rollback: reuse the post-MIR-RELEASE state above (1 node, 1 slot, 1 operand
\ ref) - the typed counts fall back to the marked high-water like the raw ones.
NODE-COUNT@        CAD-NUM:MT-IC>RAW 1 T=
SLOT-COUNT@        CAD-NUM:MT-IC>RAW 1 T=
OPERAND-COUNT@     CAD-NUM:MT-IC>RAW 1 T=

\ zero: an empty IR reports every count as item-count 0.
MIR-RESET
NODE-COUNT@        CAD-NUM:MT-IC>RAW 0 T=
SLOT-COUNT@        CAD-NUM:MT-IC>RAW 0 T=
OPERAND-COUNT@     CAD-NUM:MT-IC>RAW 0 T=
MATERIALIZED-COUNT CAD-NUM:MT-IC>RAW 0 T=

\ the node table now DERIVES its size from the model (dot habu-size-model-proportional):
\ MIR-CAP is the generous transactional CEILING (TRY-MIR-CAP pins cap+1 -> E-MIR-CAP), well
\ above a 12-block GPT-2 stack (~723 fwd+bwd nodes). The live size grows-to-largest from
\ MIR-NODE-SEED; the value below is the fail-closed boundary the ceiling die guards.
MIR-CAP $8000 T=

\ growth: fill well PAST the seed to exercise copy-on-grow (dot habu-size-model-proportional).
\ NODE-COUNT@ tracks the live count EXACTLY; a node written before a grow survives it (the
\ preservation copy-on-grow gives); each node opens with zero operands and mat=0.
400 constant MT-FILL-N          \ > MIR-NODE-SEED (128): two doublings 128->256->512
: MT-FILL-MAX ( -- )
   MIR-RESET
   MT-FILL-N 0 ?do
      MAKI-OPKIND:CAST MIR-OP-BEGIN
      1 1 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 0 MIR-OP+ drop
   loop ;
MT-FILL-MAX
NODE-COUNT@        CAD-NUM:MT-IC>RAW MT-FILL-N T=                     \ live count is exact
100 MIR-NODE-ID MIR-OP@ MAKI-OPKIND:CAST MAKI-OPKIND:EQ TTRUE        \ node written pre-grow survives
MT-FILL-N 1- MIR-NODE-ID MIR-OP@ MAKI-OPKIND:CAST MAKI-OPKIND:EQ TTRUE  \ last (post-grow) node intact
OPERAND-COUNT@     CAD-NUM:MT-IC>RAW 0 T=
MATERIALIZED-COUNT CAD-NUM:MT-IC>RAW 0 T=
MIR-RESET

\ ---- swapped-family negatives (dot habu-cad-adt-swap + Model-CAD V2 R3) ------
\ THE headline hole: a dtype/layout swap at the MIR-INPUT+/MIR-OP+ API boundary
\ is a CHECKER reject (dtype and layout are no longer indistinguishable bytes),
\ and the typed rows/cols kinds reject raw or transposed extents. Positive
\ controls pin the well-typed calls; n-launder rows pin that a raw code cannot
\ enter and a family cannot leak as n.
s" MTX-IN-OK    ( CAD-KIND:rows CAD-KIND:cols dtype layout -- MIR:input-slot ) MIR-INPUT+"  CHECK-QUIET-CANDIDATE! -1 T=
s" MTX-IN-SWAP  ( CAD-KIND:rows CAD-KIND:cols layout dtype -- MIR:input-slot ) MIR-INPUT+"  CHECK-QUIET-CANDIDATE! 0 T=
s" MTX-IN-NDT   ( CAD-KIND:rows CAD-KIND:cols n layout -- MIR:input-slot ) MIR-INPUT+"      CHECK-QUIET-CANDIDATE! 0 T=
s" MTX-IN-NROWS ( n n dtype layout -- MIR:input-slot ) MIR-INPUT+"                          CHECK-QUIET-CANDIDATE! 0 T=
s" MTX-IN-RCSWP ( CAD-KIND:cols CAD-KIND:rows dtype layout -- MIR:input-slot ) MIR-INPUT+"  CHECK-QUIET-CANDIDATE! 0 T=
s" MTX-OP-OK    ( CAD-KIND:rows CAD-KIND:cols dtype layout n n -- CAD-KIND:node-id ) MIR-OP+" CHECK-QUIET-CANDIDATE! -1 T=
s" MTX-OP-SWAP  ( CAD-KIND:rows CAD-KIND:cols layout dtype n n -- CAD-KIND:node-id ) MIR-OP+" CHECK-QUIET-CANDIDATE! 0 T=
s" MTX-OP-NLAY  ( CAD-KIND:rows CAD-KIND:cols dtype n n n -- CAD-KIND:node-id ) MIR-OP+"      CHECK-QUIET-CANDIDATE! 0 T=
s" MTX-AL-OK    ( MIR:input-slot align -- ) MIR-SLOT-AL!"  CHECK-QUIET-CANDIDATE! -1 T=
s" MTX-AL-N     ( MIR:input-slot n -- ) MIR-SLOT-AL!"      CHECK-QUIET-CANDIDATE! 0 T=
s" MTX-AL-DT    ( MIR:input-slot dtype -- ) MIR-SLOT-AL!"  CHECK-QUIET-CANDIDATE! 0 T=
\ accessor outputs are families, not n: enum->n laundering rejects
s" MTX-DT-NOUT  ( CAD-KIND:node-id -- n ) MIR-DT@"         CHECK-QUIET-CANDIDATE! 0 T=
s" MTX-LAY-NOUT ( MIR:input-slot -- n ) MIR-SLOT-LAY@"     CHECK-QUIET-CANDIDATE! 0 T=
s" MTX-AL-NOUT  ( MIR:input-slot -- n ) MIR-SLOT-AL@"      CHECK-QUIET-CANDIDATE! 0 T=
s" MTX-ROWS-NOUT ( CAD-KIND:node-id -- n ) MIR-ROWS@"      CHECK-QUIET-CANDIDATE! 0 T=

\ op-kind family negatives: a raw code cannot open a node record, the stored op
\ cannot leak as n, and opkind<->dtype cross-swaps reject both directions.
s" MTX-OPB-OK   ( opkind -- ) MIR-OP-BEGIN"            CHECK-QUIET-CANDIDATE! -1 T=
s" MTX-OPB-N    ( n -- ) MIR-OP-BEGIN"                 CHECK-QUIET-CANDIDATE! 0 T=
s" MTX-OPB-DT   ( dtype -- ) MIR-OP-BEGIN"             CHECK-QUIET-CANDIDATE! 0 T=
s" MTX-OP-NOUT  ( CAD-KIND:node-id -- n ) MIR-OP@"     CHECK-QUIET-CANDIDATE! 0 T=
s" MTX-OP-ASDT  ( CAD-KIND:node-id -- dtype ) MIR-OP@" CHECK-QUIET-CANDIDATE! 0 T=
s" MTX-DT-ASOP  ( CAD-KIND:rows CAD-KIND:cols opkind opkind -- MIR:input-slot ) MIR-INPUT+" CHECK-QUIET-CANDIDATE! 0 T=

\ derived op identity (MAKI-OPKIND:EQ, DERIVE eq S1): both args must be opkind
s" MTX-EQ-OK    ( opkind opkind -- bool ) MAKI-OPKIND:EQ" CHECK-QUIET-CANDIDATE! -1 T=
s" MTX-EQ-DT    ( dtype opkind -- bool ) MAKI-OPKIND:EQ"  CHECK-QUIET-CANDIDATE! 0 T=
s" MTX-EQ-N     ( n opkind -- bool ) MAKI-OPKIND:EQ"      CHECK-QUIET-CANDIDATE! 0 T=

\ provenance family negatives: a raw n cannot adopt, the stored provenance cannot
\ leak as n or as a foreign family, and the derived EQ takes prov only.
s" MTX-PRV-OK   ( prov -- ) MIR-PROV!"                    CHECK-QUIET-CANDIDATE! -1 T=
s" MTX-PRV-N    ( n -- ) MIR-PROV!"                       CHECK-QUIET-CANDIDATE! 0 T=
s" MTX-PRV-NOUT ( -- n ) MIR-PROV@"                       CHECK-QUIET-CANDIDATE! 0 T=
s" MTX-PRV-ASOP ( -- opkind ) MIR-PROV@"                  CHECK-QUIET-CANDIDATE! 0 T=
s" MTX-PRV-EQN  ( n prov -- bool ) MAKI-PROV:EQ"          CHECK-QUIET-CANDIDATE! 0 T=

\ typed count negatives (Model-CAD V2 B5.5): each accessor returns a
\ CAD-NUM:item-count. Positive controls pin the correct signature; the negatives
\ pin that a count cannot leak as a raw n and cannot pass as a CAD-NUM:index -
\ the last pair drives a real consumer (ADVANCE-INDEX ( index item-count -- ... )):
\ the count is accepted in the item-count operand but rejected in the index operand.
s" MTX-NC-OK    ( -- CAD-NUM:item-count ) NODE-COUNT@"        CHECK-QUIET-CANDIDATE! -1 T=
s" MTX-SC-OK    ( -- CAD-NUM:item-count ) SLOT-COUNT@"        CHECK-QUIET-CANDIDATE! -1 T=
s" MTX-OC-OK    ( -- CAD-NUM:item-count ) OPERAND-COUNT@"     CHECK-QUIET-CANDIDATE! -1 T=
s" MTX-MC-OK    ( -- CAD-NUM:item-count ) MATERIALIZED-COUNT" CHECK-QUIET-CANDIDATE! -1 T=
s" MTX-NC-NOUT  ( -- n ) NODE-COUNT@"                         CHECK-QUIET-CANDIDATE! 0 T=
s" MTX-OC-NOUT  ( -- n ) OPERAND-COUNT@"                      CHECK-QUIET-CANDIDATE! 0 T=
s" MTX-NC-ASIDX ( -- CAD-NUM:index ) NODE-COUNT@"             CHECK-QUIET-CANDIDATE! 0 T=
s" MTX-MC-ASIDX ( -- CAD-NUM:index ) MATERIALIZED-COUNT"     CHECK-QUIET-CANDIDATE! 0 T=
s" MTX-ADV-OK   ( CAD-NUM:index -- CAD-NUM:numeric-result<CAD-NUM:index> ) NODE-COUNT@ CAD-NUM:ADVANCE-INDEX"      CHECK-QUIET-CANDIDATE! -1 T=
s" MTX-ADV-SWAP ( CAD-NUM:item-count -- CAD-NUM:numeric-result<CAD-NUM:index> ) NODE-COUNT@ CAD-NUM:ADVANCE-INDEX" CHECK-QUIET-CANDIDATE! 0 T=

T-REPORT

;package

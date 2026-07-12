\ maki/model-ir-test.f - checked tests for the model IR node table, operand ref
\ tagging, output descriptor facts, shape/dtype/layout keys, render, fail-closed.
\ Test-local names are MT- prefixed; suites share the MAKI wordlist.

require lib/test.f
require lib/string.f
require test/checker-assert.f
require maki/model-ir.f

package MAKI

\ ---- render containment helper ----------------------------------------------
variable MT-VA  variable MT-VU
: MT-SAVE ( ptr u8 n -- )  MT-VU ! MT-VA ! ;
: MT-IN ( ptr u8 n -- )  MT-VA @ MT-VU @ 2swap CONTAINS? TTRUE ;

\ ---- build a small IR:  LINEAR(x,w) -> GELU  --------------------------------
variable MT-SX
variable MT-SW
variable MT-SU
variable MT-N0
variable MT-N1

: MT-SX! ( MIR:input-slot -- )  MT-SX ! ;
: MT-SX@ ( -- MIR:input-slot )  MT-SX @ ;
: MT-SW! ( MIR:input-slot -- )  MT-SW ! ;
: MT-SW@ ( -- MIR:input-slot )  MT-SW @ ;
: MT-SU! ( MIR:input-slot -- )  MT-SU ! ;
: MT-SU@ ( -- MIR:input-slot )  MT-SU @ ;
: MT-N0! ( CAD-KIND:node-id -- )  MT-N0 ! ;
: MT-N0@ ( -- CAD-KIND:node-id )  MT-N0 @ ;
: MT-N1! ( CAD-KIND:node-id -- )  MT-N1 ! ;
: MT-N1@ ( -- CAD-KIND:node-id )  MT-N1 @ ;

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

\ ---- node facts -------------------------------------------------------------
MT-N0@ MIR-OP@ OPKIND>N OP-LINEAR T=
MT-N1@ MIR-OP@ OPKIND>N OP-GELU   T=
MT-N1@ dup MIR-ROWS@ swap MIR-COLS@ 2 4 SHAPE-IS? TTRUE
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
variable MT-MV
: MT-MV! ( CAD-KIND:node-id -- )  MT-MV ! ;
: MT-MV@ ( -- CAD-KIND:node-id )  MT-MV @ ;

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
: TRY-MIR-CAP     ( -- )
   MIR-RESET  MIR-CAP 1+ 0 ?do  MAKI-OPKIND:CAST MIR-OP-BEGIN  1 1 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ drop  loop ;
: TRY-MIR-SLOTCAP ( -- )
   MIR-RESET  MIR-IN-CAP 1+ 0 ?do  1 1 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop  loop ;

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
1 LAYOUT-BUFFER MT-MARK-BUF MIR:mark

: MT-MARK! ( MIR:mark -- )
   0 MT-MARK-BUF ! ;

: MT-MARK@ ( -- MIR:mark )
   0 MT-MARK-BUF @ ;

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

T-REPORT

;package

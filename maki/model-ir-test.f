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
variable MT-SX  variable MT-SW  variable MT-SU  variable MT-N0  variable MT-N1
MIR-RESET
2 3 MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ MT-SX !   \ slot 0 = x (2x3)
3 4 MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ MT-SW !   \ slot 1 = w (3x4)
0 0 MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ MT-SU !   \ slot 2 = unbound (?x?)

MAKI-OPKIND:LINEAR MIR-OP-BEGIN
   MT-SX @ MIR-IN-REF MIR-IN+
   MT-SW @ MIR-IN-REF MIR-IN+
2 4 MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ MT-N0 !  \ node 0 : y0 = 2x4, materialized

MAKI-OPKIND:GELU MIR-OP-BEGIN
   MT-N0 @ MIR-IN+                       \ node ref (>=0)
2 4 MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ MT-N1 !  \ node 1 : y1 = 2x4

T-RESET

\ ---- table shape ------------------------------------------------------------
MIR-N@         2 T=
MIR-IN-SLOTS@  3 T=

\ ---- node facts -------------------------------------------------------------
MT-N0 @ MIR-OP@ OPKIND>N OP-LINEAR T=
MT-N1 @ MIR-OP@ OPKIND>N OP-GELU   T=
MT-N1 @ MIR-ROWS@ 2 T=
MT-N1 @ MIR-COLS@ 4 T=
MT-N1 @ MIR-DT@   DTYPE>N DT-F32   T=
MT-N1 @ MIR-LAY@  LAYOUT>N LAY-ROW T=
MT-N0 @ MIR-IN-COUNT@ 2 T=
MT-N1 @ MIR-IN-COUNT@ 1 T=

\ ---- operand ref tagging: input slot vs committed node ----------------------
MT-N0 @ 0 MIR-IN@  MIR-REF-INPUT? TTRUE
MT-N0 @ 0 MIR-IN@  MIR-REF-SLOT   MT-SX @ T=       \ x -> slot 0
MT-N0 @ 1 MIR-IN@  MIR-REF-SLOT   MT-SW @ T=       \ w -> slot 1
MT-N1 @ 0 MIR-IN@  MIR-REF-INPUT? TFALSE
MT-N1 @ 0 MIR-IN@  MIR-REF-NODE   MT-N0 @ T=       \ gelu consumes node 0

\ ---- materialization flag + count ------------------------------------------
MT-N0 @ MIR-MAT@ TTRUE
MIR-MAT-COUNT 2 T=
0 MT-N0 @ MIR-MAT!                                \ fusion clears node 0
MT-N0 @ MIR-MAT@ TFALSE
MIR-MAT-COUNT 1 T=
1 MT-N0 @ MIR-MAT!                                \ restore for later checks

\ ---- shape / dtype / layout keys -------------------------------------------
MT-N1 @ MIR-SHAPE-KEY  s" 2x4" T$=
MT-N1 @ MIR-DTYPE-KEY  s" f32" T$=
MT-N1 @ MIR-LAYOUT-KEY s" row" T$=
MT-SX @ MIR-SLOT-SHAPE-KEY s" 2x3" T$=
MT-SU @ MIR-SLOT-SHAPE-KEY s" ?x?" T$=           \ unbound extents render "?"

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
MIR-RESET
4 8 MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop                     \ slot 0 = x (4x8)
MAKI-OPKIND:RESHAPE MIR-OP-BEGIN
   0 MIR-IN-REF MIR-IN+
8 4 MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW  MV-RESHAPE MVV-FREE 8 4 MV-PACK  0  MIR-OP+ MT-MV !

MT-MV @ MIR-MOVE?          TTRUE
MT-MV @ MIR-OP@ OPKIND>N OP-RESHAPE T=
MT-MV @ MIR-MOVE-VERDICT@  MVV-FREE T=
MT-MV @ MIR-SHAPE-KEY s" 8x4" T$=
MT-MV @ MIR-MAT@           TFALSE                       \ free -> not materialized

MIR-RENDER MT-SAVE
s" node.0.op: reshape"    MT-IN
s" node.0.verdict: free"  MT-IN

\ ---- fail-closed probes -----------------------------------------------------
: TRY-MIR-IDX     ( -- )  MIR-N@ MIR-OP@ drop ;
\ (a bad op-kind tag is a checker reject now - pinned by the opkind negatives
\ below - so the old TRY-MIR-OPKIND runtime throw probe is unrepresentable)
: TRY-MIR-REF     ( -- )  MIR-RESET MAKI-OPKIND:GELU MIR-OP-BEGIN 5 MIR-IN+ ;
: TRY-MIR-STATE   ( -- )  MIR-RESET 0 MIR-IN+ ;
\ (a bad dtype/layout tag is a checker reject now - pinned by the swapped-family
\ negatives below - so the old TRY-MIR-DT runtime throw probe is unrepresentable)
: TRY-MIR-INSLOT  ( -- )  MIR-RESET 0 MIR-SLOT-ROWS@ drop ;
: TRY-MIR-CAP     ( -- )
   MIR-RESET  MIR-CAP 1+ 0 ?do  MAKI-OPKIND:CAST MIR-OP-BEGIN  1 1 MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ drop  loop ;
: TRY-MIR-SLOTCAP ( -- )
   MIR-RESET  MIR-IN-CAP 1+ 0 ?do  1 1 MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop  loop ;

\ a verdict requested from a non-movement (gelu) node fails closed
: TRY-MIR-NOTMOVE ( -- )
   MIR-RESET  0 0 MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop
   MAKI-OPKIND:GELU MIR-OP-BEGIN  0 MIR-IN-REF MIR-IN+
   0 0 MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+  MIR-MOVE-VERDICT@ drop ;

' TRY-MIR-IDX     E-MIR-IDX     TTHROWS
' TRY-MIR-REF     E-MIR-REF     TTHROWS
' TRY-MIR-STATE   E-MIR-STATE   TTHROWS
' TRY-MIR-INSLOT  E-MIR-INSLOT  TTHROWS
' TRY-MIR-CAP     E-MIR-CAP     TTHROWS
' TRY-MIR-SLOTCAP E-MIR-INSLOT  TTHROWS
' TRY-MIR-NOTMOVE E-MV-NOTMOVE  TTHROWS

\ ---- swapped-family negatives (dot habu-cad-adt-swap, corrected plan) --------
\ THE headline hole this dot exists to close: a dtype/layout swap at the
\ MIR-INPUT+/MIR-OP+ API boundary is a CHECKER reject (dtype and layout are no
\ longer indistinguishable bytes). Positive controls pin the well-typed calls;
\ n-launder rows pin that a raw code cannot enter and a family cannot leak as n.
s" MTX-IN-OK    ( n n dtype layout -- n ) MIR-INPUT+"  CHECK-QUIET-CANDIDATE! -1 T=
s" MTX-IN-SWAP  ( n n layout dtype -- n ) MIR-INPUT+"  CHECK-QUIET-CANDIDATE! 0 T=
s" MTX-IN-NDT   ( n n n layout -- n ) MIR-INPUT+"      CHECK-QUIET-CANDIDATE! 0 T=
s" MTX-OP-OK    ( n n dtype layout n n -- n ) MIR-OP+" CHECK-QUIET-CANDIDATE! -1 T=
s" MTX-OP-SWAP  ( n n layout dtype n n -- n ) MIR-OP+" CHECK-QUIET-CANDIDATE! 0 T=
s" MTX-OP-NLAY  ( n n dtype n n n -- n ) MIR-OP+"      CHECK-QUIET-CANDIDATE! 0 T=
s" MTX-AL-OK    ( n align -- ) MIR-SLOT-AL!"           CHECK-QUIET-CANDIDATE! -1 T=
s" MTX-AL-N     ( n n -- ) MIR-SLOT-AL!"               CHECK-QUIET-CANDIDATE! 0 T=
s" MTX-AL-DT    ( n dtype -- ) MIR-SLOT-AL!"           CHECK-QUIET-CANDIDATE! 0 T=
\ accessor outputs are families, not n: enum->n laundering rejects
s" MTX-DT-NOUT  ( n -- n ) MIR-DT@"                    CHECK-QUIET-CANDIDATE! 0 T=
s" MTX-LAY-NOUT ( n -- n ) MIR-SLOT-LAY@"              CHECK-QUIET-CANDIDATE! 0 T=
s" MTX-AL-NOUT  ( n -- n ) MIR-SLOT-AL@"               CHECK-QUIET-CANDIDATE! 0 T=

\ op-kind family negatives: a raw code cannot open a node record, the stored op
\ cannot leak as n, and opkind<->dtype cross-swaps reject both directions.
s" MTX-OPB-OK   ( opkind -- ) MIR-OP-BEGIN"            CHECK-QUIET-CANDIDATE! -1 T=
s" MTX-OPB-N    ( n -- ) MIR-OP-BEGIN"                 CHECK-QUIET-CANDIDATE! 0 T=
s" MTX-OPB-DT   ( dtype -- ) MIR-OP-BEGIN"             CHECK-QUIET-CANDIDATE! 0 T=
s" MTX-OP-NOUT  ( n -- n ) MIR-OP@"                    CHECK-QUIET-CANDIDATE! 0 T=
s" MTX-OP-ASDT  ( n -- dtype ) MIR-OP@"                CHECK-QUIET-CANDIDATE! 0 T=
s" MTX-DT-ASOP  ( n n opkind opkind -- n ) MIR-INPUT+" CHECK-QUIET-CANDIDATE! 0 T=

T-REPORT

end-package

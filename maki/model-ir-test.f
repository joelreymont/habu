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
2 3 SHAPE DT-F32 LAY-ROW MIR-INPUT+ MT-SX!   \ slot 0 = x (2x3)
3 4 SHAPE DT-F32 LAY-ROW MIR-INPUT+ MT-SW!   \ slot 1 = w (3x4)
0 0 SHAPE DT-F32 LAY-ROW MIR-INPUT+ MT-SU!   \ slot 2 = unbound (?x?)

OP-LINEAR MIR-OP-BEGIN
   MT-SX@ MIR-IN-REF MIR-IN+
   MT-SW@ MIR-IN-REF MIR-IN+
2 4 SHAPE DT-F32 LAY-ROW 0 1 MIR-OP+ MT-N0!  \ node 0 : y0 = 2x4, materialized

OP-GELU MIR-OP-BEGIN
   MT-N0@ MIR-NODE-REF MIR-IN+
2 4 SHAPE DT-F32 LAY-ROW 0 1 MIR-OP+ MT-N1!  \ node 1 : y1 = 2x4

T-RESET

\ ---- table shape ------------------------------------------------------------
MIR-N@         2 T=
MIR-IN-SLOTS@  3 T=

\ ---- node facts -------------------------------------------------------------
MT-N0@ MIR-OP@ OP-LINEAR T=
MT-N1@ MIR-OP@ OP-GELU   T=
MT-N1@ dup MIR-ROWS@ swap MIR-COLS@ 2 4 SHAPE-IS? TTRUE
MT-N1@ MIR-DT@ DT-F32 DTYPE-EQUAL? TTRUE
MT-N1@ MIR-LAY@ LAY-ROW LAYOUT-EQUAL? TTRUE
MT-N0@ MIR-IN-COUNT@ 2 T=
MT-N1@ MIR-IN-COUNT@ 1 T=

\ ---- operand ref tagging: input slot vs committed node ----------------------
MT-N0@ 0 MIR-INPUT-IDX MIR-IN@ MIR-REF-INPUT? TTRUE
MT-N0@ 0 MIR-INPUT-IDX MIR-IN@ MIR-REF-SLOT MT-SX@ MT-SLOT=
MT-N0@ 1 MIR-INPUT-IDX MIR-IN@ MIR-REF-SLOT MT-SW@ MT-SLOT=
MT-N1@ 0 MIR-INPUT-IDX MIR-IN@ MIR-REF-INPUT? TFALSE
MT-N1@ 0 MIR-INPUT-IDX MIR-IN@ MIR-REF-NODE MT-N0@ MT-NODE=

\ ---- materialization flag + count ------------------------------------------
MT-N0@ MIR-MAT@ TTRUE
MIR-MAT-COUNT 2 T=
0 MT-N0@ MIR-MAT!
MT-N0@ MIR-MAT@ TFALSE
MIR-MAT-COUNT 1 T=
1 MT-N0@ MIR-MAT!

\ ---- shape / dtype / layout keys -------------------------------------------
MT-N1@ MIR-SHAPE-KEY  s" 2x4" T$=
MT-N1@ MIR-DTYPE-KEY  s" f32" T$=
MT-N1@ MIR-LAYOUT-KEY s" row" T$=
MT-SX@ MIR-SLOT-SHAPE-KEY s" 2x3" T$=
MT-SU@ MIR-SLOT-SHAPE-KEY s" ?x?" T$=

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
4 8 SHAPE DT-F32 LAY-ROW MIR-INPUT+ drop
OP-RESHAPE MIR-OP-BEGIN
   0 MIR-SLOT-ID MIR-IN-REF MIR-IN+
8 4 SHAPE DT-F32 LAY-ROW MV-RESHAPE MVV-FREE 8 4 MV-PACK 0 MIR-OP+ MT-MV!

MT-MV@ MIR-MOVE?          TTRUE
MT-MV@ MIR-OP@ OP-RESHAPE T=
MT-MV@ MIR-MOVE-VERDICT@  MVV-FREE T=
MT-MV@ MIR-SHAPE-KEY s" 8x4" T$=
MT-MV@ MIR-MAT@           TFALSE

MIR-RENDER MT-SAVE
s" node.0.op: reshape"    MT-IN
s" node.0.verdict: free"  MT-IN

\ ---- fail-closed probes -----------------------------------------------------
: TRY-MIR-IDX     ( -- )  MIR-N@ MIR-NODE-ID MIR-OP@ drop ;
: TRY-MIR-OPKIND  ( -- )  MIR-RESET OP-N MIR-OP-BEGIN ;
: TRY-MIR-REF     ( -- )  MIR-RESET OP-GELU MIR-OP-BEGIN 5 RAW>REF MIR-IN+ ;
: TRY-MIR-STATE   ( -- )
   MIR-RESET
   1 1 SHAPE DT-F32 LAY-ROW MIR-INPUT+ MIR-IN-REF MIR-IN+ ;
: TRY-MIR-INSLOT  ( -- )  MIR-RESET 0 RAW>SLOT MIR-SLOT-ROWS@ drop ;
: TRY-MIR-CAP     ( -- )
   MIR-RESET  MIR-CAP 1+ 0 ?do  OP-CAST MIR-OP-BEGIN  1 1 SHAPE DT-F32 LAY-ROW 0 1 MIR-OP+ drop  loop ;
: TRY-MIR-SLOTCAP ( -- )
   MIR-RESET  MIR-IN-CAP 1+ 0 ?do  1 1 SHAPE DT-F32 LAY-ROW MIR-INPUT+ drop  loop ;

\ a verdict requested from a non-movement (gelu) node fails closed
: TRY-MIR-NOTMOVE ( -- )
   MIR-RESET
   0 0 SHAPE DT-F32 LAY-ROW MIR-INPUT+ {: s:MIR:input-slot :}
   OP-GELU MIR-OP-BEGIN s MIR-IN-REF MIR-IN+
   0 0 SHAPE DT-F32 LAY-ROW 0 1 MIR-OP+  MIR-MOVE-VERDICT@ drop ;

: TRY-MIR-MARK-GROW ( -- )
   MIR-RESET
   1 0 0 MIR-MARK:MAKE MIR-RELEASE ;

' TRY-MIR-IDX     E-MIR-IDX     TTHROWS
' TRY-MIR-OPKIND  E-MIR-OPKIND  TTHROWS
' TRY-MIR-REF     E-MIR-REF     TTHROWS
' TRY-MIR-STATE   E-MIR-STATE   TTHROWS
' TRY-MIR-INSLOT  E-MIR-INSLOT  TTHROWS
' TRY-MIR-CAP     E-MIR-CAP     TTHROWS
' TRY-MIR-SLOTCAP E-MIR-INSLOT  TTHROWS
' TRY-MIR-NOTMOVE E-MV-NOTMOVE  TTHROWS
' TRY-MIR-MARK-GROW E-MIR-STATE TTHROWS

\ ---- nominal handle rejection ----------------------------------------------
s" MT-TYPED-NODE ( CAD-KIND:node-id -- n ) MIR-OP@" MT-CHECK-YES
s" MT-TYPED-SLOT ( MIR:input-slot -- CAD-KIND:rows ) MIR-SLOT-ROWS@" MT-CHECK-YES
s" MT-TYPED-REF ( MIR:operand-ref -- bool ) MIR-REF-INPUT?" MT-CHECK-YES
s" MT-TYPED-INPUT ( CAD-KIND:node-id MIR:input-index -- MIR:operand-ref ) MIR-IN@" MT-CHECK-YES

s" MT-NODE-AS-SLOT ( CAD-KIND:node-id -- CAD-KIND:rows ) MIR-SLOT-ROWS@" MT-CHECK-NO
s" MT-NODE-AS-REF ( CAD-KIND:node-id -- ) MIR-IN+" MT-CHECK-NO
s" MT-NODE-AS-OBJ ( CAD-KIND:obj-id -- n ) MIR-OP@" MT-CHECK-NO
s" MT-N-AS-INPUT ( CAD-KIND:node-id n -- MIR:operand-ref ) MIR-IN@" MT-CHECK-NO
s" MT-RAW-DESC ( n n n CAD-KIND:layout -- MIR:input-slot ) MIR-INPUT+" MT-CHECK-NO

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
   2 3 SHAPE DT-F32 LAY-ROW MIR-INPUT+ {: s:MIR:input-slot :}
   OP-GELU MIR-OP-BEGIN
   s MIR-IN-REF MIR-IN+
   2 3 SHAPE DT-F32 LAY-ROW 0 1 MIR-OP+ {: base:CAD-KIND:node-id :}
   MIR-MARK MT-MARK!
   4 5 SHAPE DT-F32 LAY-ROW MIR-INPUT+ drop
   OP-RELU MIR-OP-BEGIN
   base MIR-NODE-REF MIR-IN+
   2 3 SHAPE DT-F32 LAY-ROW 0 1 MIR-OP+ drop
   MT-MARK@ MIR-RELEASE
   base ;

MT-ROLLBACK MIR-OP@ OP-GELU T=
MIR-N@ 1 T=
MIR-IN-SLOTS@ 1 T=

T-REPORT

;package

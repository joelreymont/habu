\ maki/backward-test.f - checked tests for the model-IR reverse transform (cad-9b/9e).
\ Backward nodes appended as ordinary IR: elementwise *-bwd emission, fan-out
\ cotangent summation via OP-ADD, matmul transpose+matmul adjoints, add copy-through,
\ the seeded output cotangent, per-input gradient reporting, that a backward
\ elementwise chain fuses under FP-BUILD, and every fail-closed path. cad-9e adds the
\ reduce/scatter adjoint emission (bias/linear/slice/gather/scale) with shape asserts
\ and the scale partial-broadcast fail-closed boundary.

require lib/test.f
require lib/string.f
require maki/cad.f
require maki/backward.f
require maki/fusion-plan.f

package MAKI

\ ---- render containment helper (report rows) -------------------------------
variable BT-VA  variable BT-VU
: BT-SAVE ( ptr u8 n -- )  BT-VU ! BT-VA ! ;
: BT-IN ( ptr u8 n -- )  BT-VA @ BT-VU @ 2swap CONTAINS? TTRUE ;
: BT-SLOT ( n -- MIR:input-slot )  MIR-SLOT-ID ;
: BT-SLOT-REF ( n -- MIR:operand-ref )  BT-SLOT MIR-IN-REF ;
: BT-NODE ( n -- CAD-KIND:node-id )  MIR-NODE-ID ;
: BT-NODE-REF ( n -- MIR:operand-ref )  BT-NODE MIR-NODE-REF ;
: BT-OP@ ( n -- n )  BT-NODE MIR-OP@ ;
: BT-ROWS@ ( n -- CAD-KIND:rows )  BT-NODE MIR-ROWS@ ;
: BT-COLS@ ( n -- CAD-KIND:cols )  BT-NODE MIR-COLS@ ;
: BT-ATTR@ ( n -- n )  BT-NODE MIR-ATTR@ ;
: BT-IN-COUNT@ ( n -- n )  BT-NODE MIR-IN-COUNT@ ;
: BT-IN@ ( n n -- MIR:operand-ref )
   {: node:n idx:n :}  node BT-NODE idx MIR-INPUT-IDX MIR-IN@ ;
: BT-RID@ ( n -- n )  BT-NODE FP-RID@ ;
: BT-GRAD@ ( n -- MIR:operand-ref )  BT-SLOT BW-SLOT-GRAD@ ;
: BT-GRAD-NODE@ ( n -- CAD-KIND:node-id )  BT-GRAD@ MIR-REF-NODE ;
: BT-GRAD-NODE-RAW ( n -- n )  BT-GRAD-NODE@ NODE>RAW ;
: BT-HAS-GRAD? ( n -- bool )  BT-SLOT BW-HAS-GRAD? ;
: BT-SEED-RAW ( -- n )  BW-SEED-SLOT@ SLOT>RAW ;
: BT-REF= ( MIR:operand-ref MIR:operand-ref -- )  MIR-REF= TTRUE ;

\ ---- fail-closed probes (MODEL: parses at runtime, so build the IR by hand) --
: BT-MK1 ( n -- )  {: op:n :}                            \ single op over one 2x2 input
   MIR-RESET  2 2 SHAPE DT-F32 LAY-ROW MIR-INPUT+ {: s:MIR:input-slot :}
   op MIR-OP-BEGIN s MIR-IN-REF MIR-IN+
      2 2 SHAPE DT-F32 LAY-ROW 0 1 MIR-OP+ drop ;
: BT-MK-SCALE ( n n -- ) {: sr:n sc:n :}                 \ SCALE(x:2x3, s:sr x sc)
   MIR-RESET
   2 3 SHAPE DT-F32 LAY-ROW MIR-INPUT+ {: x:MIR:input-slot :}
   sr sc SHAPE DT-F32 LAY-ROW MIR-INPUT+ {: s:MIR:input-slot :}
   OP-SCALE MIR-OP-BEGIN x MIR-IN-REF MIR-IN+ s MIR-IN-REF MIR-IN+
   2 3 SHAPE DT-F32 LAY-ROW 0 1 MIR-OP+ drop ;
: BT-TRY-STATE ( -- )  BW-FWD-N@ drop ;                  \ accessor before build
: BT-TRY-EMPTY ( -- )  MIR-RESET BW-BUILD ;              \ empty IR
: BT-TRY-CAST  ( -- )  OP-CAST  BT-MK1 BW-BUILD ;           \ no adjoint (non-differentiable)
: BT-TRY-SCALE-BC ( -- )  1 3 BT-MK-SCALE BW-BUILD ;        \ s=1x3: partial broadcast (v1)
: BT-BUILD-FANOUT ( -- )
   MIR-RESET
   0 0 SHAPE DT-F32 LAY-ROW MIR-INPUT+ {: s:MIR:input-slot :}
   OP-GELU MIR-OP-BEGIN s MIR-IN-REF MIR-IN+
   0 0 SHAPE DT-F32 LAY-ROW 0 1 MIR-OP+ {: nd:CAD-KIND:node-id :}
   OP-ADD MIR-OP-BEGIN nd MIR-NODE-REF MIR-IN+ nd MIR-NODE-REF MIR-IN+
   0 0 SHAPE DT-F32 LAY-ROW 0 1 MIR-OP+ drop ;

T-RESET

\ ---- accessor before BW-BUILD fails closed (runs first: BW-BUILT? still 0) ---
' BT-TRY-STATE E-BW-STATE TTHROWS

\ ---- pure elementwise chain GELU GELU: two gelu-bwd nodes that FUSE ----------
MODEL: CHAIN ( x:4x8 -- y ) GELU GELU ;
BW-CAN? TTRUE
BW-FIRST-BAD -1 T=
BW-BUILD
BW-FWD-N@      2 T=
BW-BWD-COUNT   2 T=
2 BT-OP@ OP-GELU-BWD T=                       \ first appended backward node
3 BT-OP@ OP-GELU-BWD T=
\ the seed cotangent lives in a fresh input slot for the output node (node 1)
BT-SEED-RAW 1 T=                             \ slots: 0 = x, 1 = seed
\ the backward elementwise chain fuses into ONE region
FP-BUILD
2 BT-RID@ 3 BT-RID@ T=
\ input x receives a gradient (produced by the last backward node)
0 BT-GRAD-NODE-RAW 3 T=

\ ---- fan-out: a value used twice sums its cotangents via OP-ADD -------------
\ n0 = gelu(i0) ; n1 = add(n0, n0) -> n0 used twice on the backward path.
BT-BUILD-FANOUT
BW-BUILD
BW-BWD-COUNT 2 T=
2 BT-OP@ OP-ADD      T=                       \ the fan-out summation node
3 BT-OP@ OP-GELU-BWD T=
0 BT-GRAD-NODE-RAW 3 T=                  \ x-grad = gelu-bwd(sum, x)

\ ---- add copies the cotangent to BOTH inputs (no new nodes) -----------------
MODEL: ADDM ( x:2x2 y:2x2 -- z ) ADD ;
BW-BUILD
BW-BWD-COUNT 0 T=                              \ pure copy: nothing emitted
\ both input grads are the seed cotangent (an input ref, slot 2)
0 BT-GRAD@ 2 BT-SLOT-REF BT-REF=
1 BT-GRAD@ 2 BT-SLOT-REF BT-REF=

\ ---- matmul: dX = ct @ Wt, dW = Xt @ ct (transpose + matmul nodes) ----------
MODEL: MM ( x:2x3 w:3x4 -- y ) MATMUL ;
BW-BUILD
BW-BWD-COUNT 4 T=
1 BT-OP@ OP-TRANSPOSE T=                      \ Wt
2 BT-OP@ OP-MATMUL    T=                      \ dX
3 BT-OP@ OP-TRANSPOSE T=                      \ Xt
4 BT-OP@ OP-MATMUL    T=                      \ dW
\ dX has X's shape (2x3), dW has W's shape (3x4)
0 BT-GRAD-NODE-RAW 2 T=   2 BT-ROWS@ 2 ROWS-IS? TTRUE  2 BT-COLS@ 3 COLS-IS? TTRUE
1 BT-GRAD-NODE-RAW 4 T=   4 BT-ROWS@ 3 ROWS-IS? TTRUE  4 BT-COLS@ 4 COLS-IS? TTRUE

\ ---- softmax adjoint reads the OUTPUT row (the forward node itself) ----------
MODEL: SM ( x:4x8 -- y ) SOFTMAX-ROW ;
BW-BUILD
BW-BWD-COUNT 1 T=
1 BT-OP@ OP-SOFTMAX-ROW-BWD T=
1 1 BT-IN@ 0 BT-NODE-REF BT-REF=             \ operand 1 = the saved output node 0

\ ---- reduction adjoints emit their dedicated backward op over (ct, saved input) --
MODEL: LN ( x:4x8 -- y ) LAYERNORM ;
BW-BUILD
BW-BWD-COUNT 1 T=
1 BT-OP@ OP-LAYERNORM-BWD T=
1 1 BT-IN@ 0 BT-SLOT-REF BT-REF=              \ operand 1 = the saved input x
MODEL: RMS ( x:4x8 -- y ) RMSNORM ;
BW-BUILD
1 BT-OP@ OP-RMSNORM-BWD T=

\ ---- rope adjoint: a 3-operand backward op (cotangent, cos, sin) via BW-OP3 ------
MODEL: RP ( x:2x4 c:2x4 s:2x4 -- y ) ROPE ;
BW-BUILD
BW-BWD-COUNT 1 T=
1 BT-OP@ OP-ROPE-BWD T=
1 BT-IN-COUNT@ 3 T=                           \ cotangent + cos + sin
0 BT-GRAD-NODE-RAW 1 T=                  \ only x receives a gradient (node 1)

\ ---- report rows: seed, node count, per-input gradient ----------------------
MODEL: R ( x:4x8 -- y ) GELU ;
BW-BUILD
REPORT:NEW BW-INTO REPORT:RENDER BT-SAVE
s" backward.seed: input 1" BT-IN
s" backward.nodes: fwd=1 bwd=1" BT-IN
s" backward.grad: input 0 <- node 1" BT-IN

\ ---- cad-9e: bias adjoint (dx = cotangent copy + d-bias = OP-ROWSUM-BWD) -----
MODEL: BIASM ( x:2x3 b:1x3 -- y ) BIAS ;
BW-BUILD
BW-BWD-COUNT 1 T=
1 BT-OP@ OP-ROWSUM-BWD T=
1 BT-ROWS@ 1 ROWS-IS? TTRUE  1 BT-COLS@ 3 COLS-IS? TTRUE
0 BT-GRAD@ 2 BT-SLOT-REF BT-REF=          \ dx = the seed cotangent (input slot 2)
1 BT-GRAD-NODE-RAW 1 T=                   \ d-bias = the row-reduce node

\ ---- cad-9e: linear adjoint (matmul adjoints + d-bias = OP-ROWSUM-BWD) -------
MODEL: LINM ( x:2x3 w:3x4 b:1x4 -- y ) LINEAR ;
BW-BUILD
BW-BWD-COUNT 5 T=                               \ 2 transpose + 2 matmul + 1 rowsum
0 BT-GRAD-NODE@ MIR-ROWS@ 2 ROWS-IS? TTRUE
0 BT-GRAD-NODE@ MIR-COLS@ 3 COLS-IS? TTRUE
1 BT-GRAD-NODE@ MIR-ROWS@ 3 ROWS-IS? TTRUE
1 BT-GRAD-NODE@ MIR-COLS@ 4 COLS-IS? TTRUE
2 BT-GRAD-NODE@ MIR-OP@ OP-ROWSUM-BWD T=
2 BT-GRAD-NODE@ MIR-ROWS@ 1 ROWS-IS? TTRUE
2 BT-GRAD-NODE@ MIR-COLS@ 4 COLS-IS? TTRUE

\ ---- cad-9e: slice adjoint (OP-PAD-SCATTER at the forward slice offset) ------
MODEL: SLM ( x:4x4 -- y ) SLICE:1..3 ;
BW-CAN? TTRUE
BW-BUILD
BW-BWD-COUNT 1 T=
1 BT-OP@ OP-PAD-SCATTER T=
1 BT-ROWS@ 4 ROWS-IS? TTRUE  1 BT-COLS@ 4 COLS-IS? TTRUE
1 BT-ATTR@ MV-PA@ 1 T=  1 BT-ATTR@ MV-PB@ 3 T=   \ r0=1, r1=3 from the forward slice
0 BT-GRAD-NODE-RAW 1 T=

\ ---- cad-9e: gather adjoint (OP-SCATTER-ADD at the gathered indices) ---------
MODEL: GAM ( x:4x2 idx:3x1 -- y ) GATHER ;
BW-BUILD
BW-BWD-COUNT 1 T=
1 BT-OP@ OP-SCATTER-ADD T=
1 BT-ROWS@ 4 ROWS-IS? TTRUE  1 BT-COLS@ 2 COLS-IS? TTRUE
1 BT-IN-COUNT@ 2 T=                            \ cotangent + index operand
1 1 BT-IN@ 1 BT-SLOT-REF BT-REF=               \ operand 1 = the index input (slot 1)
0 BT-GRAD-NODE-RAW 1 T=                        \ x receives the scatter-add node
1 BT-HAS-GRAD? TFALSE                           \ the index operand gets no gradient

\ ---- cad-9e: scale adjoint, same-shape operand (elementwise product rule) ----
MODEL: SCM ( x:2x3 s:2x3 -- z ) SCALE ;
BW-BUILD
BW-BWD-COUNT 2 T=
1 BT-OP@ OP-MUL T=  2 BT-OP@ OP-MUL T=
0 BT-HAS-GRAD? TTRUE  1 BT-HAS-GRAD? TTRUE

\ ---- cad-9e: scale adjoint, 1x1 scalar operand (broadcast-scale + full-reduce dot) --
MODEL: SCS ( x:2x3 s:1x1 -- z ) SCALE ;
BW-BUILD
BW-BWD-COUNT 2 T=
1 BT-OP@ OP-SCALE T=                           \ dx = broadcast scale(ct, s)
2 BT-OP@ OP-FULLSUM-DOT-BWD T=                 \ d-scale = full-reduce dot -> 1x1
2 BT-ROWS@ 1 ROWS-IS? TTRUE  2 BT-COLS@ 1 COLS-IS? TTRUE
0 BT-GRAD-NODE-RAW 1 T=  1 BT-GRAD-NODE-RAW 2 T=

\ ---- fail closed: no-adjoint (cast), empty IR, scale partial broadcast -------
' BT-TRY-CAST     E-BW-NOADJ     TTHROWS
' BT-TRY-EMPTY    E-BW-EMPTY     TTHROWS
' BT-TRY-SCALE-BC E-BW-BROADCAST TTHROWS

T-REPORT

;package

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

\ ---- fail-closed probes (MODEL: parses at runtime, so build the IR by hand) --
: BT-MK1 ( n -- )  {: op:n :}                            \ single op over one 2x2 input
   MIR-RESET  2 2 DT-F32 LAY-ROW MIR-INPUT+ drop
   op MIR-OP-BEGIN  0 MIR-IN-REF MIR-IN+  2 2 DT-F32 LAY-ROW 0 1 MIR-OP+ drop ;
: BT-MK-SCALE ( n n -- ) {: sr:n sc:n :}                 \ SCALE(x:2x3, s:sr x sc)
   MIR-RESET
   2 3 DT-F32 LAY-ROW MIR-INPUT+ drop                     \ x = 2x3 (slot 0)
   sr sc DT-F32 LAY-ROW MIR-INPUT+ drop                   \ s = sr x sc (slot 1)
   OP-SCALE MIR-OP-BEGIN  0 MIR-IN-REF MIR-IN+  1 MIR-IN-REF MIR-IN+
   2 3 DT-F32 LAY-ROW 0 1 MIR-OP+ drop ;
: BT-TRY-STATE ( -- )  BW-FWD-N@ drop ;                  \ accessor before build
: BT-TRY-EMPTY ( -- )  MIR-RESET BW-BUILD ;              \ empty IR
: BT-TRY-CAST  ( -- )  OP-CAST  BT-MK1 BW-BUILD ;           \ no adjoint (non-differentiable)
: BT-TRY-SCALE-BC ( -- )  1 3 BT-MK-SCALE BW-BUILD ;        \ s=1x3: partial broadcast (v1)

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
2 MIR-OP@ OP-GELU-BWD T=                       \ first appended backward node
3 MIR-OP@ OP-GELU-BWD T=
\ the seed cotangent lives in a fresh input slot for the output node (node 1)
BW-SEED-SLOT@ 1 T=                             \ slots: 0 = x, 1 = seed
\ the backward elementwise chain fuses into ONE region
FP-BUILD
2 FP-RID@ 3 FP-RID@ T=
\ input x receives a gradient (produced by the last backward node)
0 BW-SLOT-GRAD@ 3 T=

\ ---- fan-out: a value used twice sums its cotangents via OP-ADD -------------
\ n0 = gelu(i0) ; n1 = add(n0, n0) -> n0 used twice on the backward path.
MIR-RESET
0 0 DT-F32 LAY-ROW MIR-INPUT+ drop
OP-GELU MIR-OP-BEGIN  0 MIR-IN-REF MIR-IN+  0 0 DT-F32 LAY-ROW 0 1 MIR-OP+ drop
OP-ADD  MIR-OP-BEGIN  0 MIR-IN+ 0 MIR-IN+   0 0 DT-F32 LAY-ROW 0 1 MIR-OP+ drop
BW-BUILD
BW-BWD-COUNT 2 T=
2 MIR-OP@ OP-ADD      T=                       \ the fan-out summation node
3 MIR-OP@ OP-GELU-BWD T=
0 BW-SLOT-GRAD@ 3 T=                           \ x-grad = gelu-bwd(sum, x)

\ ---- add copies the cotangent to BOTH inputs (no new nodes) -----------------
MODEL: ADDM ( x:2x2 y:2x2 -- z ) ADD ;
BW-BUILD
BW-BWD-COUNT 0 T=                              \ pure copy: nothing emitted
\ both input grads are the seed cotangent (an input ref, slot 2)
0 BW-SLOT-GRAD@ 2 MIR-IN-REF T=
1 BW-SLOT-GRAD@ 2 MIR-IN-REF T=

\ ---- matmul: dX = ct @ Wt, dW = Xt @ ct (transpose + matmul nodes) ----------
MODEL: MM ( x:2x3 w:3x4 -- y ) MATMUL ;
BW-BUILD
BW-BWD-COUNT 4 T=
1 MIR-OP@ OP-TRANSPOSE T=                      \ Wt
2 MIR-OP@ OP-MATMUL    T=                      \ dX
3 MIR-OP@ OP-TRANSPOSE T=                      \ Xt
4 MIR-OP@ OP-MATMUL    T=                      \ dW
\ dX has X's shape (2x3), dW has W's shape (3x4)
0 BW-SLOT-GRAD@ 2 T=   2 MIR-ROWS@ 2 T=  2 MIR-COLS@ 3 T=
1 BW-SLOT-GRAD@ 4 T=   4 MIR-ROWS@ 3 T=  4 MIR-COLS@ 4 T=

\ ---- softmax adjoint reads the OUTPUT row (the forward node itself) ----------
MODEL: SM ( x:4x8 -- y ) SOFTMAX-ROW ;
BW-BUILD
BW-BWD-COUNT 1 T=
1 MIR-OP@ OP-SOFTMAX-ROW-BWD T=
1 1 MIR-IN@ 0 T=                               \ operand 1 = the saved output node 0

\ ---- reduction adjoints emit their dedicated backward op over (ct, saved input) --
MODEL: LN ( x:4x8 -- y ) LAYERNORM ;
BW-BUILD
BW-BWD-COUNT 1 T=
1 MIR-OP@ OP-LAYERNORM-BWD T=
1 1 MIR-IN@ 0 MIR-IN-REF T=                    \ operand 1 = the saved input x
MODEL: RMS ( x:4x8 -- y ) RMSNORM ;
BW-BUILD
1 MIR-OP@ OP-RMSNORM-BWD T=

\ ---- rope adjoint: a 3-operand backward op (cotangent, cos, sin) via BW-OP3 ------
MODEL: RP ( x:2x4 c:2x4 s:2x4 -- y ) ROPE ;
BW-BUILD
BW-BWD-COUNT 1 T=
1 MIR-OP@ OP-ROPE-BWD T=
1 MIR-IN-COUNT@ 3 T=                           \ cotangent + cos + sin
0 BW-SLOT-GRAD@ 1 T=                           \ only x receives a gradient (node 1)

\ ---- report rows: seed, node count, per-input gradient ----------------------
MODEL: R ( x:4x8 -- y ) GELU ;
BW-BUILD
RPT-NEW BW-INTO RPT-RENDER BT-SAVE
s" backward.seed: input 1" BT-IN
s" backward.nodes: fwd=1 bwd=1" BT-IN
s" backward.grad: input 0 <- node 1" BT-IN

\ ---- cad-9e: bias adjoint (dx = cotangent copy + d-bias = OP-ROWSUM-BWD) -----
MODEL: BIASM ( x:2x3 b:1x3 -- y ) BIAS ;
BW-BUILD
BW-BWD-COUNT 1 T=
1 MIR-OP@ OP-ROWSUM-BWD T=
1 MIR-ROWS@ 1 T=  1 MIR-COLS@ 3 T=              \ d-bias is 1x3
0 BW-SLOT-GRAD@ 2 MIR-IN-REF T=                 \ dx = the seed cotangent (input slot 2)
1 BW-SLOT-GRAD@ 1 T=                            \ d-bias = the row-reduce node

\ ---- cad-9e: linear adjoint (matmul adjoints + d-bias = OP-ROWSUM-BWD) -------
MODEL: LINM ( x:2x3 w:3x4 b:1x4 -- y ) LINEAR ;
BW-BUILD
BW-BWD-COUNT 5 T=                               \ 2 transpose + 2 matmul + 1 rowsum
0 BW-SLOT-GRAD@ MIR-ROWS@ 2 T=  0 BW-SLOT-GRAD@ MIR-COLS@ 3 T=   \ d-x 2x3
1 BW-SLOT-GRAD@ MIR-ROWS@ 3 T=  1 BW-SLOT-GRAD@ MIR-COLS@ 4 T=   \ d-w 3x4
2 BW-SLOT-GRAD@ MIR-OP@ OP-ROWSUM-BWD T=
2 BW-SLOT-GRAD@ MIR-ROWS@ 1 T=  2 BW-SLOT-GRAD@ MIR-COLS@ 4 T=   \ d-b 1x4

\ ---- cad-9e: slice adjoint (OP-PAD-SCATTER at the forward slice offset) ------
MODEL: SLM ( x:4x4 -- y ) SLICE:1..3 ;
BW-CAN? TTRUE
BW-BUILD
BW-BWD-COUNT 1 T=
1 MIR-OP@ OP-PAD-SCATTER T=
1 MIR-ROWS@ 4 T=  1 MIR-COLS@ 4 T=              \ padded back to the input shape
1 MIR-ATTR@ MV-PA@ 1 T=  1 MIR-ATTR@ MV-PB@ 3 T=   \ r0=1, r1=3 from the forward slice
0 BW-SLOT-GRAD@ 1 T=

\ ---- cad-9e: gather adjoint (OP-SCATTER-ADD at the gathered indices) ---------
MODEL: GAM ( x:4x2 idx:3x1 -- y ) GATHER ;
BW-BUILD
BW-BWD-COUNT 1 T=
1 MIR-OP@ OP-SCATTER-ADD T=
1 MIR-ROWS@ 4 T=  1 MIR-COLS@ 2 T=              \ scattered back to the input shape
1 MIR-IN-COUNT@ 2 T=                            \ cotangent + index operand
1 1 MIR-IN@ 1 MIR-IN-REF T=                     \ operand 1 = the index input (slot 1)
0 BW-SLOT-GRAD@ 1 T=                            \ x receives the scatter-add node
1 BW-HAS-GRAD? TFALSE                           \ the index operand gets no gradient

\ ---- cad-9e: scale adjoint, same-shape operand (elementwise product rule) ----
MODEL: SCM ( x:2x3 s:2x3 -- z ) SCALE ;
BW-BUILD
BW-BWD-COUNT 2 T=
1 MIR-OP@ OP-MUL T=  2 MIR-OP@ OP-MUL T=
0 BW-HAS-GRAD? TTRUE  1 BW-HAS-GRAD? TTRUE

\ ---- cad-9e: scale adjoint, 1x1 scalar operand (broadcast-scale + full-reduce dot) --
MODEL: SCS ( x:2x3 s:1x1 -- z ) SCALE ;
BW-BUILD
BW-BWD-COUNT 2 T=
1 MIR-OP@ OP-SCALE T=                           \ dx = broadcast scale(ct, s)
2 MIR-OP@ OP-FULLSUM-DOT-BWD T=                 \ d-scale = full-reduce dot -> 1x1
2 MIR-ROWS@ 1 T=  2 MIR-COLS@ 1 T=
0 BW-SLOT-GRAD@ 1 T=  1 BW-SLOT-GRAD@ 2 T=

\ ---- fail closed: no-adjoint (cast), empty IR, scale partial broadcast -------
' BT-TRY-CAST     E-BW-NOADJ     TTHROWS
' BT-TRY-EMPTY    E-BW-EMPTY     TTHROWS
' BT-TRY-SCALE-BC E-BW-BROADCAST TTHROWS

T-REPORT

end-package

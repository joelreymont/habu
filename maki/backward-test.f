\ maki/backward-test.f - checked tests for the model-IR reverse transform (cad-9b/9e).
\ Backward nodes appended as ordinary IR: elementwise *-bwd emission, fan-out
\ cotangent summation via OP-ADD, matmul transpose+matmul adjoints, add copy-through,
\ the seeded output cotangent, per-input gradient reporting, that a backward
\ elementwise chain fuses under FP-BUILD, and every fail-closed path. cad-9e adds the
\ reduce/scatter adjoint emission (bias/linear/slice/gather/scale) with shape asserts
\ and the scale partial-broadcast fail-closed boundary. The concat-backward integration
\ model pins the split cotangent's EXACT row windows (both BW-SL range args are
\ CAD-KIND:rows, so an r0/r1 transposition is checker-invisible) and gradchecks
\ numerically through the concat boundary.

require lib/test.f
require lib/string.f
require maki/cad.f
require maki/backward.f
require maki/fusion-plan.f
require maki/gradcheck.f

package MAKI

\ ---- render containment helper (report rows) -------------------------------
variable BT-VA  variable BT-VU
: BT-SAVE ( ptr u8 n -- )  BT-VU ! BT-VA ! ;
: BT-IN ( ptr u8 n -- )  BT-VA @ BT-VU @ 2swap CONTAINS? TTRUE ;

\ ---- typed assert helpers (ids/refs are nominal; compare via the identities) --
: BT-OP= ( n n -- )  swap MIR-NODE-ID MIR-OP@ OPKIND>N swap T= ;   \ node raw, expected OP-*
: BT-GRAD-NODE= ( n n -- )                     \ slot raw, expected node raw
   swap MIR-SLOT-ID BW-SLOT-GRAD@ swap MIR-NODE-ID MIR-NODE-REF MIR-REF= TTRUE ;
: BT-GRAD-SLOT= ( n n -- )                     \ slot raw, expected slot raw
   swap MIR-SLOT-ID BW-SLOT-GRAD@ swap MIR-SLOT-ID MIR-IN-REF MIR-REF= TTRUE ;
: BT-GRAD-REF ( n -- MIR:operand-ref )  MIR-SLOT-ID BW-SLOT-GRAD@ ;
: BT-IN-NODE= ( n n n -- )                     \ node k expected-node-raw
   {: nd:n k:n xnd:n :}
   nd MIR-NODE-ID k MIR-INPUT-IDX MIR-IN@ xnd MIR-NODE-ID MIR-NODE-REF MIR-REF= TTRUE ;
: BT-IN-SLOT= ( n n n -- )                     \ node k expected-slot-raw
   {: nd:n k:n xs:n :}
   nd MIR-NODE-ID k MIR-INPUT-IDX MIR-IN@ xs MIR-SLOT-ID MIR-IN-REF MIR-REF= TTRUE ;
: BT-ROWS= ( n n -- )  swap MIR-NODE-ID MIR-ROWS@ ROWS-RAW swap T= ;
: BT-COLS= ( n n -- )  swap MIR-NODE-ID MIR-COLS@ COLS-RAW swap T= ;

\ ---- fail-closed probes (MODEL: parses at runtime, so build the IR by hand) --
: BT-MK1 ( opkind -- )                                  \ single op over one 2x2 input; op rides the stack
   MIR-RESET  2 2 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop
   MIR-OP-BEGIN  0 MIR-SLOT-ID MIR-IN-REF MIR-IN+
   2 2 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ drop ;
: BT-MK-SCALE ( n n -- ) {: sr:n sc:n :}                 \ SCALE(x:2x3, s:sr x sc)
   MIR-RESET
   2 3 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop               \ x = 2x3 (slot 0)
   sr sc SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop             \ s = sr x sc (slot 1)
   MAKI-OPKIND:SCALE MIR-OP-BEGIN  0 MIR-SLOT-ID MIR-IN-REF MIR-IN+  1 MIR-SLOT-ID MIR-IN-REF MIR-IN+
   2 3 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ drop ;
: BT-TRY-STATE ( -- )  BW-FWD-N@ drop ;                  \ accessor before build
: BT-TRY-EMPTY ( -- )  MIR-RESET BW-BUILD ;              \ empty IR
: BT-TRY-CAST  ( -- )  MAKI-OPKIND:CAST  BT-MK1 BW-BUILD ;           \ no adjoint (non-differentiable)
: BT-TRY-SCALE-BC ( -- )  1 3 BT-MK-SCALE BW-BUILD ;        \ s=1x3: partial broadcast (v1)
: BT-TRY-2ND ( -- )  BW-BUILD ;                          \ 2nd build over the current IR

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
2 OP-GELU-BWD BT-OP=                       \ first appended backward node
3 OP-GELU-BWD BT-OP=
\ the seed cotangent lives in a fresh input slot for the output node (node 1)
BW-SEED-SLOT@ 1 MIR-SLOT-ID MIR-SLOT= TTRUE                             \ slots: 0 = x, 1 = seed
\ the backward elementwise chain fuses into ONE region
FP-BUILD
2 MIR-NODE-ID FP-RID@ 3 MIR-NODE-ID FP-RID@ FP-RGN= TTRUE
\ input x receives a gradient (produced by the last backward node)
0 3 BT-GRAD-NODE=

\ ---- fan-out: a value used twice sums its cotangents via OP-ADD -------------
\ n0 = gelu(i0) ; n1 = add(n0, n0) -> n0 used twice on the backward path.
MIR-RESET
0 0 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop
MAKI-OPKIND:GELU MIR-OP-BEGIN  0 MIR-SLOT-ID MIR-IN-REF MIR-IN+
0 0 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ drop
MAKI-OPKIND:ADD MIR-OP-BEGIN
0 MIR-NODE-ID MIR-NODE-REF MIR-IN+ 0 MIR-NODE-ID MIR-NODE-REF MIR-IN+
0 0 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ drop
BW-BUILD
BW-BWD-COUNT 2 T=
2 OP-ADD BT-OP=                       \ the fan-out summation node
3 OP-GELU-BWD BT-OP=
0 3 BT-GRAD-NODE=                           \ x-grad = gelu-bwd(sum, x)

\ ---- add copies the cotangent to BOTH inputs (no new nodes) -----------------
MODEL: ADDM ( x:2x2 y:2x2 -- z ) ADD ;
BW-BUILD
BW-BWD-COUNT 0 T=                              \ pure copy: nothing emitted
\ both input grads are the seed cotangent (an input ref, slot 2)
0 2 BT-GRAD-SLOT=
1 2 BT-GRAD-SLOT=

\ ---- matmul: dX = ct @ Wt, dW = Xt @ ct (transpose + matmul nodes) ----------
MODEL: MM ( x:2x3 w:3x4 -- y ) MATMUL ;
BW-BUILD
BW-BWD-COUNT 4 T=
1 OP-TRANSPOSE BT-OP=                      \ Wt
2 OP-MATMUL BT-OP=                      \ dX
3 OP-TRANSPOSE BT-OP=                      \ Xt
4 OP-MATMUL BT-OP=                      \ dW
\ dX has X's shape (2x3), dW has W's shape (3x4)
0 2 BT-GRAD-NODE=   2 2 BT-ROWS=  2 3 BT-COLS=
1 4 BT-GRAD-NODE=   4 3 BT-ROWS=  4 4 BT-COLS=

\ ---- softmax adjoint reads the OUTPUT row (the forward node itself) ----------
MODEL: SM ( x:4x8 -- y ) SOFTMAX-ROW ;
BW-BUILD
BW-BWD-COUNT 1 T=
1 OP-SOFTMAX-ROW-BWD BT-OP=
1 1 0 BT-IN-NODE=                               \ operand 1 = the saved output node 0

\ ---- reduction adjoints emit their dedicated backward op over (ct, saved input) --
MODEL: LN ( x:4x8 -- y ) LAYERNORM ;
BW-BUILD
BW-BWD-COUNT 1 T=
1 OP-LAYERNORM-BWD BT-OP=
1 1 0 BT-IN-SLOT=                    \ operand 1 = the saved input x
MODEL: RMS ( x:4x8 -- y ) RMSNORM ;
BW-BUILD
1 OP-RMSNORM-BWD BT-OP=

\ ---- rope adjoint: a 3-operand backward op (cotangent, cos, sin) via BW-OP3 ------
MODEL: RP ( x:2x4 c:2x4 s:2x4 -- y ) ROPE ;
BW-BUILD
BW-BWD-COUNT 1 T=
1 OP-ROPE-BWD BT-OP=
1 MIR-NODE-ID MIR-IN-COUNT@ 3 T=                           \ cotangent + cos + sin
0 1 BT-GRAD-NODE=                           \ only x receives a gradient (node 1)

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
1 OP-ROWSUM-BWD BT-OP=
1 1 BT-ROWS=  1 3 BT-COLS=              \ d-bias is 1x3
0 2 BT-GRAD-SLOT=                 \ dx = the seed cotangent (input slot 2)
1 1 BT-GRAD-NODE=                            \ d-bias = the row-reduce node

\ ---- cad-9e: linear adjoint (matmul adjoints + d-bias = OP-ROWSUM-BWD) -------
MODEL: LINM ( x:2x3 w:3x4 b:1x4 -- y ) LINEAR ;
BW-BUILD
BW-BWD-COUNT 5 T=                               \ 2 transpose + 2 matmul + 1 rowsum
0 BT-GRAD-REF MIR-REF-NODE MIR-ROWS@ ROWS-RAW 2 T=  0 BT-GRAD-REF MIR-REF-NODE MIR-COLS@ COLS-RAW 3 T=   \ d-x 2x3
1 BT-GRAD-REF MIR-REF-NODE MIR-ROWS@ ROWS-RAW 3 T=  1 BT-GRAD-REF MIR-REF-NODE MIR-COLS@ COLS-RAW 4 T=   \ d-w 3x4
2 BT-GRAD-REF MIR-REF-NODE MIR-OP@ OPKIND>N OP-ROWSUM-BWD T=
2 BT-GRAD-REF MIR-REF-NODE MIR-ROWS@ ROWS-RAW 1 T=  2 BT-GRAD-REF MIR-REF-NODE MIR-COLS@ COLS-RAW 4 T=   \ d-b 1x4

\ ---- cad-9e: slice adjoint (OP-PAD-SCATTER at the forward slice offset) ------
MODEL: SLM ( x:4x4 -- y ) SLICE:1..3 ;
BW-CAN? TTRUE
BW-BUILD
BW-BWD-COUNT 1 T=
1 OP-PAD-SCATTER BT-OP=
1 4 BT-ROWS=  1 4 BT-COLS=              \ padded back to the input shape
1 MIR-NODE-ID MIR-ATTR@ MV-PA@ 1 T=  1 MIR-ATTR@ MV-PB@ 3 T=   \ r0=1, r1=3 from the forward slice
0 1 BT-GRAD-NODE=

\ ---- cad-9e: gather adjoint (OP-SCATTER-ADD at the gathered indices) ---------
MODEL: GAM ( x:4x2 idx:3x1 -- y ) GATHER ;
BW-BUILD
BW-BWD-COUNT 1 T=
1 OP-SCATTER-ADD BT-OP=
1 4 BT-ROWS=  1 2 BT-COLS=              \ scattered back to the input shape
1 MIR-NODE-ID MIR-IN-COUNT@ 2 T=                            \ cotangent + index operand
1 1 1 BT-IN-SLOT=                     \ operand 1 = the index input (slot 1)
0 1 BT-GRAD-NODE=                            \ x receives the scatter-add node
1 MIR-SLOT-ID BW-HAS-GRAD? TFALSE                           \ the index operand gets no gradient

\ ---- cad-9e: scale adjoint, same-shape operand (elementwise product rule) ----
MODEL: SCM ( x:2x3 s:2x3 -- z ) SCALE ;
BW-BUILD
BW-BWD-COUNT 2 T=
1 OP-MUL BT-OP=  2 OP-MUL BT-OP=
0 MIR-SLOT-ID BW-HAS-GRAD? TTRUE  1 BW-HAS-GRAD? TTRUE

\ ---- cad-9e: scale adjoint, 1x1 scalar operand (broadcast-scale + full-reduce dot) --
MODEL: SCS ( x:2x3 s:1x1 -- z ) SCALE ;
BW-BUILD
BW-BWD-COUNT 2 T=
1 OP-SCALE BT-OP=                           \ dx = broadcast scale(ct, s)
2 OP-FULLSUM-DOT-BWD BT-OP=                 \ d-scale = full-reduce dot -> 1x1
2 1 BT-ROWS=  2 1 BT-COLS=
0 1 BT-GRAD-NODE=  1 2 BT-GRAD-NODE=

\ ---- concat-backward integration: the cotangent splits at the forward row seam ----
\ CONCAT(a:2x4, b:3x4) -> 5x4 feeds GELU. The concat adjoint slices the upstream
\ cotangent at the row split: d-a = ct[0,2), d-b = ct[2,5). Both BW-SL row-range
\ args are CAD-KIND:rows, so a transposed r0/r1 passes the checker - the attr pins
\ below are the only guard. GC-RUN (throwaway build, rolled back) FD-verifies the
\ gradient numerically through the concat boundary before the structural build.
MODEL: CCM ( a:2x4 b:3x4 -- y ) CONCAT GELU ;
BW-CAN? TTRUE
GC-RUN V-PASS T=
BW-BUILD
BW-FWD-N@ 2 T=                                 \ concat + gelu
BW-BWD-COUNT 3 T=                              \ gelu-bwd + two cotangent slices
BW-SEED-SLOT@ 2 MIR-SLOT-ID MIR-SLOT= TTRUE    \ slots: 0 = a, 1 = b, 2 = seed
2 OP-GELU-BWD BT-OP=
3 OP-SLICE BT-OP=                              \ d-a slice
4 OP-SLICE BT-OP=                              \ d-b slice
3 0 2 BT-IN-NODE=  4 0 2 BT-IN-NODE=           \ both slices read the gelu-bwd cotangent
3 2 BT-ROWS=  3 4 BT-COLS=                     \ d-a is 2x4
4 3 BT-ROWS=  4 4 BT-COLS=                     \ d-b is 3x4
\ EXACT row windows (fails if the two BW-SL ranges were transposed)
3 MIR-NODE-ID MIR-ATTR@ MV-PA@ 0 T=  3 MIR-NODE-ID MIR-ATTR@ MV-PB@ 2 T=   \ d-a [0,2)
4 MIR-NODE-ID MIR-ATTR@ MV-PA@ 2 T=  4 MIR-NODE-ID MIR-ATTR@ MV-PB@ 5 T=   \ d-b [2,5)
0 3 BT-GRAD-NODE=                              \ d-a <- slice node 3
1 4 BT-GRAD-NODE=                              \ d-b <- slice node 4

\ ---- SECOND ORDER (grad-of-grad pilot): a 2nd BW-BUILD over the combined IR --
\ After build 1 over GELU: node 0 = gelu(x), node 1 = gelu-bwd(s, x) = dL/dx (the
\ LAST node; slots 0 = x, 1 = s = build-1 seed). Build 2 seeds v (slot 2) on node 1
\ and differentiates it: d-s = gelu-bwd(v, x); d-x = gelu-bwd2(mul(v, s), x).
MODEL: GG ( x:2x2 -- y ) GELU ;
BW-BUILD
BW-BUILD                                       \ 2nd build: gelu-bwd's adjoint row emits
BW-FWD-N@ 2 T=  BW-BWD-COUNT 3 T=
BW-SEED-SLOT@ 2 MIR-SLOT-ID MIR-SLOT= TTRUE
2 OP-GELU-BWD BT-OP=             \ d-s reuses the first derivative
3 OP-MUL BT-OP=             \ v*s
4 OP-GELU-BWD2 BT-OP=             \ d-x needs gelu''
1 2 BT-GRAD-NODE=                           \ d-s <- gelu-bwd(v, x)
0 4 BT-GRAD-NODE=                           \ d-x <- gelu-bwd2(v*s, x)
\ operand wiring: d-s reads (v, x); mul reads (v, s); gelu-bwd2 reads (v*s, x)
2 0 2 BT-IN-SLOT=   2 1 0 BT-IN-SLOT=
3 0 2 BT-IN-SLOT=   3 1 1 BT-IN-SLOT=
4 0 3 BT-IN-NODE=              4 1 0 BT-IN-SLOT=

\ ---- fail closed: any OTHER *-BWD kind still rejects the 2nd build -----------
MODEL: SGG ( x:2x2 -- y ) SILU ;
BW-BUILD
' BT-TRY-2ND E-BW-NOADJ TTHROWS                \ silu-bwd has no adjoint row yet

\ ---- fail closed: THIRD order stays closed (gelu-bwd2 has no adjoint row) ----
MODEL: GG3 ( x:2x2 -- y ) GELU ;
BW-BUILD
BW-BUILD                                       \ 2nd build succeeds (pilot above)
' BT-TRY-2ND E-BW-NOADJ TTHROWS                \ 3rd build: gelu-bwd2 is ADJ-NONE

\ ---- fail closed: no-adjoint (cast), empty IR, scale partial broadcast -------
' BT-TRY-CAST     E-BW-NOADJ     TTHROWS
' BT-TRY-EMPTY    E-BW-EMPTY     TTHROWS
' BT-TRY-SCALE-BC E-BW-BROADCAST TTHROWS

T-REPORT

end-package

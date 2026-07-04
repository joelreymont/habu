\ maki/backward-test.f - checked tests for the model-IR reverse transform (cad-9b).
\ Backward nodes appended as ordinary IR: elementwise *-bwd emission, fan-out
\ cotangent summation via OP-ADD, matmul transpose+matmul adjoints, add copy-through,
\ the seeded output cotangent, per-input gradient reporting, that a backward
\ elementwise chain fuses under FP-BUILD, and every fail-closed path.

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
: BT-TRY-STATE ( -- )  BW-FWD-N@ drop ;                  \ accessor before build
: BT-TRY-EMPTY ( -- )  MIR-RESET BW-BUILD ;              \ empty IR
: BT-TRY-SLICE ( -- )  OP-SLICE BT-MK1 BW-BUILD ;           \ unsupported adjoint
: BT-TRY-CAST  ( -- )  OP-CAST  BT-MK1 BW-BUILD ;           \ no adjoint

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

\ ---- fail closed: unsupported / no-adjoint / empty --------------------------
' BT-TRY-SLICE E-BW-UNSUP TTHROWS
' BT-TRY-CAST  E-BW-NOADJ TTHROWS
' BT-TRY-EMPTY E-BW-EMPTY TTHROWS
\ BW-CAN? classifies the slice model as not-transformable, first bad op named
MODEL: SLM2 ( x:4x4 -- y ) SLICE:0..2 ;
BW-CAN? TFALSE
BW-FIRST-BAD OP-SLICE T=

T-REPORT

end-package

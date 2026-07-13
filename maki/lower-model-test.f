\ maki/lower-model-test.f - host-side checked tests for the whole-model device leg (slice 5).
\
\ Off-device: builds multi-region models and asserts the cross-region STAGING + tolerance
\ composition the whole-model run (maki/lower-launch.f LOWER-MODEL-RUN) and its golden
\ (maki/lower-golden.f LOWER-MODEL-GOLDEN) rest on, without a GPU. The flagship model is the
\ FFN-class chain LINEAR GELU LINEAR RMSNORM: three regions (matmul+gelu epilogue, matmul,
\ row-reduce) where region 1 reads region 0's materialized output and region 2 reads region 1's,
\ so the class analyzers must accept a cross-region PRODUCER NODE as a region input (not a slot).
\ A cross-region MOVEMENT model (GELU CONCAT) exercises the materialized copy reading a producer
\ node. Fail-closed: a non-host-executable op and a region the device leg cannot lower both drop
\ MDL-LOWERABLE? to false. Off-device LOWER-MODEL-GOLDEN is an honest not-run. The kernel PTX and
\ device numerics are proven on the Orin by maki/lower-model-device-test.f. Load via maki/test.f.

require lib/test.f
require lib/string.f
require test/checker-assert.f
require maki/cad.f
require maki/lower-golden.f
require maki/lower-mm.f
require maki/lower-red.f
require maki/lower-move.f

package MAKI

\ off-device the whole-model golden is an honest not-run; on-device the real run needs registered
\ cubins (the device test's job), so that assertion is skipped there. (Wrapped in a word because
\ if/then is compile-only - not usable at the interpreter top level.)
: LMT-OFFDEV-NOTRUN ( -- )
   CUDA:OPEN? 0= if  LOWER-MODEL-GOLDEN V-NOTRUN T=  then ;

\ cols wider than the launch block: the runtime row-launch check behind the
\ typed seam (LLA-ROW-GRID) must still fail closed (E-PTX-BLOCK, defense in depth)
: LMT-ROWGRID-WIDE ( -- )  1 512 SHAPE LLA-ROW-GRID drop ;

T-RESET

\ ==== the multi-region FFN-class model: LINEAR GELU LINEAR RMSNORM ============================
\ region 0 = LINEAR + GELU epilogue (matmul); region 1 = LINEAR (matmul boundary); region 2 =
\ RMSNORM (row-reduce barrier). n1 (gelu) and n2 (linear2) are materialized region outputs.
MODEL: MFFN ( x:4x8 w1:8x16 b1:1x16 w2:16x8 b2:1x8 -- y ) LINEAR GELU LINEAR RMSNORM ;
MODEL-K 4 T=
FP-BUILD
FP-REGION-COUNT 3 T=
MIR-MAT-COUNT   3 T=

\ region class routing (the whole-model dispatch reuses these)
0 FP-REGION-ID LLA-REGION-MATMUL? TTRUE
1 FP-REGION-ID LLA-REGION-MATMUL? TTRUE
2 FP-REGION-ID LLA-REGION-REDUCE? TTRUE
2 FP-REGION-ID LLA-REGION-MATMUL? TFALSE
0 FP-REGION-ID LLA-REGION-MOVE?   TFALSE

\ cross-region operand resolution: region 1's contraction A operand is the gelu node (n1), and
\ region 2's reduction input is the linear node (n2) - both cross-region producers, not slots.
1 FP-REGION-ID LMM-ANALYZE
0 LMM-IN-REF@ 1 T=                       \ operand 0 = node 1 (gelu, cross-region producer)
0 LMM-IN-REF@ MIR-REF-INPUT? TFALSE
1 LMM-IN-REF@ MIR-REF-INPUT? TTRUE        \ operand 1 = the w2 input slot
2 FP-REGION-ID LRED-ANALYZE
0 LRED-IN-REF@ 2 T=                       \ operand 0 = node 2 (linear2, cross-region producer)
0 LRED-IN-REF@ MIR-REF-INPUT? TFALSE

\ typed row-launch seam (LLA-ROW-GRID): grid=rows / p_k=cols is static; byte-identity
\ on the analyzed rmsnorm region (4x8): grid = 4 rows, u32 param = 8 cols.
LLA-GRID-RED 4 T=                         \ grid = region rows
LLA-NVAR @ 8 T=                           \ u32 kernel param = region cols
512 1 SHAPE LLA-ROW-GRID 512 T=           \ rows drive the grid...
LLA-NVAR @ 1 T=                           \ ...cols the param, not vice versa
' LMT-ROWGRID-WIDE E-PTX-BLOCK TTHROWS    \ runtime row-launch check stays reachable
\ swapped roles into the typed seam reject BEFORE runtime (same arity)
s" LMT-RG-OK       ( CAD-KIND:rows CAD-KIND:cols -- n ) LLA-ROW-GRID" CHECK-QUIET-CANDIDATE! -1 T=
s" LMT-NEG-RG-SWAP ( CAD-KIND:cols CAD-KIND:rows -- n ) LLA-ROW-GRID" CHECK-QUIET-CANDIDATE! 0 T=
s" LMT-NEG-RG-RAW  ( n n -- n ) LLA-ROW-GRID"                         CHECK-QUIET-CANDIDATE! 0 T=

\ typed matmul-param seam (LLA-SET-MNK!): p_m/p_n/p_k order is static; byte-identity
\ on the real staged GEMM region 1 (C(4x8) = A(4x16) . B(16x8) + b(1x8)).
1 FP-REGION-ID LLA-STAGE-MM
LLA-PM @ 4  T=                            \ p_m = M = rows(C)
LLA-PN @ 8  T=                            \ p_n = N = cols(C)
LLA-PK @ 16 T=                            \ p_k = K = cols(A)
LLA-PM @ LMM-M@ T=  LLA-PN @ LMM-N@ T=  LLA-PK @ LMM-K@ T=
\ ANY transposition of the (M, N, K) triple into the typed seam rejects BEFORE
\ runtime; K is a bare n by design, and a role never unifies with it either way
s" LMT-MNK-OK       ( CAD-KIND:rows CAD-KIND:cols n -- ) LLA-SET-MNK!" CHECK-QUIET-CANDIDATE! -1 T=
s" LMT-NEG-MNK-SWAP ( CAD-KIND:cols CAD-KIND:rows n -- ) LLA-SET-MNK!" CHECK-QUIET-CANDIDATE! 0 T=
s" LMT-NEG-MNK-MK   ( n CAD-KIND:cols CAD-KIND:rows -- ) LLA-SET-MNK!" CHECK-QUIET-CANDIDATE! 0 T=
s" LMT-NEG-MNK-NK   ( CAD-KIND:rows n CAD-KIND:cols -- ) LLA-SET-MNK!" CHECK-QUIET-CANDIDATE! 0 T=
s" LMT-NEG-MNK-RAW  ( n n n -- ) LLA-SET-MNK!"                         CHECK-QUIET-CANDIDATE! 0 T=

\ tolerance composition inputs: 2 matmul regions + 1 row-reduce region, summed across the chain
MDL-COUNT-REGIONS
MDL-N-REGIONS@ 3 T=
MDL-N-MM@  2 T=
MDL-N-RED@ 1 T=
MDL-N-EW@  0 T=
MDL-N-MV@  0 T=

\ the whole model is device-lowerable; off-device the golden is an honest not-run
MDL-LOWERABLE? TTRUE
LMT-OFFDEV-NOTRUN

\ ==== cross-region MOVEMENT: GELU CONCAT (region 0 EW, region 1 materialized copy) =============
\ concat's A operand is the interior gelu node; the copy region binds gelu's device buffer.
MODEL: MGC ( x:4x8 b:4x8 -- y ) GELU CONCAT ;
FP-BUILD
FP-REGION-COUNT 2 T=
0 FP-REGION-ID LLA-REGION-MATMUL? TFALSE
0 FP-REGION-ID LLA-REGION-REDUCE? TFALSE
1 FP-REGION-ID LLA-REGION-MOVE?   TTRUE   \ region 1 = a materialized movement copy
MDL-COUNT-REGIONS
MDL-N-EW@ 1 T=                            \ the gelu region
MDL-N-MV@ 1 T=                            \ the copy region
MDL-N-REGIONS@ 2 T=
MDL-LOWERABLE? TTRUE

\ ==== fail-closed: a non-host-executable op, and a region the device leg cannot lower ==========
\ CAST has no host reference, so the whole-model golden cannot compare against the executor.
MODEL: MCAST ( x:2x2 -- y ) CAST ;
FP-BUILD
MDL-LOWERABLE? TFALSE

\ RESIDUAL-ADD is a BINARY epilogue after the contraction; the matmul kernel fuses only unary
\ activations, so region 0 fails LMM analysis and the model is not device-lowerable (falls back
\ to the host golden leg on-device, rather than throwing).
MODEL: MBIN ( x:4x8 w:8x8 r:4x8 -- y ) MATMUL RESIDUAL-ADD ;
FP-BUILD
MDL-LOWERABLE? TFALSE

T-REPORT

;package

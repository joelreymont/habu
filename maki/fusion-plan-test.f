\ maki/fusion-plan-test.f - checked tests for the cad-2 fusion region planner.
\ Region growth over captured + hand-built model IR: elementwise chains, matmul
\ prologue/epilogue, one-contraction-per-region, the two-row-reduce softmax budget,
\ movement dissolution vs materialization, multi-use materialize, the typed split
\ rows, materialization flags, and every fail-closed accessor path.

require lib/test.f
require lib/string.f
require maki/cad.f
require maki/fusion-plan.f

package MAKI

\ ---- render containment helper (report split rows) -------------------------
variable FT-VA  variable FT-VU
: FT-SAVE ( ptr u8 n -- )  FT-VU ! FT-VA ! ;
: FT-IN ( ptr u8 n -- )  FT-VA @ FT-VU @ 2swap CONTAINS? TTRUE ;

\ ---- fail-closed probes ----------------------------------------------------
: TRY-STATE  ( -- )  FP-RESET FP-REGION-COUNT drop ;      \ accessor before build
: TRY-RID    ( -- )  99 FP-RID@ drop ;                    \ node index out of range
: TRY-RGN    ( -- )  99 FP-REGION-MEMBERS drop ;          \ region index out of range
: TRY-REASON ( -- )  99 FP-REASON-NAME 2drop ;            \ bad reason tag

T-RESET

\ ---- accessor before FP-BUILD fails closed ---------------------------------
' TRY-STATE E-FP-STATE TTHROWS

\ ---- pure elementwise chain: GELU SILU RELU -> one region of three ----------
MODEL: CHAIN ( x:4x8 -- y ) GELU SILU RELU ;
FP-BUILD
FP-REGION-COUNT      1 T=
0 FP-REGION-MEMBERS  3 T=
FP-SPLIT-COUNT       0 T=
0 FP-RID@ 0 T=  1 FP-RID@ 0 T=  2 FP-RID@ 0 T=
\ interior nodes cleared; only the model output materializes
0 MIR-MAT@ TFALSE  1 MIR-MAT@ TFALSE  2 MIR-MAT@ TTRUE

\ ---- FFN LINEAR GELU LINEAR: gelu fuses as matmul epilogue; second matmul ----
\ splits (one contraction per region) -> 2 regions, matmul-boundary at node 2.
MODEL: FFN ( x:2x3 w1:3x4 b1:1x4 w2:4x5 b2:1x5 -- y ) LINEAR GELU LINEAR ;
FP-BUILD
FP-REGION-COUNT 2 T=
0 FP-RID@ 0 T=  1 FP-RID@ 0 T=  2 FP-RID@ 1 T=
FP-SPLIT-COUNT 1 T=
0 FP-SPLIT-NODE@   2 T=
0 FP-SPLIT-REASON@ SR-MATMUL T=
\ node0 (first linear) is interior; node1 (region output) + node2 (model out) set
0 MIR-MAT@ TFALSE  1 MIR-MAT@ TTRUE  2 MIR-MAT@ TTRUE
\ report row wording
RPT-NEW FP-REPORT+ RPT-RENDER FT-SAVE
s" matmul-boundary at node 2" FT-IN

\ ---- MIX GELU SILU MATMUL RELU: prologue (gelu,silu) + matmul + epilogue relu -
\ all fold into one region (a single contraction) -> no split.
MODEL: MIX ( x:2x2 w:2x2 -- y ) GELU SILU MATMUL RELU ;
FP-BUILD
FP-REGION-COUNT     1 T=
0 FP-REGION-MEMBERS 4 T=
FP-SPLIT-COUNT      0 T=

\ ---- a free reshape dissolves and does NOT break the chain ------------------
\ GELU RESHAPE:8x4 RELU: reshape is MVV-FREE -> one region, reshape not materialized.
MODEL: MVB ( x:4x8 -- y ) GELU RESHAPE:8x4 RELU ;
FP-BUILD
FP-REGION-COUNT 1 T=
FP-SPLIT-COUNT  0 T=
1 MIR-MAT@ TFALSE                         \ the free reshape stays dissolved

\ ---- matmul -> matmul splits (matmul-boundary) -----------------------------
MODEL: MM2 ( x:2x3 w1:3x4 w2:4x5 -- y ) MATMUL MATMUL ;
FP-BUILD
FP-REGION-COUNT    2 T=
FP-SPLIT-COUNT     1 T=
0 FP-SPLIT-REASON@ SR-MATMUL T=
0 FP-SPLIT-NODE@   1 T=

\ ---- a materialize-verdict movement node splits (layout-conflict) ----------
\ GELU CONCAT: concat is MVV-MATERIALIZE, so it cannot dissolve into gelu's region.
MODEL: MMAT ( x:2x4 b:2x4 -- y ) GELU CONCAT ;
FP-BUILD
FP-REGION-COUNT    2 T=
FP-SPLIT-COUNT     1 T=
0 FP-SPLIT-REASON@ SR-LAYOUT T=
0 FP-SPLIT-NODE@   1 T=

\ ---- two same-row reductions fuse (softmax max+sum budget) ------------------
MODEL: RR2 ( x:4x8 -- y ) LAYERNORM SOFTMAX-ROW ;
FP-BUILD
FP-REGION-COUNT 1 T=
FP-SPLIT-COUNT  0 T=

\ ---- a third row-reduction exhausts the budget -> barrier-boundary ----------
MODEL: RR3 ( x:4x8 -- y ) LAYERNORM SOFTMAX-ROW RMSNORM ;
FP-BUILD
FP-REGION-COUNT    2 T=
FP-SPLIT-COUNT     1 T=
0 FP-SPLIT-REASON@ SR-BARRIER T=
0 FP-SPLIT-NODE@   2 T=

\ ---- multi-use producer materializes + splits (hand-built IR) --------------
\ node0 = GELU(i0) ; node1 = ADD(n0, n0) -> n0 used twice -> not single-use.
MIR-RESET
0 0 DT-F32 LAY-ROW MIR-INPUT+ drop
OP-GELU MIR-OP-BEGIN  0 MIR-IN-REF MIR-IN+  0 0 DT-F32 LAY-ROW 0 1 MIR-OP+ drop
OP-ADD  MIR-OP-BEGIN  0 MIR-IN+ 0 MIR-IN+  0 0 DT-F32 LAY-ROW 0 1 MIR-OP+ drop
FP-BUILD
FP-REGION-COUNT    2 T=
FP-SPLIT-COUNT     1 T=
0 FP-SPLIT-REASON@ SR-MULTI-USE T=
0 FP-SPLIT-NODE@   0 T=                    \ reported at the materialized producer
0 MIR-MAT@ TTRUE                            \ multi-use producer is materialized

\ ---- fail-closed accessor paths (after a valid build) ----------------------
' TRY-RID    E-FP-IDX TTHROWS
' TRY-RGN    E-FP-IDX TTHROWS
' TRY-REASON E-FP-IDX TTHROWS

T-REPORT

end-package

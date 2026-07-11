\ maki/fusion-plan-test.f - checked tests for the cad-2 fusion region planner.
\ Region growth over captured + hand-built model IR: elementwise chains, matmul
\ prologue/epilogue, one-contraction-per-region, the two-row-reduce softmax budget,
\ movement dissolution vs materialization, multi-use materialize, the typed split
\ rows, materialization flags, and every fail-closed accessor path.

require lib/test.f
require lib/string.f
require test/checker-assert.f
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
: TRY-RSN-LOW  ( -- )  -1 FP-SP-REASON-AT drop ;
: TRY-RSN-HIGH ( -- )  FP-CAP FP-SP-REASON-AT drop ;

\ ---- reason column is a real ENUM: assert stored reason via its MATCH render ---
\ FP-SPLIT-REASON@ now returns `reason` (a width-1 layout value, not an n), so a
\ split's reason is asserted through FP-REASON-NAME (the one exhaustive-MATCH
\ boundary word) rather than a raw `=` against an int tag. The old "bad reason tag"
\ runtime throw is gone: an out-of-family tag is now a checker reject, pinned by the
\ swapped-role negatives at the end of this suite.
: FT-REASON= ( n ptr u8 n -- )  {: sa:ptr su:n :}        \ split-idx expected-name --
   FP-SPLIT-REASON@ FP-REASON-NAME sa su T$= ;

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
0 s" matmul-boundary" FT-REASON=
\ node0 (first linear) is interior; node1 (region output) + node2 (model out) set
0 MIR-MAT@ TFALSE  1 MIR-MAT@ TTRUE  2 MIR-MAT@ TTRUE
\ report row wording
REPORT:NEW FP-REPORT+ REPORT:RENDER FT-SAVE
s" matmul-boundary at node 2" FT-IN

\ ---- MIX GELU SILU MATMUL RELU: an EW prologue (gelu,silu) CANNOT fuse into the matmul -
\ the backend-capability gate (FP-BACKEND-EMITS?, dot cad-matmul-prologue) clears EW->MATMUL
\ because lower-mm.f cannot pre-transform A/B (would be E-LMM-PROLOGUE). So the prologue is
\ its own region {gelu,silu}, and matmul+relu epilogue is a second region {matmul,relu}; the
\ split is a backend-capability boundary at the matmul. The prologue's tail (silu) now
\ MATERIALIZES as a region output - the honest traffic cost of not fusing the prologue.
MODEL: MIX ( x:2x2 w:2x2 -- y ) GELU SILU MATMUL RELU ;
FP-BUILD
FP-REGION-COUNT     2 T=
0 FP-REGION-MEMBERS 2 T=
1 FP-REGION-MEMBERS 2 T=
0 FP-RID@ 0 T=  1 FP-RID@ 0 T=  2 FP-RID@ 1 T=  3 FP-RID@ 1 T=
FP-SPLIT-COUNT      1 T=
0 s" backend-capability" FT-REASON=
0 FP-SPLIT-NODE@    2 T=
\ prologue interior (gelu) cleared; prologue output (silu) materialized; matmul interior;
\ model output (relu) materialized.
0 MIR-MAT@ TFALSE  1 MIR-MAT@ TTRUE  2 MIR-MAT@ TFALSE  3 MIR-MAT@ TTRUE
REPORT:NEW FP-REPORT+ REPORT:RENDER FT-SAVE
s" backend-capability at node 2" FT-IN

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
0 s" matmul-boundary" FT-REASON=
0 FP-SPLIT-NODE@   1 T=

\ ---- a materialize-verdict movement node splits (layout-conflict) ----------
\ GELU CONCAT: concat is MVV-MATERIALIZE, so it cannot dissolve into gelu's region.
MODEL: MMAT ( x:2x4 b:2x4 -- y ) GELU CONCAT ;
FP-BUILD
FP-REGION-COUNT    2 T=
FP-SPLIT-COUNT     1 T=
0 s" layout-conflict" FT-REASON=
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
0 s" barrier-boundary" FT-REASON=
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
0 s" multi-use-materialize" FT-REASON=
0 FP-SPLIT-NODE@   0 T=                    \ reported at the materialized producer
0 MIR-MAT@ TTRUE                            \ multi-use producer is materialized

\ ---- a trailing movement that IS the model output materializes (mat-flag fix) ----------
\ Slice-4 gap (dot maki-fusion-plan): FP-MAT-FLAG left a free/staged movement model-output at
\ mat=0, so its region had zero materialized outputs and the copy kernel failed E-LMV-NOOUT.
\ A movement model-output must materialize exactly like a compute model-output.
MODEL: MVT ( x:4x8 -- y ) TRANSPOSE ;             \ staged verdict, standalone model output
FP-BUILD
FP-REGION-COUNT 1 T=
0 MIR-MAT@ TTRUE
MODEL: MVS ( x:4x8 -- y ) SLICE:0..2 ;            \ free verdict (r0=0 aligned), model output
FP-BUILD
FP-REGION-COUNT 1 T=
0 MIR-MAT@ TTRUE

\ ---- fail-closed accessor paths (after a valid build) ----------------------
' TRY-RID    E-FP-IDX TTHROWS
' TRY-RGN    E-FP-IDX TTHROWS
' TRY-RSN-LOW  E-LAYOUT-BOUNDS TTHROWS
' TRY-RSN-HIGH E-LAYOUT-BOUNDS TTHROWS

\ ---- swapped-role negatives (dot habu-cad-adt-swap; capability S1) ----------
\ The reason column is addressed only through `ptr reason` (FP-SP-REASON-AT), so
\ the checker rejects any attempt to launder a raw n -- or reach the column past a
\ bare `ptr a` -- into or out of it. A mis-typed reason is impossible before runtime
\ (this replaces the old "bad reason tag" throw). Diagnostics are pinned as comments.
\ store an n where a reason is required (diag: "at '!' expected: reason<> ptr
\ reason<> actual: n ptr reason<>")
s" FPT-RSN-PTR ( n -- ptr reason ) FP-SP-REASON-AT" CHECK-QUIET-CANDIDATE! -1 T=
s" FPT-RSN-NIN ( n n -- ) FP-SP-REASON-AT !"          CHECK-QUIET-CANDIDATE! 0 T=
\ fetch a reason as a bare n -- enum->n laundering (diag: "at '@' expected: n
\ actual: reason<>")
s" FPT-RSN-NOUT ( n -- n ) FP-SP-REASON-AT @"         CHECK-QUIET-CANDIDATE! 0 T=
\ The migrated accessor cannot be weakened to a plain cell pointer.
s" FPT-RSN-BARE ( n -- ptr a ) FP-SP-REASON-AT" CHECK-QUIET-CANDIDATE! 0 T=

T-REPORT

end-package

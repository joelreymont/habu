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
: TRY-RID    ( -- )  99 MIR-NODE-ID FP-RID@ drop ;        \ node index rejects at the id refinement
: TRY-RGN    ( -- )  99 FP-REGION-ID drop ;               \ region index rejects at the id refinement
: TRY-RID-STATE ( -- )  0 MIR-NODE-ID FP-RID@ drop ;      \ FP-RID@ keeps its FP-CK guard
: TRY-ID-STATE  ( -- )  0 FP-REGION-ID drop ;             \ FP-REGION-ID keeps its FP-CK guard
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
0 FP-REGION-ID FP-REGION-MEMBERS  3 T=
FP-SPLIT-COUNT       0 T=
0 MIR-NODE-ID FP-RID@ 0 FP-REGION-ID FP-RGN= TTRUE
1 MIR-NODE-ID FP-RID@ 0 FP-REGION-ID FP-RGN= TTRUE
2 MIR-NODE-ID FP-RID@ 0 FP-REGION-ID FP-RGN= TTRUE
\ interior nodes cleared; only the model output materializes
0 MIR-NODE-ID MIR-MAT@ TFALSE  1 MIR-NODE-ID MIR-MAT@ TFALSE  2 MIR-NODE-ID MIR-MAT@ TTRUE

\ ---- FFN LINEAR GELU LINEAR: gelu fuses as matmul epilogue; second matmul ----
\ splits (one contraction per region) -> 2 regions, matmul-boundary at node 2.
MODEL: FFN ( x:2x3 w1:3x4 b1:1x4 w2:4x5 b2:1x5 -- y ) LINEAR GELU LINEAR ;
FP-BUILD
FP-REGION-COUNT 2 T=
0 MIR-NODE-ID FP-RID@ 0 FP-REGION-ID FP-RGN= TTRUE
1 MIR-NODE-ID FP-RID@ 0 FP-REGION-ID FP-RGN= TTRUE
2 MIR-NODE-ID FP-RID@ 1 FP-REGION-ID FP-RGN= TTRUE
FP-SPLIT-COUNT 1 T=
0 FP-SPLIT-NODE@ 2 MIR-NODE-ID MIR-NODE= TTRUE
0 s" matmul-boundary" FT-REASON=
\ node0 (first linear) is interior; node1 (region output) + node2 (model out) set
0 MIR-NODE-ID MIR-MAT@ TFALSE  1 MIR-NODE-ID MIR-MAT@ TTRUE  2 MIR-NODE-ID MIR-MAT@ TTRUE
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
0 FP-REGION-ID FP-REGION-MEMBERS 2 T=
1 FP-REGION-ID FP-REGION-MEMBERS 2 T=
0 MIR-NODE-ID FP-RID@ 0 FP-REGION-ID FP-RGN= TTRUE
1 MIR-NODE-ID FP-RID@ 0 FP-REGION-ID FP-RGN= TTRUE
2 MIR-NODE-ID FP-RID@ 1 FP-REGION-ID FP-RGN= TTRUE
3 MIR-NODE-ID FP-RID@ 1 FP-REGION-ID FP-RGN= TTRUE
FP-SPLIT-COUNT      1 T=
0 s" backend-capability" FT-REASON=
0 FP-SPLIT-NODE@ 2 MIR-NODE-ID MIR-NODE= TTRUE
\ prologue interior (gelu) cleared; prologue output (silu) materialized; matmul interior;
\ model output (relu) materialized.
0 MIR-NODE-ID MIR-MAT@ TFALSE  1 MIR-NODE-ID MIR-MAT@ TTRUE
2 MIR-NODE-ID MIR-MAT@ TFALSE  3 MIR-NODE-ID MIR-MAT@ TTRUE
REPORT:NEW FP-REPORT+ REPORT:RENDER FT-SAVE
s" backend-capability at node 2" FT-IN

\ ---- a free reshape dissolves and does NOT break the chain ------------------
\ GELU RESHAPE:8x4 RELU: reshape is MVV-FREE -> one region, reshape not materialized.
MODEL: MVB ( x:4x8 -- y ) GELU RESHAPE:8x4 RELU ;
FP-BUILD
FP-REGION-COUNT 1 T=
FP-SPLIT-COUNT  0 T=
1 MIR-NODE-ID MIR-MAT@ TFALSE             \ the free reshape stays dissolved

\ ---- matmul -> matmul splits (matmul-boundary) -----------------------------
MODEL: MM2 ( x:2x3 w1:3x4 w2:4x5 -- y ) MATMUL MATMUL ;
FP-BUILD
FP-REGION-COUNT    2 T=
FP-SPLIT-COUNT     1 T=
0 s" matmul-boundary" FT-REASON=
0 FP-SPLIT-NODE@ 1 MIR-NODE-ID MIR-NODE= TTRUE

\ ---- a materialize-verdict movement node splits (layout-conflict) ----------
\ GELU CONCAT: concat is MVV-MATERIALIZE, so it cannot dissolve into gelu's region.
MODEL: MMAT ( x:2x4 b:2x4 -- y ) GELU CONCAT ;
FP-BUILD
FP-REGION-COUNT    2 T=
FP-SPLIT-COUNT     1 T=
0 s" layout-conflict" FT-REASON=
0 FP-SPLIT-NODE@ 1 MIR-NODE-ID MIR-NODE= TTRUE

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
0 FP-SPLIT-NODE@ 2 MIR-NODE-ID MIR-NODE= TTRUE

\ ---- multi-use producer materializes + splits (hand-built IR) --------------
\ node0 = GELU(i0) ; node1 = ADD(n0, n0) -> n0 used twice -> not single-use.
MIR-RESET
0 0 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop
MAKI-OPKIND:GELU MIR-OP-BEGIN  0 MIR-SLOT-ID MIR-IN-REF MIR-IN+
0 0 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ drop
MAKI-OPKIND:ADD MIR-OP-BEGIN
0 MIR-NODE-ID MIR-NODE-REF MIR-IN+  0 MIR-NODE-ID MIR-NODE-REF MIR-IN+
0 0 SHAPE MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ drop
FP-BUILD
FP-REGION-COUNT    2 T=
FP-SPLIT-COUNT     1 T=
0 s" multi-use-materialize" FT-REASON=
0 FP-SPLIT-NODE@ 0 MIR-NODE-ID MIR-NODE= TTRUE   \ reported at the materialized producer
0 MIR-NODE-ID MIR-MAT@ TTRUE                \ multi-use producer is materialized

\ ---- a trailing movement that IS the model output materializes (mat-flag fix) ----------
\ Slice-4 gap (dot maki-fusion-plan): FP-MAT-FLAG left a free/staged movement model-output at
\ mat=0, so its region had zero materialized outputs and the copy kernel failed E-LMV-NOOUT.
\ A movement model-output must materialize exactly like a compute model-output.
MODEL: MVT ( x:4x8 -- y ) TRANSPOSE ;             \ staged verdict, standalone model output
FP-BUILD
FP-REGION-COUNT 1 T=
0 MIR-NODE-ID MIR-MAT@ TTRUE
MODEL: MVS ( x:4x8 -- y ) SLICE:0..2 ;            \ free verdict (r0=0 aligned), model output
FP-BUILD
FP-REGION-COUNT 1 T=
0 MIR-NODE-ID MIR-MAT@ TTRUE

\ ---- fail-closed accessor paths (after a valid build) ----------------------
' TRY-RID    E-MIR-IDX TTHROWS
' TRY-RGN    E-FP-IDX TTHROWS
' TRY-RSN-LOW  E-LAYOUT-BOUNDS TTHROWS
' TRY-RSN-HIGH E-LAYOUT-BOUNDS TTHROWS

\ ---- FP-RID@ / FP-REGION-ID keep the FP-CK guard (executed E-FP-STATE) ------
FP-RESET
' TRY-RID-STATE E-FP-STATE TTHROWS
' TRY-ID-STATE  E-FP-STATE TTHROWS

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

\ ---- region-id swapped-role negatives (dot habu-maki-apply-cad-27b7a7d7) ----
\ A fusion region id is a CAD-KIND:region: laundering it into a raw n, a
\ node-id, or a plan-id (and back) rejects before runtime; positives certify.
s" FPT-RGN-OK ( CAD-KIND:node-id -- CAD-KIND:region ) FP-RID@"           CHECK-QUIET-CANDIDATE! -1 T=
s" FPT-RGN-ID-OK ( n -- CAD-KIND:region ) FP-REGION-ID"                  CHECK-QUIET-CANDIDATE! -1 T=
s" FPT-RGN-EQ-OK ( CAD-KIND:region CAD-KIND:region -- bool ) FP-RGN="    CHECK-QUIET-CANDIDATE! -1 T=
s" FPT-RGN-MEM-OK ( CAD-KIND:region -- n ) FP-REGION-MEMBERS"            CHECK-QUIET-CANDIDATE! -1 T=
s" FPT-NEG-RGN-AS-N ( CAD-KIND:node-id -- n ) FP-RID@"                   CHECK-QUIET-CANDIDATE! 0 T=
s" FPT-NEG-RGN-AS-NODE ( CAD-KIND:node-id -- CAD-KIND:node-id ) FP-RID@" CHECK-QUIET-CANDIDATE! 0 T=
s" FPT-NEG-RGN-AS-PLAN ( CAD-KIND:node-id -- CAD-KIND:plan-id ) FP-RID@" CHECK-QUIET-CANDIDATE! 0 T=
s" FPT-NEG-ID-AS-NODE ( n -- CAD-KIND:node-id ) FP-REGION-ID"            CHECK-QUIET-CANDIDATE! 0 T=
s" FPT-NEG-EQ-NODE ( CAD-KIND:region CAD-KIND:node-id -- bool ) FP-RGN=" CHECK-QUIET-CANDIDATE! 0 T=
s" FPT-NEG-EQ-PLAN ( CAD-KIND:region CAD-KIND:plan-id -- bool ) FP-RGN=" CHECK-QUIET-CANDIDATE! 0 T=
s" FPT-NEG-MEM-RAW ( n -- n ) FP-REGION-MEMBERS"                         CHECK-QUIET-CANDIDATE! 0 T=

\ ---- rendered mismatch pins the qualified family name -----------------------
\ Signature-declared types only (FP-RID@'s declared output vs the candidate's
\ declared node-id output); locals-sourced actuals are garbled by the renderer
\ (known bug, dot habu-checker-diagnostic-renderer-66c3e741).
create FPT-DBUF 4096 allot
: FPT-DIAG< ( ptr u8 n -- )
   FPT-DBUF 4096 DIAG-BUFFER!
   0 0= DIAG-JSON!
   CHECK-CANDIDATE! 0 T= ;
: FPT-DIAG? ( ptr u8 n -- )  DIAG-BUFFER$ 2swap CONTAINS? TTRUE ;
: FPT-DIAG-END ( -- )  0 0= 0= DIAG-JSON!  DIAG-BUFFER-OFF ;

s" FPT-DIAG-SWAP ( CAD-KIND:node-id -- CAD-KIND:node-id ) FP-RID@" FPT-DIAG<
s\" \"expected\"" FPT-DIAG?
s\" \"actual\"" FPT-DIAG?
s" CAD-KIND:region" FPT-DIAG?
s" CAD-KIND:node-id" FPT-DIAG?
FPT-DIAG-END

T-REPORT

end-package

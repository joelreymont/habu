\ maki/demo-ffn-test.f - the FFN flagship end-to-end demo (dot cad-demo-ffn).
\
\ ONE committed, self-contained test that walks the WHOLE Model CAD loop over the
\ docs/model-cad.md flagship FFN block and locks every number as a regression. It IS
\ the demo transcript: model defined in the REPL -> LOWER keys -> FUSE regions +
\ traffic -> MEMORY coalescing -> TILE schedule -> CERTIFY/GOLDEN/GRADCHECK gates ->
\ OPTIMIZE promote decision -> PROMOTE artifact + store rows -> traffic comparison vs
\ the unfused baseline. The win is fusion/traffic reduction, not a tensor-core parity
\ claim (docs/model-cad.md "Flagship demo").
\
\ Model (as specified by the cad-demo-ffn dot):
\   MODEL: FFN-DEMO ( x:4x8 w1:8x16 b1:1x16 w2:16x8 b2:1x8 r:4x8 -- y )
\                   LINEAR GELU LINEAR RESIDUAL-ADD RMSNORM ;
\
\ Two honest notes about what this model can and cannot express:
\   1. The flagship is "linear -> bias -> GELU -> linear -> residual -> norm" (6
\      conceptual ops). Our LINEAR op FOLDS the affine bias (weight + bias operands),
\      so "linear -> bias" is a single IR node. The 6 conceptual ops therefore capture
\      as 5 IR nodes: LINEAR, GELU, LINEAR, RESIDUAL-ADD, RMSNORM.
\   2. A true residual/skip connection adds the block's ORIGINAL input x back in. Two
\      models here show both forms. FFN-DEMO keeps the POSITIONAL capture: RESIDUAL-ADD
\      consumes the next declared input r (input 5), NOT x (input 0), so node.3.in is
\      "n2 i5". FFN-SKIP (section 12) uses the NAMED-REFERENCE capture that landed: a
\      bare "x" token in the body pushes the ORIGINAL input as RESIDUAL-ADD's second
\      operand, so node.3.in is "n2 i0" and the separate r input is gone - a real skip.
\      Named-value capture is the v1 seam; compiling the whole body as ONE checker-
\      verified composition (CAD-PLAN section 3 full static compilation) is still
\      pending. The pipeline, gates, traffic, and promote loop are the full flagship for
\      both, and FFN-SKIP's backward makes x's gradient SUM two paths (fan-out).
\
\ The store (GA-SAVE artifact + PROMOTE evidence/schedule rows) is bracketed by
\ STORE-RESET so the test leaks nothing. maki -> habu only.

require lib/test.f
require lib/string.f
require lib/float.f
require lib/fmt.f
require maki/report.f
require maki/cad.f
require maki/backward.f
require maki/golden-artifact.f

package MAKI

\ ---- render containment helper (demo-local names; no clash with cad-test) ----
variable DM-VA  variable DM-VU
: DM-SAVE  ( ptr u8 n -- )  DM-VU ! DM-VA ! ;
: DM-IN    ( ptr u8 n -- )  DM-VA @ DM-VU @ 2swap CONTAINS? TTRUE ;
: DM-NOTIN ( ptr u8 n -- )  DM-VA @ DM-VU @ 2swap CONTAINS? TFALSE ;
: DM-SLOT ( n -- MIR:input-slot )  MIR-SLOT-ID ;
: DM-GRAD-NODE ( n -- CAD-KIND:node-id )
   DM-SLOT BW-SLOT-GRAD@ MIR-REF-NODE ;

\ ---- traffic-reduction line built from the report's byte fields --------------
\ ratio = bytes-before / bytes-after in hundredths -> "demo: traffic <i>.<ff>x reduced".
: DM-RATIO$ ( n n -- ptr u8 n ) {: bf:n af:n :}   \ bytes-before bytes-after -- line
   bf 100 * af /  {: r100:n :}
   SB-RESET
   s" demo: traffic " SB-APPEND
   r100 100 /   SB-INT   $2E SB-APPEND-C
   r100 100 mod 10 < if $30 SB-APPEND-C then  r100 100 mod SB-INT
   s" x reduced" SB-APPEND
   SB$ ;

T-RESET
STORE-RESET                                        \ start clean: no stale artifact / rows

\ ==== 1. Model defined in the REPL ==========================================
MODEL: FFN-DEMO ( x:4x8 w1:8x16 b1:1x16 w2:16x8 b2:1x8 r:4x8 -- y ) LINEAR GELU LINEAR RESIDUAL-ADD RMSNORM ;
MODEL-DEFINED? TTRUE
MODEL-K 5 T=
MODEL-NAME$ s" FFN-DEMO" T$=

\ the captured IR: the exact 5-node op sequence + operand wiring (residual reads r=i5)
MIR-RENDER DM-SAVE
s" ir.nodes: 5"             DM-IN
s" node.0.op: linear"       DM-IN
s" node.0.in: i0 i1 i2"     DM-IN
s" node.1.op: gelu"         DM-IN
s" node.2.op: linear"       DM-IN
s" node.2.in: n1 i3 i4"     DM-IN
s" node.3.op: residual-add" DM-IN
s" node.3.in: n2 i5"        DM-IN
s" node.4.op: rmsnorm"      DM-IN

\ ==== 2. LOWER: model output shape / dtype / layout keys ====================
LOWER
dup REPORT:OPS-BEFORE@ 5 T=
dup REPORT:REGIONS@    5 T=
dup REPORT:SHAPE$  s" 4x8" T$=
dup REPORT:DTYPE$  s" f32" T$=
dup REPORT:LAYOUT$ s" row" T$=
dup REPORT:RENDER DM-SAVE  s" report.model: FFN-DEMO" DM-IN
drop

\ ==== 3. FUSE: regions, typed splits, bytes before / after ==================
\ region 0 = linear+gelu epilogue; the second matmul splits (one contraction per
\ region); rmsnorm's reduction barrier splits again -> 3 regions, 2 typed splits.
FUSE
dup REPORT:OPS-BEFORE@   5 T=
dup REPORT:OPS-AFTER@    3 T=
dup REPORT:REGIONS@      3 T=
dup REPORT:MATERIALIZED@ 3 T=
dup REPORT:SPLIT-COUNT   2 T=
dup 0 REPORT:SPLIT@ s" matmul-boundary at node 2"  T$=
dup 1 REPORT:SPLIT@ s" barrier-boundary at node 4" T$=
dup REPORT:BYTES-KNOWN? TTRUE
dup REPORT:BYTES-BEFORE@ 3040 T=
dup REPORT:BYTES-AFTER@  2272 T=
dup REPORT:RENDER DM-SAVE
s" fusion.split.0: matmul-boundary at node 2"  DM-IN
s" fusion.split.1: barrier-boundary at node 4" DM-IN
drop

\ ==== 4. MEMORY: coalescing rows (compute-only model -> no movement rows) =====
\ a 1xC bias reads broadcast-register; compiler-allocated outputs are 16B-aligned
\ (coalesced-v4); model inputs carry no buffer so their alignment is unknown -> scalar.
MEMORY REPORT:RENDER DM-SAVE
s" coalesce.tensor.2: i2"                             DM-IN
s" coalesce.status.2: broadcast"                      DM-IN
s" coalesce.tensor.6: n1"                             DM-IN
s" coalesce.status.6: coalesced-v4"                   DM-IN
s" coalesce.status.0: unaligned"                      DM-IN
s" memory.align: input 0 unknown alignment -> scalar" DM-IN
s" memory.move:"                                      DM-NOTIN

\ ==== 5. TILE: gemm-tf32 family candidates + default + cache key ============
TILE
dup REPORT:CAND-COUNT 32 T=
dup REPORT:SELECT@ 0 T=
dup 0 REPORT:CAND@  s" gemm-tf32-v1 bm=64 bn=64 bk=32 warps=4 stages=1"   T$=
dup 31 REPORT:CAND@ s" gemm-tf32-v1 bm=128 bn=128 bk=64 warps=8 stages=2" T$=
\ full section-7.4 cache key, exact: the report arena copy is stable, so build the
\ expected key in SB with the binary-dependent engine field spliced in, then compare.
dup REPORT:CACHE$
SB-RESET s" 3C09A0D86344114A|4x16|f32|row|al?|sm_87|" SB-APPEND
ENGINE-KEY$ SB-APPEND  s" |unprobed" SB-APPEND  SB$
STR= TTRUE
drop

\ ==== 6. CERTIFY: model-level static legality passes ========================
CERTIFY dup G-CERTIFY REPORT:GATE-TAG@ V-PASS T= drop

\ ==== 7. GOLDEN: self-consistency, then GA-SAVE + artifact-backed pass =======
GOLDEN
dup G-GOLDEN REPORT:GATE-TAG@ V-PASS T=
dup G-GOLDEN REPORT:GATE-REASON@ DM-SAVE  s" host self-consistent (5 nodes)" DM-IN
drop
GA-EXISTS? TFALSE                                  \ no external artifact yet
GA-SAVE                                            \ dump synthetic inputs + executed output
GA-EXISTS? TTRUE
GOLDEN
dup G-GOLDEN REPORT:GATE-TAG@ V-PASS T=
dup G-GOLDEN REPORT:GATE-REASON@ DM-SAVE  s" external artifact FFN-DEMO matched" DM-IN
dup REPORT:RENDER DM-SAVE  s" golden: external reference artifact comparison" DM-IN
drop

\ ==== 8. GRADCHECK: numeric host gradcheck passes ===========================
GRADCHECK
dup G-GRADCHECK REPORT:GATE-TAG@ V-PASS T=
dup G-GRADCHECK REPORT:GATE-REASON@ DM-SAVE  s" input(s) gradchecked" DM-IN
drop

\ ==== 9. OPTIMIZE: aggregate report; CAD 7c gate set promotes on host ========
\ CERTIFY + GOLDEN + GRADCHECK pass; PROFILE is not-run but mandatory-to-run and
\ non-blocking, so the gate set clears and OPTIMIZE records the promote decision.
OPTIMIZE
dup G-CERTIFY   REPORT:GATE-TAG@ V-PASS   T=
dup G-GOLDEN    REPORT:GATE-TAG@ V-PASS   T=
dup G-GRADCHECK REPORT:GATE-TAG@ V-PASS   T=
dup G-PROFILE   REPORT:GATE-TAG@ V-NOTRUN T=
dup REPORT:RENDER DM-SAVE
s" gate.certify.verdict: pass"           DM-IN
s" gate.golden.verdict: pass"            DM-IN
s" gate.gradcheck.verdict: pass"         DM-IN
s" promote: gates pass; artifact cached" DM-IN
drop

\ ==== 10. PROMOTE: artifact promoted; store evidence + schedule rows land ====
PROMOTE dup REPORT:CACHE$ s" FFN-DEMO" T$= drop
0 TARGET:SM87 SK-KEY$ EVID-GET TTRUE
s" certify=pass|golden=pass|gradcheck=pass|profile=not-run" T$=
0 TARGET:SM87 SK-KEY$ SCHED-GET TTRUE 0 T=         \ region-0 selection = gemm default (candidate 0)

\ ==== 11. Comparison vs the unfused baseline ================================
\ unfused traffic 3040 B, fused 2272 B: the flagship win, computed from the report's
\ byte fields (not a hardcoded literal), locked as "demo: traffic 1.33x reduced".
FUSE dup REPORT:BYTES-BEFORE@ swap REPORT:BYTES-AFTER@ DM-RATIO$
s" demo: traffic 1.33x reduced" T$=

\ ==== 12. FFN-SKIP: the TRUE skip connection via a named reference ===========
\ Same flagship block, but the residual adds back the ORIGINAL input x. The body
\ names it by writing a bare "x" (the signature input) before RESIDUAL-ADD, so the
\ residual's second operand is input 0 (node.3.in "n2 i0"), and the separate r input
\ is GONE - 5 inputs, not 6. The 5-node IR, fusion/traffic win, gates, and PROMOTE are
\ the full flagship; the traffic is IDENTICAL to the r-input version (x and r are both
\ 4x8 and each is read once by the residual), so the skip changes wiring, not bytes.
STORE-RESET
MODEL: FFN-SKIP ( x:4x8 w1:8x16 b1:1x16 w2:16x8 b2:1x8 -- y ) LINEAR GELU LINEAR x RESIDUAL-ADD RMSNORM ;
MODEL-DEFINED? TTRUE
MODEL-K 5 T=
MODEL-NAME$ s" FFN-SKIP" T$=

\ the captured IR: residual reads x=i0 (the true skip); there is no declared r input
MIR-RENDER DM-SAVE
s" ir.nodes: 5"             DM-IN
s" ir.inputs: 5"            DM-IN
s" node.0.op: linear"       DM-IN
s" node.0.in: i0 i1 i2"     DM-IN
s" node.2.op: linear"       DM-IN
s" node.2.in: n1 i3 i4"     DM-IN
s" node.3.op: residual-add" DM-IN
s" node.3.in: n2 i0"        DM-IN
s" node.4.op: rmsnorm"      DM-IN

\ LOWER + FUSE: same output shape and the same fusion/traffic win as FFN-DEMO
LOWER dup REPORT:SHAPE$ s" 4x8" T$= drop
FUSE
dup REPORT:OPS-AFTER@    3 T=
dup REPORT:REGIONS@      3 T=
dup REPORT:MATERIALIZED@ 3 T=
dup REPORT:SPLIT-COUNT   2 T=
dup 0 REPORT:SPLIT@ s" matmul-boundary at node 2"  T$=
dup 1 REPORT:SPLIT@ s" barrier-boundary at node 4" T$=
dup REPORT:BYTES-BEFORE@ 3040 T=
dup REPORT:BYTES-AFTER@  2272 T=
drop

\ gates + promote end-to-end: host self-consistent + gradcheck pass, PROFILE non-blocking
CERTIFY dup G-CERTIFY REPORT:GATE-TAG@ V-PASS T= drop
GOLDEN
dup G-GOLDEN REPORT:GATE-TAG@ V-PASS T=
dup G-GOLDEN REPORT:GATE-REASON@ DM-SAVE  s" host self-consistent (5 nodes)" DM-IN
drop
GRADCHECK dup G-GRADCHECK REPORT:GATE-TAG@ V-PASS T= drop
OPTIMIZE
dup G-CERTIFY   REPORT:GATE-TAG@ V-PASS T=
dup G-GOLDEN    REPORT:GATE-TAG@ V-PASS T=
dup G-GRADCHECK REPORT:GATE-TAG@ V-PASS T=
dup REPORT:RENDER DM-SAVE  s" promote: gates pass; artifact cached" DM-IN
drop
PROMOTE dup REPORT:CACHE$ s" FFN-SKIP" T$= drop

\ backward: x fans out (the first LINEAR reads it AND the residual skip re-reads it),
\ so its cotangent SUMS the two paths -> the input-0 gradient node is an OP-ADD. The
\ r-input FFN-DEMO had NO fan-out on x (x fed only the first linear -> a matmul grad).
BW-BUILD
0 DM-SLOT BW-HAS-GRAD? TTRUE
0 DM-GRAD-NODE MIR-OP@ OP-ADD T=
0 DM-GRAD-NODE MIR-ROWS@ 4 T=
0 DM-GRAD-NODE MIR-COLS@ 8 T=

STORE-RESET                                        \ leave the store as we found it
T-REPORT

;package

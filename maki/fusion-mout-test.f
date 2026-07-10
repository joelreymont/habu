\ maki/fusion-mout-test.f - the single-materialized-output-per-region invariant.
\
\ PROOF that the fusion planner (maki/fusion-plan.f) NEVER plans a region with more
\ than one materialized output - the invariant the lowering passes' MULTIOUT guards
\ (E-LEW-MULTIOUT / E-LRED-MULTIOUT / E-LMM-MULTIOUT) assert as defense-in-depth. It is
\ a structural property of FP-ASSIGN + FP-MARK, not a v1 feature cap:
\
\   1. FP-ASSIGN grows a region as a LINEAR operand-0 chain. FP-STEP joins node K into
\      its operand-0 producer P's region only when FP-JOIN? holds, and FP-JOIN? refuses
\      when `P FP-REF-USES 1 >` (a fanned-out producer is materialized). So any node used
\      more than once - in ANY operand position - has its operand-0 child refuse to join,
\      making it a chain TAIL. Every non-tail member therefore has FP-REF-USES == 1 and its
\      sole use is operand-0 of the next member (a simple chain, no in-region fan-out).
\   2. FP-MAT-FLAG marks a node materialized iff it is a model output, a materialize/gathered
\      or region-crossing movement, multi-use, OR a region output (a consumer in another
\      region). Each of those forces the node to be a chain TAIL, so an INTERIOR member is
\      never materialized.
\   => every region has exactly ONE materialized output = its tail. Never >1 (structurally
\      impossible); never 0 (the tail is a model output, or its consumer lives in another
\      region making it a region output - see fusion-plan-test MVT/MVS for the movement
\      model-output case that closed the historical zero-output E-LMV-NOOUT bug).
\
\ This suite is the positive owner of that invariant: a battery of FAN-OUT models (captured
\ residual/skip + hand-built extreme fan-out) each asserts MAXMAT == 1 over all planned
\ regions. The three lowering guards' fail-closed behaviour on a (corrupted) multi-output
\ plan lives with each pass: maki/lower-ew-test.f, lower-red-test.f, lower-mm-test.f. Host-
\ only (planner assertion, no PTX/device leg). Load via maki/test.f.

require lib/test.f
require lib/string.f
require maki/cad.f
require maki/fusion-plan.f

package MAKI

\ materialized outputs planned into region r
: MOUT-RGN ( n -- n ) {: r:n :}
   0  MIR-N@ 0 ?do  i FP-RID@ r =  i MIR-MAT@  and if 1+ then  loop ;

\ the maximum materialized-output count over every planned region (FP-BUILD must have run)
: MOUT-MAX ( -- n )
   0  FP-REGION-COUNT 0 ?do  i MOUT-RGN max  loop ;

\ regions whose materialized-output count is not exactly one (the invariant violation count)
: MOUT-BAD ( -- n )
   0  FP-REGION-COUNT 0 ?do  i MOUT-RGN 1 <> if 1+ then  loop ;

T-RESET

\ ---- captured fan-out battery: >V names a value; a bare NAME fans it to a later op ----

\ residual/skip: gelu output feeds the relu chain AND the residual add (2 uses)
MODEL: MO-RESID ( x:4x8 -- y ) GELU >V H RELU H RESIDUAL-ADD ;
FP-BUILD
FP-REGION-COUNT 2 T=  MOUT-MAX 1 T=  MOUT-BAD 0 T=

\ one producer fanned to THREE consumers (relu chain + add + mul)
MODEL: MO-TRIPLE ( x:4x8 -- y ) GELU >V H RELU H ADD H MUL ;
FP-BUILD
MOUT-MAX 1 T=  MOUT-BAD 0 T=

\ cross-class fan-out: gelu feeds a row-reduce branch AND a residual branch
MODEL: MO-XCLASS ( x:4x8 -- y ) GELU >V H RMSNORM H RESIDUAL-ADD ;
FP-BUILD
MOUT-MAX 1 T=  MOUT-BAD 0 T=

\ two fan-out points in one body (gelu and relu both fan out)
MODEL: MO-DEEP ( x:4x8 -- y ) GELU >V A RELU >V B A ADD B ADD ;
FP-BUILD
MOUT-MAX 1 T=  MOUT-BAD 0 T=

\ matmul output fanned to a relu branch AND a residual branch
MODEL: MO-MMFAN ( x:2x3 w:3x4 -- y ) MATMUL >V H RELU H RESIDUAL-ADD ;
FP-BUILD
MOUT-MAX 1 T=  MOUT-BAD 0 T=

\ FFN block + skip: linear/gelu intermediate fans to a second linear AND the residual add
MODEL: MO-FFN ( x:2x3 w1:3x4 b1:1x4 w2:4x4 b2:1x4 -- y ) LINEAR GELU >V H LINEAR RMSNORM H RESIDUAL-ADD ;
FP-BUILD
MOUT-MAX 1 T=  MOUT-BAD 0 T=

\ ---- hand-built extreme fan-out (beyond the DSL's chain shape) ---------------
\ n0 -> three independent relus, recombined by two adds. n0 is used 3x, each relu once.
MIR-RESET
0 0 MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop                                              \ i0
OP-GELU MIR-OP-BEGIN  0 MIR-IN-REF MIR-IN+  0 0 MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ drop \ n0=GELU(i0)
OP-RELU MIR-OP-BEGIN  0 MIR-IN+            0 0 MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ drop  \ n1=RELU(n0)
OP-RELU MIR-OP-BEGIN  0 MIR-IN+            0 0 MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ drop  \ n2=RELU(n0)
OP-RELU MIR-OP-BEGIN  0 MIR-IN+            0 0 MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ drop  \ n3=RELU(n0)
OP-ADD  MIR-OP-BEGIN  1 MIR-IN+ 2 MIR-IN+  0 0 MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ drop  \ n4=ADD(n1,n2)
OP-ADD  MIR-OP-BEGIN  4 MIR-IN+ 3 MIR-IN+  0 0 MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ drop  \ n5=ADD(n4,n3)
FP-BUILD
FP-REGION-COUNT 4 T=  MOUT-MAX 1 T=  MOUT-BAD 0 T=

\ diamond: n0 -> {relu, silu} -> add (producer used in op0 and op1 of the join)
MIR-RESET
0 0 MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop                                              \ i0
OP-GELU MIR-OP-BEGIN  0 MIR-IN-REF MIR-IN+  0 0 MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ drop \ n0=GELU(i0)
OP-RELU MIR-OP-BEGIN  0 MIR-IN+            0 0 MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ drop  \ n1=RELU(n0)
OP-SILU MIR-OP-BEGIN  0 MIR-IN+            0 0 MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ drop  \ n2=SILU(n0)
OP-ADD  MIR-OP-BEGIN  1 MIR-IN+ 2 MIR-IN+  0 0 MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ drop  \ n3=ADD(n1,n2)
FP-BUILD
FP-REGION-COUNT 3 T=  MOUT-MAX 1 T=  MOUT-BAD 0 T=

T-REPORT

end-package

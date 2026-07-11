\ maki/gradcheck-test.f - checked tests for host model-IR gradcheck (cad-7a).
\ At tensor granularity via the full-tensor executor: the flagship MLP (LINEAR GELU
\ LINEAR) plus norms/softmax/rope/matmul/movement/reduce/scatter models all gradcheck
\ V-PASS; only cast stays honest not-run; IR non-corruption via MIR-MARK/RELEASE; the
\ tolerance predicate; and DETECTION of a deliberately-wrong adjoint fixture.

require lib/test.f
require lib/string.f
require maki/cad.f
require maki/gradcheck.f

package MAKI

\ ---- reason containment helper ---------------------------------------------
: GCT-REASON-IN ( ptr u8 n -- )  GC-RE$ 2swap CONTAINS? TTRUE ;

\ ---- deliberately-wrong-adjoint fixture (hand-built) -----------------------
create GX 4 cells allot   create GS 4 cells allot

: DET-BUILD ( -- )        \ slot0=x, n0=gelu(x), slot1=seed, n1=WRONG relu-bwd(seed,x)
   MIR-RESET
   2 2 MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop
   MAKI-OPKIND:GELU MIR-OP-BEGIN 0 MIR-IN-REF MIR-IN+ 2 2 MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ drop
   2 2 MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop
   MAKI-OPKIND:RELU-BWD MIR-OP-BEGIN 1 MIR-IN-REF MIR-IN+ 0 MIR-IN-REF MIR-IN+
      2 2 MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ drop ;

: DET-BUILD-OK ( -- )     \ same fixture with the CORRECT gelu-bwd adjoint
   MIR-RESET
   2 2 MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop
   MAKI-OPKIND:GELU MIR-OP-BEGIN 0 MIR-IN-REF MIR-IN+ 2 2 MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ drop
   2 2 MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW MIR-INPUT+ drop
   MAKI-OPKIND:GELU-BWD MIR-OP-BEGIN 1 MIR-IN-REF MIR-IN+ 0 MIR-IN-REF MIR-IN+
      2 2 MAKI-DTYPE:DF32 MAKI-LAYOUT:ROW 0 1 MIR-OP+ drop ;

: DET-RUN ( -- )          \ fill x + seed, run the whole hand-built IR
   0.7 GX 0 T-SET 1.3 GX 1 T-SET 0.9 GX 2 T-SET 1.7 GX 3 T-SET
   1.0 GS 4 T-FILL
   EX-RESET  GX 0 EX-BIND  GS 1 EX-BIND  EX-RUN ;

: DET-ANALYTIC ( -- r )  1 EX-OUT@ 0 T-GET ;      \ backward node 1, element 0

: DET-FD ( -- r )         \ central diff of the forward gelu node 0 at element 0
   GX 0 T-GET {: base:r :}
   base 0.001 f+ GX 0 T-SET  1 EX-RUN-N  0 EX-OUT@ 0 T-GET {: yp:r :}
   base 0.001 f- GX 0 T-SET  1 EX-RUN-N  0 EX-OUT@ 0 T-GET {: ym:r :}
   base GX 0 T-SET
   yp ym f-  0.002 f/ ;

T-RESET

\ ---- tolerance predicate ---------------------------------------------------
1.0 1.0 GC-CLOSE? TTRUE
1.0 1.005 GC-CLOSE? TTRUE                 \ within absolute 0.01
1.0 2.0 GC-CLOSE? TFALSE                  \ a 2x-wrong gradient is caught

\ ---- PASS: reference-complete elementwise models ---------------------------
MODEL: GC-G ( x:2x2 -- y ) GELU ;
GC-RUN V-PASS T=
MODEL: GC-CH ( x:2x2 -- y ) GELU SILU RELU ;
GC-RUN V-PASS T=
MODEL: GC-MUL ( x:2x2 y:2x2 -- z ) MUL ;
GC-RUN V-PASS T=
MODEL: GC-ADD ( x:2x2 y:2x2 -- z ) ADD ;
GC-RUN V-PASS T=

\ ---- PASS: the flagship MLP now gradchecks on host --------------------------
MODEL: GC-MLP ( x:2x3 w1:3x4 b1:1x4 w2:4x2 b2:1x2 -- y ) LINEAR GELU LINEAR ;
GC-RUN V-PASS T=
s" host: 5 input(s) gradchecked" GCT-REASON-IN

\ ---- PASS: contraction + row words + rope ----------------------------------
MODEL: GC-MM ( x:2x3 w:3x4 -- y ) MATMUL ;
GC-RUN V-PASS T=
MODEL: GC-LN ( x:4x8 -- y ) LAYERNORM ;
GC-RUN V-PASS T=
MODEL: GC-RMS ( x:4x8 -- y ) RMSNORM ;
GC-RUN V-PASS T=
MODEL: GC-SM ( x:2x4 -- y ) SOFTMAX-ROW ;
GC-RUN V-PASS T=
MODEL: GC-RP ( x:2x4 c:2x4 s:2x4 -- y ) ROPE ;
GC-RUN V-PASS T=

\ ---- PASS: movement + reduce/scatter (backward exercises pad-scatter etc) ----
MODEL: GC-SL ( x:4x4 -- y ) SLICE:0..2 ;
GC-RUN V-PASS T=
MODEL: GC-TR ( x:2x3 -- y ) TRANSPOSE ;
GC-RUN V-PASS T=
MODEL: GC-RE ( x:4x2 -- y ) RESHAPE:2x4 ;
GC-RUN V-PASS T=
MODEL: GC-BS ( x:2x3 b:1x3 -- y ) BIAS ;
GC-RUN V-PASS T=
MODEL: GC-SC ( x:2x3 s:1x1 -- z ) SCALE ;
GC-RUN V-PASS T=
MODEL: GC-GA ( x:4x2 idx:3x1 -- y ) GATHER ;
GC-RUN V-PASS T=

\ ---- NOT-RUN: only cast remains (non-differentiable) ------------------------
MODEL: GC-CAST ( x:2x2 -- y ) CAST ;
GC-RUN V-NOTRUN T=
s" no-adjoint:cast" GCT-REASON-IN

\ ---- IR is NOT corrupted: the throwaway backward pass is released -----------
MODEL: GC-K ( x:2x2 -- y ) GELU RELU ;
GC-RUN drop
MIR-N@ 2 T=                               \ 2 forward nodes, backward pass rolled back
MIR-IN-SLOTS@ 1 T=                        \ only x; the seed slot was released

\ ---- gate wiring: GRADCHECK-INTO produces a real verdict --------------------
MODEL: GC-CAST2 ( x:2x2 -- y ) CAST ;
REPORT:NEW GRADCHECK-INTO G-GRADCHECK REPORT:GATE-TAG@ V-NOTRUN T=
MODEL: GC-G2 ( x:2x2 -- y ) GELU ;
REPORT:NEW GRADCHECK-INTO G-GRADCHECK REPORT:GATE-TAG@ V-PASS T=

\ ---- DETECTION: a deliberately-wrong adjoint fixture -----------------------
\ relu-bwd where gelu-bwd is correct: the analytic grad it yields must DISAGREE
\ with the finite difference of the forward gelu.
DET-BUILD  DET-RUN
DET-ANALYTIC DET-FD GC-CLOSE? TFALSE      \ wrong analytic caught

\ ---- the same fixture with the CORRECT adjoint (gelu-bwd) agrees ------------
DET-BUILD-OK  DET-RUN
DET-ANALYTIC DET-FD GC-CLOSE? TTRUE

T-REPORT

end-package

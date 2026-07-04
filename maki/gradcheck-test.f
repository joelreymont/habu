\ maki/gradcheck-test.f - checked tests for host model-IR gradcheck (cad-9d).
\ Pass on reference-complete elementwise models (unary + binary, chains, copy-through
\ input-ref grads), honest not-run with a named reason for reductions / matmul /
\ no-adjoint / unsupported-adjoint, the tolerance predicate, IR non-corruption via
\ MIR-MARK/RELEASE, and DETECTION of a deliberately-wrong adjoint fixture.

require lib/test.f
require lib/string.f
require maki/cad.f
require maki/gradcheck.f

package MAKI

\ ---- reason containment helper ---------------------------------------------
: GCT-REASON-IN ( ptr u8 n -- )  GC-RE$ 2swap CONTAINS? TTRUE ;

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
\ binary product rule: both input gradients are checked
MODEL: GC-MUL ( x:2x2 y:2x2 -- z ) MUL ;
GC-RUN V-PASS T=
\ add copies the cotangent: the input gradients are the seed itself (an input ref)
MODEL: GC-ADD ( x:2x2 y:2x2 -- z ) ADD ;
GC-RUN V-PASS T=

\ ---- IR is NOT corrupted: the throwaway backward pass is released -----------
MODEL: GC-K ( x:2x2 -- y ) GELU RELU ;
GC-RUN drop
MIR-N@ 2 T=                               \ 2 forward nodes, backward pass rolled back
MIR-IN-SLOTS@ 1 T=                        \ only x; the seed slot was released

\ ---- NOT-RUN with named reasons --------------------------------------------
MODEL: GC-LN ( x:4x8 -- y ) LAYERNORM ;
GC-RUN V-NOTRUN T=
s" host-unsupported:layernorm" GCT-REASON-IN
MODEL: GC-MM ( x:2x3 w:3x4 -- y ) MATMUL ;
GC-RUN V-NOTRUN T=
s" host-unsupported:matmul" GCT-REASON-IN
MODEL: GC-CAST ( x:2x2 -- y ) CAST ;
GC-RUN V-NOTRUN T=
s" no-adjoint:cast" GCT-REASON-IN
\ cad-9e: slice now HAS a supported adjoint, but the movement op is not host-executable
\ (the reduce/scatter host executor is the cad-7 full-tensor path) -> honest host-unsupported.
MODEL: GC-SL ( x:4x4 -- y ) SLICE:0..2 ;
GC-RUN V-NOTRUN T=
s" host-unsupported:slice" GCT-REASON-IN

\ ---- gate wiring: GRADCHECK-INTO produces a real verdict --------------------
RPT-NEW GRADCHECK-INTO G-GRADCHECK RPT-GATE-TAG@ V-NOTRUN T=   \ (GC-SL still active)
MODEL: GC-G2 ( x:2x2 -- y ) GELU ;
RPT-NEW GRADCHECK-INTO G-GRADCHECK RPT-GATE-TAG@ V-PASS T=

\ ---- DETECTION: a deliberately-wrong adjoint fixture -----------------------
\ Hand-build gelu(x) forward + a WRONG backward node (relu-bwd where gelu-bwd is
\ correct); the analytic grad it yields must DISAGREE with the finite difference.
MIR-RESET
2 2 DT-F32 LAY-ROW MIR-INPUT+ drop                              \ slot0 = data x
OP-GELU MIR-OP-BEGIN  0 MIR-IN-REF MIR-IN+  2 2 DT-F32 LAY-ROW 0 1 MIR-OP+ drop   \ n0 = gelu(x)
2 2 DT-F32 LAY-ROW MIR-INPUT+ drop                              \ slot1 = seed cotangent
OP-RELU-BWD MIR-OP-BEGIN  1 MIR-IN-REF MIR-IN+  0 MIR-IN-REF MIR-IN+  2 2 DT-F32 LAY-ROW 0 1 MIR-OP+ drop  \ n1 = relu-bwd(seed,x) WRONG
GC-FILL  1.0 1 GC-IS!
MIR-N@ GC-EVAL-N
1 GC-VF@  0 1 0 GC-FD  GC-CLOSE? TFALSE                         \ wrong analytic caught

\ same fixture with the CORRECT adjoint (gelu-bwd) agrees ---------------------
MIR-RESET
2 2 DT-F32 LAY-ROW MIR-INPUT+ drop
OP-GELU MIR-OP-BEGIN  0 MIR-IN-REF MIR-IN+  2 2 DT-F32 LAY-ROW 0 1 MIR-OP+ drop
2 2 DT-F32 LAY-ROW MIR-INPUT+ drop
OP-GELU-BWD MIR-OP-BEGIN  1 MIR-IN-REF MIR-IN+  0 MIR-IN-REF MIR-IN+  2 2 DT-F32 LAY-ROW 0 1 MIR-OP+ drop
GC-FILL  1.0 1 GC-IS!
MIR-N@ GC-EVAL-N
1 GC-VF@  0 1 0 GC-FD  GC-CLOSE? TTRUE

T-REPORT

end-package

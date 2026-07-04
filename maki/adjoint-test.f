\ maki/adjoint-test.f - checked tests for the model-op adjoint registry (cad-9a).
\ Adjoint id, save-need, gradient mask, v1-supported flag, the dedicated backward
\ op-kind mapping, the op-registry vjp wiring, and every fail-closed lookup path.

require lib/test.f
require lib/string.f
require maki/adjoint.f

package MAKI

\ ---- fail-closed probes (top level cannot push quotations) ------------------
: TRY-ADJ-KIND   ( -- )  OP-N ADJ-ID drop ;               \ op-kind out of range
: TRY-ADJ-SAVEBAD ( -- ) SAVE-N ADJ-SAVE-NAME 2drop ;     \ save tag out of range
: TRY-ADJ-UNSUP-OK ( -- ) OP-GELU ADJ-UNSUP$ 2drop ;      \ reason from a supported op

T-RESET

\ ---- adjoint ids -----------------------------------------------------------
OP-GELU        ADJ-ID ADJ-GELU     T=
OP-SILU        ADJ-ID ADJ-SILU     T=
OP-RELU        ADJ-ID ADJ-RELU     T=
OP-ADD         ADJ-ID ADJ-COPY     T=
OP-RESIDUAL-ADD ADJ-ID ADJ-COPY    T=
OP-MUL         ADJ-ID ADJ-MUL      T=
OP-MATMUL      ADJ-ID ADJ-MATMUL   T=
OP-SOFTMAX-ROW ADJ-ID ADJ-SOFTMAX  T=
OP-RMSNORM     ADJ-ID ADJ-RMSNORM  T=
OP-ROPE        ADJ-ID ADJ-ROPE     T=
OP-RESHAPE     ADJ-ID ADJ-RESHAPE  T=
OP-CONCAT      ADJ-ID ADJ-CONCAT   T=
OP-CAST        ADJ-ID ADJ-NONE     T=

\ ---- differentiability (cast has no adjoint; the backward ops have none too) --
OP-GELU        ADJ-HAS? TTRUE
OP-CAST        ADJ-HAS? TFALSE
OP-GELU-BWD    ADJ-HAS? TFALSE            \ no second-order in v1
OP-SOFTMAX-ROW-BWD ADJ-HAS? TFALSE

\ ---- save-need: gelu needs the INPUT, softmax the OUTPUT, add neither ---------
OP-GELU        ADJ-SAVE SAVE-INPUT  T=
OP-RELU        ADJ-SAVE SAVE-INPUT  T=
OP-MATMUL      ADJ-SAVE SAVE-INPUT  T=
OP-SOFTMAX-ROW ADJ-SAVE SAVE-OUTPUT T=
OP-ADD         ADJ-SAVE SAVE-NONE   T=
OP-ROPE        ADJ-SAVE SAVE-NONE   T=
OP-RESHAPE     ADJ-SAVE SAVE-NONE   T=
OP-SOFTMAX-ROW ADJ-SAVE ADJ-SAVE-NAME s" output" T$=
OP-GELU        ADJ-SAVE ADJ-SAVE-NAME s" input"  T$=

\ ---- gradient mask: unary ops -> input 0 only; binary -> both ----------------
OP-GELU   0 ADJ-GRAD-IN? TTRUE
OP-GELU   1 ADJ-GRAD-IN? TFALSE
OP-ADD    0 ADJ-GRAD-IN? TTRUE
OP-ADD    1 ADJ-GRAD-IN? TTRUE
OP-MATMUL 0 ADJ-GRAD-IN? TTRUE
OP-MATMUL 1 ADJ-GRAD-IN? TTRUE
\ rope's cos/sin (inputs 1,2) are constants -> no gradient
OP-ROPE   0 ADJ-GRAD-IN? TTRUE
OP-ROPE   1 ADJ-GRAD-IN? TFALSE
OP-ROPE   2 ADJ-GRAD-IN? TFALSE

\ ---- v1 supported flag: emittable adjoints vs fail-closed boundary -----------
OP-GELU    ADJ-SUP? TTRUE
OP-MATMUL  ADJ-SUP? TTRUE
OP-CONCAT  ADJ-SUP? TTRUE
OP-RESHAPE ADJ-SUP? TTRUE
OP-SLICE   ADJ-SUP? TFALSE
OP-GATHER  ADJ-SUP? TFALSE
OP-BIAS    ADJ-SUP? TFALSE
OP-SCALE   ADJ-SUP? TFALSE
OP-LINEAR  ADJ-SUP? TFALSE
OP-CAST    ADJ-SUP? TFALSE

\ ---- dedicated backward op-kind mapping -------------------------------------
OP-GELU        ADJ-BOP OP-GELU-BWD        T=
OP-SILU        ADJ-BOP OP-SILU-BWD        T=
OP-RELU        ADJ-BOP OP-RELU-BWD        T=
OP-SOFTMAX-ROW ADJ-BOP OP-SOFTMAX-ROW-BWD T=
OP-ROPE        ADJ-BOP OP-ROPE-BWD        T=
OP-GELU        ADJ-BWD-OP? TTRUE
\ add / matmul / reshape adjoints reuse existing ops -> no dedicated bwd op-kind
OP-ADD         ADJ-BOP -1 T=
OP-MATMUL      ADJ-BOP -1 T=
OP-RESHAPE     ADJ-BOP -1 T=
OP-ADD         ADJ-BWD-OP? TFALSE

\ ---- the op-registry vjp field now points at the adjoint id (was 0) ----------
OP-GELU        OPR-VJP ADJ-GELU    T=
OP-SOFTMAX-ROW OPR-VJP ADJ-SOFTMAX T=
OP-MATMUL      OPR-VJP ADJ-MATMUL  T=
OP-CAST        OPR-VJP ADJ-NONE    T=       \ non-differentiable stays 0

\ ---- v1-boundary reason text (named, never silent) --------------------------
OP-SLICE  ADJ-UNSUP$ s" slice adjoint (pad-scatter) unimplemented (v1)" T$=
OP-GATHER ADJ-UNSUP$ s" gather adjoint (scatter-add) unimplemented (v1)" T$=
OP-CAST   ADJ-UNSUP$ s" cast has no adjoint (non-differentiable)" T$=

\ ---- fail closed ------------------------------------------------------------
' TRY-ADJ-KIND     E-ADJ-KIND  TTHROWS
' TRY-ADJ-SAVEBAD  E-ADJ-SAVE  TTHROWS
' TRY-ADJ-UNSUP-OK E-ADJ-UNSUP TTHROWS

T-REPORT

end-package

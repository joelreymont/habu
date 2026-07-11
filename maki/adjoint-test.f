\ maki/adjoint-test.f - checked tests for the model-op adjoint registry (cad-9a).
\ Adjoint id, save-need, gradient mask, v1-supported flag, the dedicated backward
\ op-kind mapping, the op-registry vjp wiring, and every fail-closed lookup path.

require lib/test.f
require lib/string.f
require test/checker-assert.f
require maki/adjoint.f

package MAKI

\ ---- fail-closed probes (top level cannot push quotations) ------------------
\ the op-kind out-of-range throw (E-ADJ-KIND) is unrepresentable: ADJ-ID takes an
\ `opkind`, so a raw code is a CHECKER reject (pinned below), never a runtime throw.
: TRY-ADJ-SAVEBAD ( -- ) SAVE-N ADJ-SAVE-NAME 2drop ;     \ save tag out of range
: TRY-ADJ-UNSUP-OK ( -- ) MAKI-OPKIND:GELU ADJ-UNSUP$ 2drop ;      \ reason from a supported op

T-RESET

\ ---- adjoint ids -----------------------------------------------------------
MAKI-OPKIND:GELU        ADJ-ID ADJ-GELU     T=
MAKI-OPKIND:SILU        ADJ-ID ADJ-SILU     T=
MAKI-OPKIND:RELU        ADJ-ID ADJ-RELU     T=
MAKI-OPKIND:ADD         ADJ-ID ADJ-COPY     T=
MAKI-OPKIND:RESIDUAL-ADD ADJ-ID ADJ-COPY    T=
MAKI-OPKIND:MUL         ADJ-ID ADJ-MUL      T=
MAKI-OPKIND:MATMUL      ADJ-ID ADJ-MATMUL   T=
MAKI-OPKIND:SOFTMAX-ROW ADJ-ID ADJ-SOFTMAX  T=
MAKI-OPKIND:RMSNORM     ADJ-ID ADJ-RMSNORM  T=
MAKI-OPKIND:ROPE        ADJ-ID ADJ-ROPE     T=
MAKI-OPKIND:RESHAPE     ADJ-ID ADJ-RESHAPE  T=
MAKI-OPKIND:CONCAT      ADJ-ID ADJ-CONCAT   T=
MAKI-OPKIND:CAST        ADJ-ID ADJ-NONE     T=
\ cad-9e reduce/scatter adjoint ids
MAKI-OPKIND:BIAS        ADJ-ID ADJ-BIAS     T=
MAKI-OPKIND:SCALE       ADJ-ID ADJ-SCALE    T=
MAKI-OPKIND:LINEAR      ADJ-ID ADJ-LINEAR   T=
MAKI-OPKIND:SLICE       ADJ-ID ADJ-SLICE    T=
MAKI-OPKIND:GATHER      ADJ-ID ADJ-GATHER   T=

\ ---- differentiability (cast has no adjoint; backward ops without a second-order
\ row have none; gelu-bwd carries the second-order pilot row) -------------------
MAKI-OPKIND:GELU        ADJ-HAS? TTRUE
MAKI-OPKIND:CAST        ADJ-HAS? TFALSE
MAKI-OPKIND:GELU-BWD    ADJ-HAS? TTRUE            \ second-order pilot (grad-of-grad)
MAKI-OPKIND:GELU-BWD2   ADJ-HAS? TFALSE           \ third order stays fail-closed
MAKI-OPKIND:RELU-BWD    ADJ-HAS? TFALSE           \ remaining *-BWD kinds: no row yet
MAKI-OPKIND:SOFTMAX-ROW-BWD ADJ-HAS? TFALSE
MAKI-OPKIND:ROWSUM-BWD  ADJ-HAS? TFALSE           \ reduce/scatter backward ops: no second-order
MAKI-OPKIND:PAD-SCATTER ADJ-HAS? TFALSE

\ ---- the second-order gelu-bwd row ------------------------------------------
MAKI-OPKIND:GELU-BWD    ADJ-ID   ADJ-GELU-BWD  T=
MAKI-OPKIND:GELU-BWD    ADJ-SAVE SAVE-INPUT    T=   \ reads its own operands (dz, x)
MAKI-OPKIND:GELU-BWD    ADJ-SUP? TTRUE
MAKI-OPKIND:GELU-BWD    0 ADJ-GRAD-IN? TTRUE        \ d-dz
MAKI-OPKIND:GELU-BWD    1 ADJ-GRAD-IN? TTRUE        \ d-x
MAKI-OPKIND:GELU-BWD    ADJ-BOP OP-GELU-BWD2  T=
MAKI-OPKIND:GELU-BWD    ADJ-BWD-OP? TTRUE

\ ---- save-need: gelu needs the INPUT, softmax the OUTPUT, add neither ---------
MAKI-OPKIND:GELU        ADJ-SAVE SAVE-INPUT  T=
MAKI-OPKIND:RELU        ADJ-SAVE SAVE-INPUT  T=
MAKI-OPKIND:MATMUL      ADJ-SAVE SAVE-INPUT  T=
MAKI-OPKIND:SOFTMAX-ROW ADJ-SAVE SAVE-OUTPUT T=
MAKI-OPKIND:ADD         ADJ-SAVE SAVE-NONE   T=
MAKI-OPKIND:ROPE        ADJ-SAVE SAVE-NONE   T=
MAKI-OPKIND:RESHAPE     ADJ-SAVE SAVE-NONE   T=
\ cad-9e: scale/linear save inputs; bias needs nothing saved (rowsum reads only ct)
MAKI-OPKIND:SCALE       ADJ-SAVE SAVE-INPUT  T=
MAKI-OPKIND:LINEAR      ADJ-SAVE SAVE-INPUT  T=
MAKI-OPKIND:BIAS        ADJ-SAVE SAVE-NONE   T=
MAKI-OPKIND:SOFTMAX-ROW ADJ-SAVE ADJ-SAVE-NAME s" output" T$=
MAKI-OPKIND:GELU        ADJ-SAVE ADJ-SAVE-NAME s" input"  T$=

\ ---- gradient mask: unary ops -> input 0 only; binary -> both ----------------
MAKI-OPKIND:GELU   0 ADJ-GRAD-IN? TTRUE
MAKI-OPKIND:GELU   1 ADJ-GRAD-IN? TFALSE
MAKI-OPKIND:ADD    0 ADJ-GRAD-IN? TTRUE
MAKI-OPKIND:ADD    1 ADJ-GRAD-IN? TTRUE
MAKI-OPKIND:MATMUL 0 ADJ-GRAD-IN? TTRUE
MAKI-OPKIND:MATMUL 1 ADJ-GRAD-IN? TTRUE
\ rope's cos/sin (inputs 1,2) are constants -> no gradient
MAKI-OPKIND:ROPE   0 ADJ-GRAD-IN? TTRUE
MAKI-OPKIND:ROPE   1 ADJ-GRAD-IN? TFALSE
MAKI-OPKIND:ROPE   2 ADJ-GRAD-IN? TFALSE
\ cad-9e: bias/linear grad every operand; slice input-0 only; gather's index no grad
MAKI-OPKIND:BIAS   0 ADJ-GRAD-IN? TTRUE
MAKI-OPKIND:BIAS   1 ADJ-GRAD-IN? TTRUE
MAKI-OPKIND:LINEAR 2 ADJ-GRAD-IN? TTRUE
MAKI-OPKIND:SLICE  0 ADJ-GRAD-IN? TTRUE
MAKI-OPKIND:GATHER 0 ADJ-GRAD-IN? TTRUE
MAKI-OPKIND:GATHER 1 ADJ-GRAD-IN? TFALSE

\ ---- v1 supported flag: cad-9e unlocked the reduce/scatter adjoints ----------
MAKI-OPKIND:GELU    ADJ-SUP? TTRUE
MAKI-OPKIND:MATMUL  ADJ-SUP? TTRUE
MAKI-OPKIND:CONCAT  ADJ-SUP? TTRUE
MAKI-OPKIND:RESHAPE ADJ-SUP? TTRUE
MAKI-OPKIND:SLICE   ADJ-SUP? TTRUE
MAKI-OPKIND:GATHER  ADJ-SUP? TTRUE
MAKI-OPKIND:BIAS    ADJ-SUP? TTRUE
MAKI-OPKIND:SCALE   ADJ-SUP? TTRUE
MAKI-OPKIND:LINEAR  ADJ-SUP? TTRUE
MAKI-OPKIND:CAST    ADJ-SUP? TFALSE          \ only cast stays non-differentiable

\ ---- dedicated backward op-kind mapping -------------------------------------
MAKI-OPKIND:GELU        ADJ-BOP OP-GELU-BWD        T=
MAKI-OPKIND:SILU        ADJ-BOP OP-SILU-BWD        T=
MAKI-OPKIND:RELU        ADJ-BOP OP-RELU-BWD        T=
MAKI-OPKIND:SOFTMAX-ROW ADJ-BOP OP-SOFTMAX-ROW-BWD T=
MAKI-OPKIND:ROPE        ADJ-BOP OP-ROPE-BWD        T=
MAKI-OPKIND:GELU        ADJ-BWD-OP? TTRUE
\ add / matmul / reshape adjoints reuse existing ops -> no dedicated bwd op-kind
MAKI-OPKIND:ADD         ADJ-BOP -1 T=
MAKI-OPKIND:MATMUL      ADJ-BOP -1 T=
MAKI-OPKIND:RESHAPE     ADJ-BOP -1 T=
MAKI-OPKIND:ADD         ADJ-BWD-OP? TFALSE

\ ---- the op-registry vjp field now points at the adjoint id (was 0) ----------
MAKI-OPKIND:GELU        OPR-VJP ADJ-GELU    T=
MAKI-OPKIND:SOFTMAX-ROW OPR-VJP ADJ-SOFTMAX T=
MAKI-OPKIND:MATMUL      OPR-VJP ADJ-MATMUL  T=
MAKI-OPKIND:CAST        OPR-VJP ADJ-NONE    T=       \ non-differentiable stays 0

\ ---- v1-boundary reason text: only cast remains unsupported (named, never silent) --
MAKI-OPKIND:CAST   ADJ-UNSUP$ s" cast has no adjoint (non-differentiable)" T$=

\ ---- fail closed ------------------------------------------------------------
' TRY-ADJ-SAVEBAD  E-ADJ-SAVE  TTHROWS
' TRY-ADJ-UNSUP-OK E-ADJ-UNSUP TTHROWS

\ op-kind out of range is a CHECKER reject now (a raw code cannot index the adjoint
\ table); positive control pins the well-typed accessor.
s" ATX-KIND-OK ( opkind -- n ) ADJ-ID"  CHECK-QUIET-CANDIDATE! -1 T=
s" ATX-KIND-N  ( n -- n ) ADJ-ID"        CHECK-QUIET-CANDIDATE!  0 T=

T-REPORT

end-package

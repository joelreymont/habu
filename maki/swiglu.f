\ maki/swiglu.f - SwiGLU, the LLaMA-family gated-MLP activation + its closed-form VJP.
\
\ swiglu(gate, up) = silu(gate) * up ; silu(x) = x*sigmoid(x) (maki/silu.f). A FUSED
\ arity-2 elementwise op: one pass reads gate and up and writes silu(gate)*up - a
\ dedicated op-kind (not a silu;mul pair) because it is the device-kernel target on
\ the bandwidth-bound decode path (dot habu-epic-gb10-uma). Both operands are DATA
\ (same shape); there are no attrs. Reference is SWIGLU-F, bound in the op registry;
\ the executor maps it over elements (EX-EW2, the mul precedent).
\
\ VJP (closed form, decomposes into existing COMPLETE ops - no dedicated *-BWD op).
\ With s = silu(gate) and s'(x) = sigmoid(x)*(1 + x*(1 - sigmoid(x))) (maki/silu.f
\ SILU-GRAD, the same derivative OP-SILU-BWD carries):
\   d_up   = dy * s              = OP-SILU(gate) then OP-MUL(dy, .)
\   d_gate = dy * up * s'(gate)  = OP-SILU-BWD(dy*up, gate)
\ so maki/backward.f BW-STEP-SWIGLU emits OP-MUL / OP-SILU / OP-SILU-BWD (all already
\ registry-complete) - the bcast-mul precedent (no new backward op-kind). SWIGLU-DGATE
\ / SWIGLU-DUP are those closed forms as scalar words: the gradcheck golden the host and
\ device tests compare central differences against. Reuse SILU-F / SILU-BWD (both public)
\ rather than re-deriving sigmoid. Needs maki/silu.f. maki -> habu only (no error codes).

require maki/silu.f

package MAKI
public

\ forward reference (gate up -- y): silu(gate) * up. Stack order matches EX-EW2-EL
\ (operand 0 = gate below operand 1 = up), so it binds as the mul reference does.
: SWIGLU-F ( r r -- r ) {: g:r u:r :}  g SILU-F u f* ;

\ closed-form VJP (the gradcheck golden; NOT the production path, which decomposes
\ into OP-SILU/OP-MUL/OP-SILU-BWD in maki/backward.f). SILU-BWD(dz,x) = dz*silu'(x),
\ so d_gate = SILU-BWD(dy*up, gate); d_up = dy*silu(gate).
: SWIGLU-DGATE ( r r r -- r ) {: dy:r g:r u:r :}  dy u f*  g SILU-BWD ;
: SWIGLU-DUP   ( r r -- r )   {: dy:r g:r :}       g SILU-F  dy f* ;

;package

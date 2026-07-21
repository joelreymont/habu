\ maki/prec-attr.f - per-op COMPUTE-PRECISION attribute for GEMM-class ops
\ (docs/model-unified.md "Per-op compute precision"; the approved branch of the
\ ratified numerics policy, dot habu-per-op-precision).
\
\ A GEMM-class op (matmul / linear / equation) carries the tensor-core input dtype it
\ COMPUTES in: tf32 (default), fp16, or bf16. The tag is DECLARED at the authoring
\ surface - the equation/MODEL: layer inherits it from a per-workload default
\ (CPREC-DEFAULT!) - and RESOLVED into each node's attrs cell at capture, so precision
\ is per op and never a global flip: the device-lowering stage reads a node's tag and
\ sets the emitter knob MMA-DTYPE (lib/ptx/cg-mma.f: 0=tf32, 1=fp16, 2=bf16) for that
\ op alone. This file OWNS the attribute plumbing (tags, packing, the GEMM-class guard,
\ the workload default) + the reduced-precision GOLDEN tolerance contract; it does NOT
\ emit device code (a later stage reads the tag) and does NOT touch the gate-licensed
\ tolerance rows (maki/precision.f), which stay f32/tf32-only.
\
\ ATTRS-CELL COMPOSITION (plan-ops.f MV-PACK precedent). A GEMM node's attrs cell packs
\ the precision in a HIGH field over the op's existing LOW payload:
\   bits [31: 0]  payload   equation = its spec-registry slot (maki/spec.f EQ-SLOT);
\                           matmul / linear = 0 (they carry no other attr)
\   bits [33:32]  cprec     CPREC-TF32..CPREC-BF16 (0..2)
\ CPREC-TF32 is 0, so a tf32-default node's cell is BYTE-IDENTICAL to the pre-attribute
\ encoding (0 for matmul/linear, the bare slot for equation) - only an fp16/bf16 opt-in
\ sets the high bits. The executor reads the payload through CPREC-PAYLOAD@ so the slot
\ survives an fp16/bf16 tag. The 32-bit payload field far exceeds EQ-CAP (32 slots), so
\ the slot can never reach the precision field.
\
\ FAIL CLOSED: an out-of-range precision tag is E-CPREC-TAG; tagging a non-GEMM-class op
\ (only matmul/linear/equation are GEMM-class) is E-CPREC-OP. maki -> habu only;
\ prec-attr owns -5430..-5433.

require lib/prelude.f                \ true / false: the GEMM-class predicate arms
require lib/float.f                  \ POW10: render the golden-tolerance rows without float memory
require maki/op-kind.f               \ the opkind family: the GEMM-class guard MATCHes over it

-5430 constant E-CPREC-TAG    \ compute-precision tag out of range (not tf32/fp16/bf16)
-5431 constant E-CPREC-OP     \ precision tagged onto a non-GEMM-class op

package MAKI
public

\ ---- compute-precision tags (align 1:1 with the MMA-DTYPE knob, lib/ptx/cg-mma.f:195) --
0 constant CPREC-TF32          \ f32-in, tf32 mma, f32 accumulate - the default
1 constant CPREC-FP16          \ f16 operands, f32 accumulate
2 constant CPREC-BF16          \ bf16 operands, f32 accumulate
3 constant CPREC-N             \ range bound

: CPREC-NAME ( n -- ptr u8 n )
   case
      CPREC-TF32 of s" tf32" endof
      CPREC-FP16 of s" fp16" endof
      CPREC-BF16 of s" bf16" endof
      E-CPREC-TAG throw
   endcase ;

private
: CPREC-CK ( n -- n )  dup 0 < over CPREC-N >= or if E-CPREC-TAG throw then ;

\ ---- attrs-cell pack / unpack (payload low, precision high; see file head) --------
32        constant CPREC-SH      \ precision field shift (payload occupies the low 32 bits)
$3        constant CPREC-MASK    \ 2-bit precision field mask
$FFFFFFFF constant CPREC-PMASK   \ 32-bit payload field mask (equation slot / 0)

public
\ pack a GEMM op's payload (equation slot, or 0) with its compute precision -> attrs cell.
: CPREC-PACK ( n n -- n ) {: pay:n cp:n :}
   cp CPREC-CK drop
   pay  cp CPREC-SH lshift  or ;
: CPREC@ ( n -- n )          CPREC-SH rshift CPREC-MASK and ;   \ attr -> compute precision
: CPREC-PAYLOAD@ ( n -- n )  CPREC-PMASK and ;                 \ attr -> op payload (equation slot / 0)

\ ---- GEMM-class guard: compute precision applies to matmul / linear / equation only --
\ seg-attn is a FUSED token-mixer whose internal GEMMs are a separate precision concern
\ (out of scope here), so it is NOT GEMM-class for this attribute.
: CPREC-GEMM? ( opkind -- bool )
   MATCH opkind
      matmul          OF true  ENDOF
      linear          OF true  ENDOF
      equation        OF true  ENDOF
      add             OF false ENDOF
      mul             OF false ENDOF
      scale           OF false ENDOF
      bias            OF false ENDOF
      relu            OF false ENDOF
      gelu            OF false ENDOF
      layernorm       OF false ENDOF
      rmsnorm         OF false ENDOF
      softmax-row     OF false ENDOF
      residual-add    OF false ENDOF
      cast            OF false ENDOF
      silu            OF false ENDOF
      rope            OF false ENDOF
      reshape         OF false ENDOF
      transpose       OF false ENDOF
      slice           OF false ENDOF
      concat          OF false ENDOF
      gather          OF false ENDOF
      relu-bwd        OF false ENDOF
      gelu-bwd        OF false ENDOF
      silu-bwd        OF false ENDOF
      layernorm-bwd   OF false ENDOF
      rmsnorm-bwd     OF false ENDOF
      softmax-row-bwd OF false ENDOF
      rope-bwd        OF false ENDOF
      rowsum-bwd      OF false ENDOF
      fullsum-dot-bwd OF false ENDOF
      pad-scatter     OF false ENDOF
      scatter-add     OF false ENDOF
      gelu-bwd2       OF false ENDOF
      seg-attn        OF false ENDOF
      seg-attn-bwd    OF false ENDOF
      bcast-mul       OF false ENDOF
      dropout         OF false ENDOF
      dropout-bwd     OF false ENDOF
      swiglu          OF false ENDOF
   ;MATCH ;

: CPREC-GEMM-CK ( opkind -- )  CPREC-GEMM? 0= if E-CPREC-OP throw then ;

\ stamp a GEMM node's precision onto its attrs (the single guarded tagging entry): the
\ op must be GEMM-class (else E-CPREC-OP) and the tag in range (else E-CPREC-TAG).
: CPREC-TAG ( n n opkind -- n )   \ payload precision opkind -- attr
   CPREC-GEMM-CK
   CPREC-PACK ;

\ ---- workload default (per-workload; RESOLVED into per-node attrs at capture) --------
\ CPREC-DEFAULT holds the precision each GEMM composer (maki/plan-ops.f) stamps onto its
\ node. It seeds per-node tags at capture; the device-lowering stage reads only the
\ per-node tag, never this cell - so it is a workload default, not a global codegen flip.
\ It persists (like maki/precision.f PREC-REQ) until reset; the default is tf32, so a
\ workload that never sets it captures every GEMM node byte-identical to the pre-attribute
\ encoding. CPREC-DEFAULT-RESET restores tf32.
private
variable CPREC-REQ
public
: CPREC-DEFAULT! ( n -- )            CPREC-CK CPREC-REQ ! ;   \ set the workload compute precision
: CPREC-DEFAULT@ ( -- n )            CPREC-REQ @ ;
: CPREC-DEFAULT-RESET ( -- )         CPREC-TF32 CPREC-REQ ! ;
CPREC-DEFAULT-RESET

\ ---- per-op precision override (the MODEL: grammar's MATMUL:FP16 source) ---------------
\ A `<OP>:<PREC>` grammar token (maki/cad.f) declares ONE GEMM node's compute precision
\ explicitly, overriding the workload default for that node ALONE. The parser threads it
\ positionally: it emits a CPREC-NEXT! before each tagged composer in the compiled body, and
\ every GEMM composer (plan-ops.f) resolves its precision through CPREC-NEXT@ - the override
\ when set (CONSUMED, so it binds exactly one node), else CPREC-DEFAULT@. So two differently
\ tagged ops in one body each get their own tag and an untagged sibling keeps the default.
\ CPREC-NEXT-RESET clears a pending override at each capture (cad.f CAP-BEGIN) so a mid-parse
\ throw never leaks it into the next MODEL:.
private
-1 constant CPREC-OVR-NONE           \ "no pending override" (outside the 0..CPREC-N tag range)
variable CPREC-OVR
public
: CPREC-NEXT! ( n -- )        CPREC-CK CPREC-OVR ! ;     \ bind the next GEMM node's precision
: CPREC-NEXT-RESET ( -- )     CPREC-OVR-NONE CPREC-OVR ! ;
: CPREC-NEXT@ ( -- n )        \ pending override (consumed) else the workload default
   CPREC-OVR @ dup CPREC-OVR-NONE = if drop CPREC-DEFAULT@ exit then
   CPREC-NEXT-RESET ;
CPREC-NEXT-RESET

\ ---- reduced-precision GOLDEN tolerance contract (docs/model-unified.md) --------------
\ The host executor is f32/f64 EXACT, so a reduced-precision GEMM golden is compared to
\ the exact reference under a tolerance DERIVED from the compute dtype's mantissa - never
\ the zero-tolerance integer-fill argument on real data, never a bare epsilon.
\
\ Error model: each input is rounded f32 -> the reduced type before the tensor-core
\ product (unit roundoff u = 2^-(p+1), p = stored mantissa bits); the accumulator stays
\ f32, so accumulation adds no term above the f32 floor. The dot-product relative error is
\ therefore bounded by the input rounding, a small constant times u.
\   tf32 / fp16: p = 10 -> u = 2^-11 ~= 4.9e-4. fp16 SHARES tf32's 10-bit significand, so
\     its bound EQUALS the MEASURED tf32 GEMM row (maki/precision.f: rtol 2e-3 ~= 4u, ~2.5x
\     over the measured ~8e-4 on this device class).
\   bf16: p = 7 -> u = 2^-8 ~= 3.9e-3, 8x tf32's unit roundoff, so the bound scales 8x -> 2e-2.
\ atol is the f32 accumulator floor 1e-6 for every dtype (accumulation is f32). These are
\ the ANALYTICAL authoring bounds a reduced-precision golden test compares under; the
\ gate-LICENSED rows (maki/precision.f) are untouched - an fp16/bf16 licensed row lands with
\ a DEVICE measurement (the MMA lane), not here.
private
: CPREC-F ( n n -- r ) {: m:n e:n :}  m s>f  e POW10  f* ;   \ mant * 10^exp, no float memory
public
: CPREC-GOLD-ATOL ( n -- r )  CPREC-CK drop  1 -6 CPREC-F ;   \ cprec -- atol: f32 accumulator floor, all dtypes
: CPREC-GOLD-RTOL ( n -- r )  \ cprec -- rtol
   CPREC-CK
   dup CPREC-BF16 = if drop 2 -2 CPREC-F exit then   \ bf16: 8x unit roundoff
   drop 2 -3 CPREC-F ;                                \ tf32 / fp16: 10-bit significand

;package

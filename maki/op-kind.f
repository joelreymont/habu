\ maki/op-kind.f - the canonical model op-kind registry (CAD-PLAN section 4.2).
\
\ The single op set shared by the Phase-0 MODEL: parser (maki/cad.f) and the
\ descriptor-mode planning vocabulary (maki/tensor-value.f). One enum, defined
\ once, so capture and planners agree on op identity - the registry is the single
\ extension point (planners never learn op names). Adding an op is one entry
\ here plus its scalar reference + VJP in the op registry (cad-1). maki -> habu
\ only; needs no library beyond core (named constants, no runtime logic).
\
\ Backward op-kinds (OP-*-BWD, cad-9) extend the SAME enum: the model-IR reverse
\ transform (maki/backward.f) emits them as ordinary nodes so they enter the same
\ fusion/traffic/memory planners. They are SYNTHESIZED only - never parseable in a
\ MODEL: body (the maki/cad.f OP-KIND token map does not name them); matmul/movement
\ adjoints reuse the existing ops (transpose+matmul, reshape/transpose/slice), so
\ only the elementwise/reduction adjoints that are genuinely new ops appear here.

package MAKI
public

0  constant OP-ADD
1  constant OP-MUL
2  constant OP-SCALE
3  constant OP-BIAS
4  constant OP-RELU
5  constant OP-GELU
6  constant OP-LAYERNORM
7  constant OP-RMSNORM
8  constant OP-SOFTMAX-ROW
9  constant OP-MATMUL
10 constant OP-LINEAR
11 constant OP-RESIDUAL-ADD
12 constant OP-CAST
13 constant OP-SILU            \ x * sigmoid(x) (reference: maki/silu.f)
14 constant OP-ROPE            \ rotary pair rotation (reference: maki/rope.f)
\ ---- movement ops (CLASS-MOVEMENT: exact layout rewrites, no compute; 6.3) ---
15 constant OP-RESHAPE         \ same elements, new RxC   (reference: maki/move.f)
16 constant OP-TRANSPOSE       \ RxC -> CxR
17 constant OP-SLICE           \ row-range copy
18 constant OP-CONCAT          \ row-wise append
19 constant OP-GATHER          \ row indexed select
\ ---- backward op-kinds (cad-9: synthesized by maki/backward.f, not MODEL:-typed) --
20 constant OP-RELU-BWD          \ dz gated by sign(x)              (ref: RELU-BWD)
21 constant OP-GELU-BWD          \ dz * gelu'(x)                    (ref: GELU-BWD)
22 constant OP-SILU-BWD          \ dz * silu'(x)                    (ref: SILU-BWD)
23 constant OP-LAYERNORM-BWD     \ row-coupled layernorm VJP        (ref: LN-BWD)
24 constant OP-RMSNORM-BWD       \ row-coupled rmsnorm VJP          (ref: RMS-BWD)
25 constant OP-SOFTMAX-ROW-BWD   \ softmax VJP over the OUTPUT row  (ref: SM-BWD)
26 constant OP-ROPE-BWD          \ rotate cotangent by -angle       (ref: ROPE-BWD)
27 constant OP-N               \ op-kind range bound

end-package

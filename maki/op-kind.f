\ maki/op-kind.f - the canonical model op-kind registry (CAD-PLAN section 4.2).
\
\ The single op set shared by the Phase-0 MODEL: parser (maki/cad.f) and the
\ descriptor-mode planning vocabulary (maki/tensor-value.f). One enum, defined
\ once, so capture and planners agree on op identity - the registry is the single
\ extension point (planners never learn op names). Adding an op is one entry
\ here plus its scalar reference + VJP in the op registry (cad-1). maki -> habu
\ only; needs no library beyond core (named constants, no runtime logic).

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
13 constant OP-N               \ op-kind range bound

end-package

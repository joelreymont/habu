\ gemm-checked-test.f - the tiled GEMM data-flow expressed as a CHECKED KERNEL: body.
\
\ This proves the real matrix-shaped checked GEMM entry from lib/ptx/cg-matmul.f:
\ A[M,K] * B[K,N] -> C[M,N]. The PTX instruction chunks remain target primitives,
\ but the public entry is a `KERNEL:` body with a typed mmctx phase token and a
\ typed mmacc token. The negative (a non-stack-neutral K-loop body) is gated by
\ lib/ptx/gemm-checked-neg-test.f.

require lib/ptx/test-prelude.f
require lib/ptx/cg-matmul.f

T-RESET

256 %BLOCK

\ Clean load past this point is the positive proof: MM-CHECKED from cg-matmul.f
\ certifies as a matrix-shaped checked KERNEL body.

T-REPORT

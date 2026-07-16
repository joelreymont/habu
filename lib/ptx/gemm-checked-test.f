\ gemm-checked-test.f - the PRODUCTION tiled GEMM certifies as a checked KERNEL: body.
\
\ Stage 3 (dot habu-re-express-tiled-9cc4a73a): lib/ptx/cg-matmul.f MM-CHECKED is
\ no longer a wrapper over a bespoke trusted phase shim - its body is the typed
\ tile-pipe composition (MM-BEGIN / MM-K-LOOP / MM-STORE over PIPE-SETUP /
\ PIPE-ACC-ZERO / PIPE-LOOP / PIPE-STORE with the checked RB-TILE compute), and
\ EMIT-MATMUL ships exactly that certified kernel. A clean load of this file IS
\ the certification proof for the production emitter: a checker reject would
\ emit a diagnostic and fail the load. The negatives (inline K-loop, missing
\ MM-STORE, swapped A/B operands) are gated by lib/ptx/gemm-checked-neg-test.f;
\ the byte-identity and pipeline-shape regressions live in lib/ptx/tile-pipe-test.f.

require lib/ptx/test-prelude.f
require lib/ptx/cg-matmul.f

T-RESET

256 %BLOCK

\ Clean load past this point is the positive proof: MM-CHECKED from cg-matmul.f
\ certifies as the production matrix-shaped checked KERNEL body.

T-REPORT

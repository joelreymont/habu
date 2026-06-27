\ matmul-cg.f - emit the tiled SGEMM kernel MM to PTX (see lib/ptx/cg-matmul.f for
\ the full design notes: 16x16 tiles, shared staging, the runtime K-loop, the ~77
\ GFLOP/s baseline vs Triton ~1474, and the path to close the gap). Load after
\ src/arch/ptx/emit.f + lib/ptx/cg.f + lib/ptx/cg-matmul.f; emits to stdout.
EMIT-MATMUL

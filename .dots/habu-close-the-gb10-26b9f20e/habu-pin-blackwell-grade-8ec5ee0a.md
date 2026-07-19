---
title: Pin Blackwell-grade ptxas in the toolchain
status: closed
priority: 1
issue-type: task
created-at: "\"2026-07-19T13:54:47.072257+02:00\""
closed-at: "2026-07-19T14:37:41.810667+02:00"
---

Root cause of the entire tf32 scheduling gap (found 2026-07-19 by the orchestrator, closing the contradiction chain from rounds 4-7): system CUDA 13.0.2's ptxas has an immature sm_121 scheduler - it issues every HMMA at a fixed 16-cycle yield-set interval (the 40 NOPs are its stall-field-overflow encoding) - while ptxas 13.3.33 (the cuda_nvcc-linux-sbsa-13.3.33 archive Triton's build cache fetched to ~/.triton/nvidia/nvcc-blackwell/) produces the resident-warp schedule from our UNMODIFIED PTX: zero NOPs, ~28% fewer steady-window stall cycles, same 128 regs, zero spills, and all 138 mma-gemm-check rows element-exact. Every prior falsification (register renaming, issue order, tile-granular hoisting, reqntid, register budgets) was correct - the discriminator was the assembler binary. INTERIM (label as such): PTXAS env override pointing at the 13.3 archive. LONG-TERM FIX this dot owns: (1) provision the exact nvcc archive into Habu's own tool store (not Triton's cache - another project's cache dir can vanish), sha256-pinned, documented in docs/gb10 recipe + bootstrap docs; (2) teach lib/ptx/toolchain.f version-aware resolution: probe order PTXAS-env -> Habu tool store -> system CUDA, READ the ptxas version, and for the GB10 profile require >= the pinned version with a loud named diagnostic (never silent) when only an older assembler resolves - the scheduling quality difference is ~27% of GEMM throughput, too large to lose silently; (3) re-measure the full head-to-head matrix (tf32/fp16/bf16, all four shapes, doc protocol) under the pinned assembler and write the eval-triton.md round recording the corrected verdict: the rounds 4-7 'compiler-scheduling-class' conclusion is amended - mechanism analysis stands, remedy was an assembler upgrade.

---
title: "Infer: NVFP4 quantized decode"
status: open
priority: 2
issue-type: task
created-at: "2026-07-21T15:59:04.567006+02:00"
blocks:
  - habu-infer-end-to-20fa7684
---

Phase 4, THE throughput lever: decode on the GB10 is bandwidth-bound (~273 GB/s shared), so bf16 GPT-2-class runs at a memory ceiling and a 7B-class model caps around ~20 tok/s regardless of compute - NVFP4 weights (proven working on this chip during the Triton bring-up: tl.dot_scaled ran) cut weight traffic ~4x. Scope: NVFP4 weight quantization offline (from the loaded fp weights, scales per block, committed quantization recipe with error measurement vs fp16 logits), a quantized GEMM path for the MLP+projection matmuls (the MMA harness discipline - element-tolerance goldens instead of exact, tolerance DERIVED from measured quantization error not defaulted), and the decode loop wired to choose the quantized path. KV cache stays fp16/bf16 in this dot (KV quantization is its own later dot). Acceptance: perplexity/logit-error budget met on the reference prompts, measured tokens/sec vs the bf16 baseline recorded on the quiet box.

Review incorporation 2026-07-21 (docs/inference-engine-plan.md sect 3.5 + M8) - SCOPE NARROWED to the QUANTIZATION PACKER + QUALITY CONTRACT (the execution paths split into their own dots: habu-infer-batch-one GEMV, habu-infer-small-batch GEMM, dispatch/measurement). Fourfold byte reduction is NOT a fourfold throughput promise - batch-one decode behaves like GEMV, not a well-filled GEMM. This dot: offline packer converting bf16/fp16 in bounded chunks directly into final kernel layout (never both full models resident), scales/metadata emitted once, calibration method + checkpoint checksum + quality results (logit error, perplexity delta, task suite, fixed continuations) + kernel-compatibility recorded in the pack. No default tolerances without measured justification.

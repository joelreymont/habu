---
title: Emit GPT-2 tensor kernels
status: open
priority: 1
issue-type: task
created-at: "2026-08-03T09:38:58.825361+02:00"
---

Why: resident GPT-2 weights have no device compute path. Result: one checked Forth PTX source emits the fixed GPT-2 tensor entries needed by decode: token-plus-position embedding, affine LayerNorm, row-major linear with bias, tied unembed without bias, in-place GELU, and residual add. ABIs are direct u64 buffers plus u32 extents; no ABI version, descriptor table, registry, generic runtime, committed binary, host fallback, or second formulas. Extents are runtime parameters and all launches cover tails. Owner: GPT2 tensor PTX emission only. Production red: the entry symbols do not exist. Acceptance: the emitted single module assembles for the active target and every entry matches an independent F32 host golden on boundary and tail extents through real CUDA; the GPT-2 768/2304/3072/50257 shapes execute. Smallest owning check: focused GPT-2 tensor-kernel device test.

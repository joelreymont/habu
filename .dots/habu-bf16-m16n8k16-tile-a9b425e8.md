---
title: bf16 m16n8k16 tile variant
status: closed
priority: 1
issue-type: task
created-at: "2026-07-19T13:01:36.631542+02:00"
closed-at: "2026-07-19T13:37:28.353585+02:00"
---

Follow-on the fp16 tile closure named, now unblocked by the ratified numerics policy (Joel 2026-07-19: reduced precision allowed where accuracy budget allows) and wanted for nanoGPT training, where bf16 is the conventional mixed-precision dtype (f32 range, 8-bit mantissa - no loss-scaling gymnastics). Extend MMA-DTYPE with a bf16 value: same m16n8k16 shape and fragment maps as fp16 (mma.sync.aligned.m16n8k16.row.col.f32.bf16.bf16.f32), different host pack (F64>BF16 round-to-nearest-even; truncation is NOT acceptable) and cvt on any readback. The zero-tolerance element-exact argument carries with tighter fill bounds: bf16's 8-bit significand represents integers up to 256 exactly, so the existing A in 1..13, B in 1..11 fills remain exactly representable and every f32 partial sum stays under 2^24 - state the adapted argument in the check-file header. Both the k-major and the MMA-BTF16 transposed-Bs B feeds should work unchanged (halves are 2 bytes either way) - verify and extend the guard rather than assuming. Element-exact rows both warp grids + epilogue + both B feeds; tf32 AND fp16 byte-identity when off; fail-closed on unwired combos; timing last and solo per the doc protocol vs Triton bf16 if the referee script exposes it (add a bf16 referee run mirroring the fp16 one), else vs our own fp16. Files: lib/ptx + tools/ptx + docs only - no maki (the per-op precision tagging consumes this later).

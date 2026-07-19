---
title: Shared-memory epilogue for coalesced C stores
status: closed
priority: 1
issue-type: task
created-at: "\"2026-07-19T10:08:34.186356+02:00\""
closed-at: "2026-07-19T10:51:51.442660+02:00"
---

Round-3 lever 1 of the GB10 gap campaign (docs/eval-triton.md round-2 next-lever section, campaign header habu-close-the-gb10-26b9f20e). Habu's MMA tile stores each lane's D fragments straight to global as scattered 4-byte st.global.f32 writes; Triton stages the accumulator tile through shared memory and writes C coalesced. Implement the smem epilogue in lib/ptx/cg-mma.f: after the K loop, lanes write their accumulator fragments to a staging tile in shared memory (reusing the pipeline's smem allocation - it is dead after the last cp.async wait), barrier, then the block writes C rows coalesced (128-byte lines). Element-exact via tools/ptx/mma-gemm-check.f rows for every affected config FIRST; all existing configs must stay byte-identical when the epilogue is off (emit diff); then the doc's exact timing protocol per shape, extending the round tables + perf-rows.tsv. Expect the biggest effect at 512 (compute-light launch where the store fraction is largest).

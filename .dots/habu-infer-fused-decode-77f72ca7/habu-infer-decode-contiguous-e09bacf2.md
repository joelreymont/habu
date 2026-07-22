---
title: "Infer decode: contiguous device kernel"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:41:52.350680+02:00"
blocks:
  - habu-infer-decode-supported-29bebe81
  - habu-infer-decode-online-17d2db72
---

Why this exists:
M3 stage A needs a single-query device kernel over contiguous K and V before page indirection is introduced.

Required result:
emit one supported BF16/FP16 kernel with FP32 dot, online-softmax recurrence, and value accumulation using the geometry contract.

Done when:
synthetic short and long rows agree with the online-softmax oracle; unsupported geometry never launches; kernel is registered in the performance watch table.

Expected touch points: new lib/ptx/cg-decode-contiguous.f, focused device test, perf-watch and FILEMAP rows.
Smallest check: focused host compile test and correctness-only GB10 run.
Prerequisites: supported geometry contract and online softmax oracle.
Owned result: contiguous kernel emitter only.
Claim: unassigned.

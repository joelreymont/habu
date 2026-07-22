---
title: "Infer decode: TMA page-fetch candidate"
status: open
priority: 2
issue-type: task
created-at: "2026-07-22T09:41:52.373314+02:00"
blocks:
  - habu-infer-decode-vector-e5ac69b3
---

Why this exists:
Tensor Memory Accelerator page fetch is only a candidate and must be implemented without assuming it wins.

Required result:
add one TMA transfer variant for supported contiguous physical pages behind the common paged recurrence and geometry contract.

Done when:
it is numerically equivalent to the vector baseline, rejects unsupported alignment or geometry before launch, and exposes transfer-specific counters for the common benchmark.

Expected touch points: new lib/ptx/cg-decode-paged-tma.f, focused device test, perf-watch and FILEMAP rows.
Smallest check: correctness-only GB10 parity run.
Prerequisites: vector-load paged kernel.
Owned result: TMA transfer variant only.
Claim: unassigned.

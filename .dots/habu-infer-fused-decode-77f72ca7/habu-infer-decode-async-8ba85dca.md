---
title: "Infer decode: async-copy candidate"
status: open
priority: 2
issue-type: task
created-at: "2026-07-22T09:41:52.379112+02:00"
blocks:
  - habu-infer-decode-vector-e5ac69b3
---

Why this exists:
viable asynchronous copy support must be compared with vector loads and TMA under the same recurrence rather than dismissed or mixed into one kernel.

Required result:
add one supported asynchronous staging variant with explicit shared-memory and synchronization geometry.

Done when:
parity with the vector baseline, fail-closed unsupported geometry, and common benchmark counters.

Expected touch points: new lib/ptx/cg-decode-paged-async.f, focused device test, perf-watch and FILEMAP rows.
Smallest check: correctness-only GB10 parity run.
Prerequisites: vector-load paged kernel.
Owned result: asynchronous transfer variant only.
Claim: unassigned.

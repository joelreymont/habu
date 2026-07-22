---
title: "Infer ops: eight-hour soak verdict"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T10:07:45.946940+02:00"
blocks:
  - habu-infer-ops-bounded-c2d7a786
  - habu-infer-ops-restart-c97dc2a9
  - habu-infer-quant-dispatch-2a1cc579
---

Why this exists:
The release needs measured evidence that the complete server remains correct, bounded, and stable over sustained use.

Required result:
Run the validated soak workload for eight hours on the DGX Spark and publish canonical results, raw-log digests, allocator invariant history, latency distributions, faults, memory, cancellations, and restart identity.

Done when:
No correctness, ownership, leak, or stability failure occurs; every interval validates; post-run cleanup returns to the declared baseline; any failure produces an explicit no-go verdict rather than a partial pass.

Expected touch points: canonical soak records and the release verdict note.
Smallest check: schema validation and reducer replay over every interval.
Prerequisites: bounded soak runner, restart reproducibility, and quantized performance decision.
Owned result: eight-hour release evidence and verdict only.
Claim: unassigned.

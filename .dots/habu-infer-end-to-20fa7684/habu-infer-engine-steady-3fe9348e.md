---
title: "Infer engine: steady-state decode record"
status: open
priority: 2
issue-type: task
created-at: "2026-07-22T09:41:52.423880+02:00"
blocks:
  - habu-infer-engine-64-02416606
  - habu-infer-m0-benchmark-c50501b7
  - habu-infer-m0-benchmark-67ece165
---

Why this exists:
later scheduling and quantization work needs one reproducible BF16 single-sequence baseline without overstating GPT-2 as product performance.

Required result:
measure warmed steady-state decode through the completed engine under the M0 schema on an idle machine.

Done when:
canonical record includes exact commits, pack/checkpoint digests, prompt/output, run count, median/p95, memory, faults, and explicit GPT-2-oracle label.

Expected touch points: canonical benchmark record and concise note.
Smallest check: M0 schema validation and reducer replay.
Prerequisites: 64-token GPT-2 oracle and M0 benchmark matrix runner.
Owned result: GPT-2 engine baseline data only.
Claim: unassigned.

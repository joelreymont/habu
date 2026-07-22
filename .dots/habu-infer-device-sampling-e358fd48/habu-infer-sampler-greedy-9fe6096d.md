---
title: "Infer sampler: greedy and temperature kernel"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:43:30.692522+02:00"
blocks:
  - habu-infer-sampler-host-be28f359
---

Why this exists:
the minimum device path needs deterministic argmax and temperature scaling/reduction before more complex filters.

Required result:
implement one supported device kernel with the canonical tie rule and numeric semantics.

Done when:
exact identifiers match host greedy and deterministic temperature fixtures; invalid temperature and unsupported geometry reject before launch.

Expected touch points: new lib/ptx/cg-sampling-basic.f, focused device test, perf-watch and FILEMAP rows.
Smallest check: correctness-only GB10 parity.
Prerequisites: host critical-path baseline.
Owned result: greedy and temperature device kernel only.
Claim: unassigned.

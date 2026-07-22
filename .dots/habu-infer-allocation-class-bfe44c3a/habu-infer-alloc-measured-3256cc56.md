---
title: "Infer alloc: measured backing policy"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:43:30.680794+02:00"
blocks:
  - habu-infer-alloc-backing-84051fcd
---

Why this exists:
the capacity planner needs a committed policy selected from data, not intuition.

Required result:
run the allocation-class harness for all valid classes and record the chosen backing and rejected alternatives with measured evidence.

Done when:
every class cites canonical result records and a cleanup/synchronization contract; tied or workload-dependent cases remain conditional; the source-weight result agrees with the landed residency measurement.

Expected touch points: canonical result table and concise docs note.
Smallest check: result schema validation and reducer replay.
Prerequisites: backing microbenchmark.
Owned result: measured policy data only.
Claim: unassigned.

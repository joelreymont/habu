---
title: Upgrade GC from measured workloads
status: open
priority: 2
issue-type: task
created-at: "2026-04-01T22:06:02.388843+02:00"
blocks:
  - habu-define-canonical-workload-ae1f969c
---

Problem: GC policy and root scanning must improve from real clean workload behavior rather than synthetic assumptions. Acceptance: nursery, tenure, remembered-set, and root scanning changes are driven by measured Maxima workloads with memory and GC evidence. Files: src/runtime/gc.zig, src/runtime/heap.zig, tooling output from canonical workloads. Verify: clean workload GC and memory comparisons before and after. Blockers: habu-define-canonical-workload-ae1f969c.

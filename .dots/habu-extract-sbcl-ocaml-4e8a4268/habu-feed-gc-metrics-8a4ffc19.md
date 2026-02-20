---
title: Feed GC metrics into perf loop
status: closed
priority: 2
issue-type: task
created-at: "\"2026-02-20T08:55:19.452340+01:00\""
closed-at: "2026-02-20T09:59:04.529183+01:00"
close-reason: completed
blocks:
  - habu-add-allocation-hot-1f31ac72
---

File: tools/perf-loop:1; cause: perf loop ranks CPU hotspots but not GC debt; fix: include GC parity deltas and rank top GC regressions; why: continuous self-improvement for GC.

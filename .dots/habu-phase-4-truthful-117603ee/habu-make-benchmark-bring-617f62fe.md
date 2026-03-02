---
title: Make benchmark bring-up fail closed
status: open
priority: 1
issue-type: task
created-at: "2026-04-01T22:06:02.333590+02:00"
blocks:
  - habu-prove-interactive-and-5238da2f
---

Problem: benchmark harness can emit timings from dirty or partial Maxima environments. Acceptance: no timing output is emitted unless the clean manifest loads, selected workloads all succeed, and provenance is recorded. Files: bench/maxima_workload.zig:394-401,763-890. Verify: good-run and dirty-run harness tests. Blockers: habu-prove-interactive-and-5238da2f.

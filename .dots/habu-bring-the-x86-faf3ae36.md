---
title: "Bring the x86-64 profiler to arm64's"
status: open
priority: 2
issue-type: task
created-at: "2026-11-26T10:43:16.594180+01:00"
---

Problem: on pulsar the Tender placement load (2.05 s) takes 11.2 s under 1000 prof-rate and records 791 samples, where arm64 records 14901 for the same load; docs/debugging.md:298-320 lists x86-only differences: each signal counts one sample, every sample scans at most PROF-WALK-CELLS, ticks are not sent on to the profiling thread. Acceptance: the profiled load runs near its unprofiled time and its samples match the elapsed ms; x86 weights, scans and routes samples as arm64 does, and the doc's x86 exceptions go. Depends: none. Claim: unassigned.

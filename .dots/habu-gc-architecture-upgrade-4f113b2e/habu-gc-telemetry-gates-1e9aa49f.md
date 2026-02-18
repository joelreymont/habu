---
title: GC telemetry gates
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-02-18T21:50:33.275375+01:00\\\"\""
closed-at: "2026-02-18T22:00:39.614143+01:00"
close-reason: Add GC phase telemetry and bench gates
---

bench/gc.zig:1 and bench/check.zig:1. Cause: no phase-level GC metrics for root/build/copy/finalize and no Maxima workload gate. Fix: add per-phase counters and benchmark JSON gates. Why: prevents blind tuning and regressions.

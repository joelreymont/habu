---
title: Profile maxima hotspots
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-02-17T22:23:04.182924+01:00\\\"\""
closed-at: "2026-02-20T21:12:27.651382+01:00"
close-reason: Add dual-mode maxima hotspot tool and report with reproducible JIT/interpreter deltas
blocks:
  - habu-maxima-load-to-e6d01b9c
---

bench/comprehensive_bench.zig plus maxima workload scripts. Cause: no hotspot ranking on real Maxima calls. Fix: collect interpreter/JIT profiles for integrate/ratsimp/factor/solve/det workloads.

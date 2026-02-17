---
title: Profile maxima hotspots
status: open
priority: 1
issue-type: task
created-at: "2026-02-17T22:23:04.182924+01:00"
blocks:
  - habu-maxima-load-to-e6d01b9c
---

bench/comprehensive_bench.zig plus maxima workload scripts. Cause: no hotspot ranking on real Maxima calls. Fix: collect interpreter/JIT profiles for integrate/ratsimp/factor/solve/det workloads.

---
title: Maxima hotspot uplift after crash fix
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-02-22T18:18:23.246533+01:00\\\"\""
closed-at: "2026-02-22T18:57:45.021542+01:00"
close-reason: "Done: global alias+name indexing, VM lookup O(1), resolver trace cache, JIT cons fast path; benches improved"
---

tools/maxima-hotspots currently reports ~1.0x JIT/interp on top workloads (ratsimp/factor/integrate/solve/determinant). Implement generic (non-Maxima-specific) runtime/compiler optimizations to reduce eval/vm transient allocations and dispatch overhead. Depends on habu-rca-curr-maxima-365c1a4a and aligns with habu-cut-vm-gc-511ec7d3.

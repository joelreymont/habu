---
title: "Find a position's block without a linear search"
status: closed
priority: 2
issue-type: task
created-at: "\"2026-10-01T11:32:11.958946+02:00\""
closed-at: "2026-10-14T19:41:02.985589+03:00"
close-reason: "Implemented in c91beb3d: allocator POS-BLOCK bisects existing ascending block starts, preserving allocation results; 64-condition CPU measurement, masked output comparison, focused suites and convergence passed."
---

Problem: src/compiler/native/regalloc.f:1469 POS-BLOCK searches the block table linearly, once per eviction or candidate through MB-ANCH-POS and MB-DEF-OP?, about 39 ms at 64 conds (r4-nest commit 3 report). Acceptance: a position's block found in constant or logarithmic time from data the allocator already holds; allocation unchanged (masked dump compare, g1 == g2, two-generation build); time at 64 conds before and after in user CPU. Files: src/compiler/native/regalloc.f.

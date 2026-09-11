---
title: Compile the Tender load through the optimizer in 1.7 s
status: open
priority: 2
issue-type: task
created-at: "2026-09-11T16:38:06.249420+03:00"
---

Problem: the 3,079-definition Tender source takes 176 s through NCOMP:COMPILE on 41df9051 (forced tier 1, counted), 0.87 s through the JIT; Joel's bar (2026-09-11) is 1.7 s, 0.55 ms per definition, and executables are built 100% by the optimizer so this is the build time of every application. Measured budget (per-pass timers 99.7% accounted, 1 ms sampling): IR substrate handle re-resolution per cell read 92 s across every pass (16 nested calls per read, 1.77 G reads); symbol filter SHA-256 15 s (removed, c64808e4); regalloc.f proper 14 s and superlinear (vals^1.71 top decile, 79 bodies over 256 values cost 48 s); verify ACCEPT 31 s (ops^1.48); spill rewrite ^1.94; combine ops^1.21; checker.f/xref.f 14 s; per-definition fixed setup 17 s (context map, 15 tables, 86-declarer model walk, 7 dialect binds, 46 BIND misses per definition); the compiler in the cold seed is JIT code. Trivial-definition floor 6.8 ms, target under 0.5 ms. Acceptance: forced-tier Tender load under 1.7 s wall with 3,079/3,079 through NCOMP:COMPILE, trivial-definition floor under 0.5 ms, every existing negative test kept, no validator bypass; each child lands with a controlled before/after pair on one root. Files: src/compiler/**. Verify: 1 set-tier entry with a counting xt on NCOMP-DISPATCH:XT-CELL over /home/joel/Work/Tender src/main.f. Depends: none. Ownership: cedar (root), rowan (substrate, reuse, measurement). Claim: unassigned.

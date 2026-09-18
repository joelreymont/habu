---
title: Make non-strict float comparisons reject NaN
status: closed
priority: 2
issue-type: task
created-at: "2026-09-18T19:52:45.365661+03:00"
closed-at: "2026-09-18T21:19:14.070441+03:00"
close-reason: implemented by alder (5973ba43 Reject NaN in non-strict floating comparisons), reviewed, chained CC (gen2==gen3, full suite 449/449 on fCC3), integrated at fba695f8
---

Problem: lib/prelude.f:35-36 defines f<= as f> 0= and f>= as f< 0=. Both strict comparisons are false for unordered operands, so the inverses incorrectly return true. A quiet NaN made all four forms NaN <= 1, NaN >= 1, 1 <= NaN and 1 >= NaN true under tiers 0 and 1. Range checks therefore accept NaN. Acceptance: implement ordered <= and >= semantics; regressions cover NaN on either side, two NaNs, equality, signed zeros, infinities and finite ordering at both tiers. Files: lib/prelude.f and its tests. Verify: rebuild the baked prelude, focused tests at both tiers and the required full native suite. Ownership: prelude. Claim: unassigned.

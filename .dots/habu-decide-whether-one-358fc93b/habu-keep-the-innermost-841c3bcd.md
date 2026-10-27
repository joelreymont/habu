---
title: Keep the innermost tier 0 loop limit in a register
status: open
priority: 3
issue-type: task
created-at: "2026-10-27T20:21:25.401735+02:00"
---

tender WALK-TO-CHILD spends 11 loop-control insns per iteration at tier 0 vs 6 at tier 1 (5.5G): each loop step recomputes the loop frame address and reloads the limit from memory. Depends on habu-carry-the-tier-47ae3cb8 (which keeps the index memory-authoritative). Acceptance: the innermost do loop's step reads no loop-frame memory when the body makes no call; j, leave, unloop, throw and a call in the body correct. Gate: test/compile-floor-gate.f. Tests: test/compiler/tier0-inline-prims.f emitted-code rows plus value checks. Measure zz and tender run instructions before/after. Source: jerry gap attribution 2026-10-27 (zz daily --json at zzusage 52d7726f, gap 1.86G of 14.20G; tender c2-office-scale at 6689bab1, gap 63.36G of 330.82G). Claim: unassigned.

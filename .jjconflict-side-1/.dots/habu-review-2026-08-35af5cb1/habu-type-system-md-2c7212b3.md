---
title: type-system.md contradicts effects.md on narrowing
status: open
priority: 3
issue-type: task
created-at: "2026-08-22T22:38:25.875379+02:00"
---

Problem: docs/type-system.md:77-79 says u8/u16/u32 widen to n and the other way needs an explicit conversion; docs/effects.md:131-133 and checker.f:1253-1262 INT-WIDENS? ('got CC-N = IF RES-TRUE') let n unify with every integer type in both directions, so ': F ( n -- u8 ) ;' certifies. type-system.md is the read-first document. Acceptance: either the doc states the real lattice or the checker refuses n->u8 with a test - a ruling is needed; record it. Files: docs/type-system.md, docs/effects.md, src/core/checker.f. Verify: the fixture matches the ruling. Depends: none. Ownership: type lattice. Claim: unassigned.

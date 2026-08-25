---
title: Reuse frame slots for values never live at once
status: open
priority: 2
issue-type: task
created-at: "2026-07-31T20:09:03.319708+02:00"
---

src/compiler/native/regalloc.f hands out a fresh slot for every value it spills and never reuses one, so a long block reserves as much frame as it has spilled values even when their lifetimes are disjoint. src/compiler/native/regalloc-verify.f's slot rule is the exact form of that: a slot is written once. Reuse is the ordinary optimisation - two values whose slot lifetimes do not overlap can share - and it needs both halves changed together: the allocator has to track when a slot's content is dead (its last reload) and the validator's rule has to become the interference statement it stands in for, which needs the value identity a load carries and therefore the module-to-module correspondence of dot habu-prove-the-spill-0294e0e8. Doing the allocator half alone would red the validator, which is the design working. Owners: A64RA, A64RAV. Depends on habu-prove-the-spill-0294e0e8.

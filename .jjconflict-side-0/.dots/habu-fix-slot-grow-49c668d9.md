---
title: Fix slot-grow rollback escape (two sites)
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T09:01:43.394084+02:00"
---

Problem: a participant arena-grow failure inside SNAPSHOT followed by coordinator ROLLBACK writes outside the arena. Two sites share the shape: GENERATED-DECL-CTOR ARM-GROW (src/core/generated-declaration.f lines 685-710) and DEV-PART-ENSURE (src/core/decl-event.f line 616). In both, the grow can fail after the snapshot recorded the old capacity, and the subsequent rollback path can touch cells past the old arena bound. One dot, both sites, one structural pattern: grow before snapshot, or make the snapshot record the post-grow capacity, or make rollback bound its writes by the capacity it snapshotted - decide once, apply to both, and forbid the third variant by inventory or fixture so the next participant cannot reintroduce the shape. Acceptance: a fault-injected grow failure at each site followed by coordinator rollback leaves the arena byte-identical to the pre-declaration snapshot and writes nothing outside it, proven by a focused fixture per site; a mutation restoring the old order fails. Files: src/core/generated-declaration.f, src/core/decl-event.f, the generated-declaration transaction suite. Verify: generated-declaration and declaration-event suites. Depends: none. Ownership: participant slot-grow ordering at the two named sites only. Claim: unassigned.

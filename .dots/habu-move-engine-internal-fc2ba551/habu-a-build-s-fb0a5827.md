---
title: "A build's checker asks hb's own primitive table which primitives it elaborates"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T11:11:59.208070+02:00"
---

Problem: src/core/checker.f NAV-PRIM-ROW (:10406), which NAV-USE-AT (:10428) calls, walks PRIM-SPEC, the running hb's baked copy of src/habu/prims.f, and picks a row by the kind that table gives it. In a build the tree's prims.f can differ from hb's: a primitive the tree adds, or a row whose kind the tree changes, is looked up in hb's table for one generation. No failing program is reduced yet. Acceptance: in a build that states its primitives from the kernel object (dot habu-a-build-s-87c9c7c2, src/arch/arm64/link-kernel.f:263 STATE-PRIMS), NAV-USE-AT reads what CHECK:PRIM-NAME and CHECK:PRIM-CLOSE recorded, not PRIM-SPEC. Depends: habu-a-build-s-87c9c7c2 part 1. Claim: unassigned. Consumers: none meets it.

---
title: "A build's checker asks hb's own primitive table which primitives it elaborates"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T11:11:59.208070+02:00"
---

Problem: src/core/checker.f NAV-PRIM-ROW walks PRIM-SPEC, the running hb's baked copy of src/habu/prims.f, and PRIMITIVE-STATE takes a primitive with no effect row as elaborated only when that table says K-ELAB. In a build the tree's prims.f can differ from hb's: a K-ELAB primitive the tree adds, or a row whose kind the tree changes, is typed by hb's table for one generation. Found reading the code for privacy step 2c; no failing program reduced yet. Acceptance: in a build that states its primitives from the kernel object (dot habu-a-build-s-87c9c7c2, link-kernel.f STATE-PRIMS), the kind of each stated row comes from the object: PRIMITIVE-STATE and NAV-USE-AT read what CHECK:PRIM-NAME and CHECK:PRIM-CLOSE recorded, not PRIM-SPEC. Files: src/core/checker.f. Verify: a tree that adds a K-ELAB row to prims.f builds a program calling it in one build (reduce first: show today's refusal or misbinding). Depends: habu-a-build-s-87c9c7c2 part 1. Ownership: the lane. Claim: unassigned (brief after 2c part 1 lands; no consumer meets it).

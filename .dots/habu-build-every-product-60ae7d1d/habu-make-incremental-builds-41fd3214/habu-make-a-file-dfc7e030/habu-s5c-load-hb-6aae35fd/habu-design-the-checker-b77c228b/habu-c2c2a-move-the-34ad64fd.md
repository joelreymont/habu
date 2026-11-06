---
title: "C2c2a: move the build's string table before the checker"
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T23:55:34.806704+02:00"
---

Problem: DECISIONS 'where a word's facts text is laid': the one string table loads before the checker as package INTERN's first part so CHECK:FACTS! calls it by name; today src/habu/intern.f loads after RESTLIB, long after checker.f (boot row ~61) and check-hook.f, and intern.f's requires (layout, xref, error, string, memory, image-lifecycle) load after them, so it cannot move whole. Acceptance: the build space's table (intern.f IMAGE-BASE, SPACE?, the slot probe SPACE-SLOT, SPACE-ADD, SPACE-STRING, CLEANUP, EACH-STRING and their cells, about intern.f:228-336) moves as is into a new kernel-only file loaded between boot-cells.f and prims.f (boot-files.f FILL) and provided in native-runtime.f, the first part of package INTERN, written against kernel words only (here, allot, align, map-anon, munmap, BOOT:IMAGE-BASE-CELL; DPHW-CELL spelled as check-hook.f spells a layout cell before layout.f); E-INTERN-REWIND is minted there and src/lib/error.f's line goes; intern.f reopens INTERN, keeps the pool, STRING and the table's PROCESS-CELL registration; no behavior changes. Files: the new file, src/habu/intern.f, src/habu/boot-files.f, src/habu/native-runtime.f, src/lib/error.f. Verify: candidate build within 5 minutes; chain fixpoint; bin/hb --load test/run.f. Depends: none. Ownership: those files. Claim: unassigned.

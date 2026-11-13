---
title: "C2c2a: move the build's string table before the checker"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-06T23:55:34.806704+02:00"
closed-at: "2026-11-07T00:54:22.842703+02:00"
close-reason: "Landed on master: chain build /tmp/jc.k1 rc=0, fixpoint at generation 2 (277.8 s); full suite on its gen2 293 of 293 PASS rc=0 (/tmp/jt.2C, 93.9 s); review ACCEPT (/tmp/rv-land.ZU)."
---

Problem: by the lead's choice of 2026-11-06, the one string table loads before the checker as package INTERN's first part so CHECK:FACTS! calls it by name; today src/habu/intern.f loads after RESTLIB, long after checker.f (boot row ~61) and check-hook.f, and intern.f's requires (layout, xref, error, string, memory, image-lifecycle) load after them, so it cannot move whole. Acceptance: the build space's table (intern.f IMAGE-BASE, SPACE?, the slot probe SPACE-SLOT, SPACE-ADD, SPACE-STRING, CLEANUP, EACH-STRING and their cells, about intern.f:228-336) moves as is into a new kernel-only file loaded between boot-cells.f and prims.f (boot-files.f FILL) and provided in native-runtime.f, the first part of package INTERN, written against kernel words only (here, allot, align, map-anon, munmap, BOOT:IMAGE-BASE-CELL; DPHW-CELL spelled as check-hook.f spells a layout cell before layout.f); E-INTERN-REWIND is minted there and src/lib/error.f's line goes; intern.f reopens INTERN, keeps the pool, STRING and the table's PROCESS-CELL registration; no behavior changes. Files: the new file, src/habu/intern.f, src/habu/boot-files.f, src/habu/native-runtime.f, src/lib/error.f. Verify: candidate build within 5 minutes; chain fixpoint; bin/hb --load test/run.f. Depends: none. Ownership: those files. Claim: agent=worker workspace=.jj-ws.noindex/jerry-c2c2a.

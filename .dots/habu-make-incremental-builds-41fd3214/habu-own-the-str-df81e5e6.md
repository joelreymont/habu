---
title: Own the string pool across a field rollback
status: open
priority: 2
issue-type: task
created-at: "2026-10-25T06:49:45.975051+01:00"
---

src/core/type-family.f PFTX ROLLBACK lowers TF-STR-U to PFTX.STRU, but DEV-OPEN (decl-event.f:259) opens the field transaction before the family interns its name, so the window can hold a live family name; a rollback then commit lets the next intern overwrite it. Scrubbing it corrupts TFX-POP. Found by scrub lane.

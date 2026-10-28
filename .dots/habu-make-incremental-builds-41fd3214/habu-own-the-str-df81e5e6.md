---
title: Own the string pool across a field rollback
status: closed
priority: 2
issue-type: task
created-at: "2026-10-25T06:49:45.975051+01:00"
closed-at: "2026-10-28T17:13:33.248128+02:00"
close-reason: landed baef7bb4; PFTX.STRU gone
---

src/core/type-family.f PFTX ROLLBACK lowers TF-STR-U to PFTX.STRU, but DEV-OPEN (decl-event.f:259) opens the field transaction before the family interns its name, so the window can hold a live family name; a rollback then commit lets the next intern overwrite it. Scrubbing it corrupts TFX-POP. Found by scrub lane.

Design /Users/joel/.cache/tmp/carl-file/design-pftx.md: drop PFTX.STRU; TFAM-REWIND owns the pool. Worker lane in .jj-ws.noindex/carl-pftx on spnzoswy.

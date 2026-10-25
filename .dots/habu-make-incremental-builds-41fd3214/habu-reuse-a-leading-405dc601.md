---
title: Reuse a leading rolled-back file
status: open
priority: 2
issue-type: task
created-at: "2026-10-25T06:59:21.895631+01:00"
---

A rollback before a file's first definition (roll.f = ROLLED drop, ROLLED as in ROLL-HIT-CASE) refuses that file and its parent cut-below-segment: FILE-CUT (tools/native-unit-object.f) keeps event rows with cell 1 < kept, and the parent's LOAD row carries cell 1 = ndict@ = kept, so the cut drops it. Conservative (no wrong image) but such a file never reuses. Found by the hres major review.

---
title: Export an unknown fixed kind as unknown
status: closed
priority: 2
issue-type: task
created-at: "2026-10-24T03:46:16.350516+03:00"
closed-at: "2026-10-28T17:13:33.451730+02:00"
close-reason: landed eef06374
---

UNIT-EXPORT-LINK (src/core/checker.f) exports a link whose fixed kind is still unknown (-1) as kind none, so after replay a contract can look known where the cold build left it unknown. Found by lane carl-adm (zlnyxvvn). Workspace: none yet.

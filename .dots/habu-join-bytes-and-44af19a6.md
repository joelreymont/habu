---
title: Join bytes and paths into an arena
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T07:23:10.679095+01:00"
---

Problem: string.f's SB holds 1024 bytes and FS:JOIN-PATH joins into a fixed buffer, so a path (up to 4095 bytes) or longer text cannot be built in the storage most callers hold, an ARENA; Omabox wrote six private joins (bob's /tmp/bob-habu-arena-join.md: CONFIG:JOIN, LOCAL:JOIN, REPLAY:JOINED, RECONCILE:JOIN-BYTES, RECONCILE:JOINED, one in OMABOX). Acceptance: library words join two byte strings, a span of them, and two paths by FS's separator rule into an arena; Omabox's six can be deleted. Depends: the design (SB's fixed cap is the cause to weigh) and the names, Joel's. Claim: unassigned.

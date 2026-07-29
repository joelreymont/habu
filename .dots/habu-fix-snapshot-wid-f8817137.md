---
title: Fix snapshot WID validator region base
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T19:58:29.566008+02:00"
---

Full context: PRIORITY 1, small and independent. EM-SNAPSHOT-VALIDATE-WIDS compares and rebases out-of-line dictionary name pointers against DBASE at src/habu/habu2.f:4206-4207 and again at 4229-4230, but BSNAPREBASE (src/habu/habu2.f:4131) canonicalises those pointers to the RBASE-VA sentinel. Correct before SNAP v4, when DBASE equalled RBASE-VA; now every package record whose name exceeds DNAME-INL (16 characters) fails validation with exit 79 and the message snapshot trailer corrupt. Reproducer: any snapshot image containing the package OWNER-WID-COLD-TEST (19 characters, test/owner-wid-source.f:3). Replace both DBASE uses with RBASE-VA and add a negative regression that snapshots a package with a name longer than 16 characters and boots the image. NOTE this is the same repair as the commit a2c4ec40 that was lost with its workspace; it was independently re-derived. Landing it ALONE only converts exit 79 into the region-relocation crash, so it must land with the two priority-1 dots beside it.

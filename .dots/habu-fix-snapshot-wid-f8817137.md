---
title: Fix snapshot WID validator region base
status: active
priority: 2
issue-type: task
created-at: "2026-07-29T19:58:29.566008+02:00"
---

Full context: PRIORITY 1, small and independent. EM-SNAPSHOT-VALIDATE-WIDS compares and rebases out-of-line dictionary name pointers against DBASE at src/habu/habu2.f:4206-4207 and again at 4229-4230, but BSNAPREBASE (src/habu/habu2.f:4131) canonicalises those pointers to the RBASE-VA sentinel. Correct before SNAP v4, when DBASE equalled RBASE-VA; now every package record whose name exceeds DNAME-INL (16 characters) fails validation with exit 79 and the message snapshot trailer corrupt. Reproducer: any snapshot image containing the package OWNER-WID-COLD-TEST (19 characters, test/owner-wid-source.f:3). Replace both DBASE uses with RBASE-VA and add a negative regression that snapshots a package with a name longer than 16 characters and boots the image. NOTE this is the same repair as the commit a2c4ec40 that was lost with its workspace; it was independently re-derived. Landing it ALONE only converts exit 79 into the region-relocation crash, so it must land with the two priority-1 dots beside it.

Claim: agent=snaprel workspace=.jj-ws/habu-relocate-snapshot-region-752042fe

FIXED 2026-07-29 (agent=snaprel). The analysis in this dot was correct as
written. `EM-SNAPSHOT-VALIDATE-WIDS` now rebases both out-of-line dictionary
name pointers against `RBASE-VA`, the base `BSNAPREBASE` actually canonicalises
to, instead of the live `DBASE` (src/habu/habu2.f, the owner-record name check
and the package-record name check). x13 is free at both points and is not live
across the call from `EM-SNAPSHOT-RESTORE`, so it carries the constant.
Measured on the real child path `bin/hb --load test/owner-wid-child.f`: before
the change two assertions that expect a clean boot got exit 79 "snapshot
trailer corrupt" and two more got 29; after the change no assertion anywhere in
that run gets 79 or 29. The engine rebuilds to a byte-identical fixpoint and the
self-check census is unchanged at 4232 certified words. `test/gate-stdlib.f`
has exactly the same six red phases before and after, so the change adds no new
failure. As this dot already said, the fix alone does not turn
`owner-wid-internal` green: the residual failures are the mapping collision
(exit 78) owned by habu-relocate-snapshot-region-752042fe.

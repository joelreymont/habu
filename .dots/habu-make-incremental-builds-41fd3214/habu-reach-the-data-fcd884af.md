---
title: Reach the DATA of retired records
status: closed
priority: 1
issue-type: task
created-at: "2026-10-28T22:03:02.466948+02:00"
closed-at: "2026-10-29T00:44:58.481224+02:00"
close-reason: "Landed 3ea4c5dc 'Reach the DATA of retired records' (review ACCEPT, 5 items). Proofs at the merged tip: native-file-replay-e2e, native-file-frame-e2e, native-file-auto-e2e test: ok (user 40/99/160 s). POINTER-CELL? and REACH stay on REC-DEFER-CELL; SEED-KIND keeps TRAILER-CELL."
---

Roots lane L7 (R4, census .jj-ws.noindex/carl-roots/.out/roots.md): 23 ordered-dependency files name a retired record as holder (FIRST-RECORD-XT 12, CHECKER-VERIFY-PKG-DEPTH 11; retired by undefine, storage kept). Origin reading by ordinal accepts a retired record; its DATA row resolves on warm install; spelling lookups keep refusing retired names. Brief .jj-ws.noindex/carl-l7/.out/brief-l7.md, workspace carl-l7; worker launches after the agent limit resets. Claim: carl.

Lane done in .jj-ws.noindex/carl-l7, awaiting review: yxkkkupr 35dd5e29 "Read retired records' storage by ordinal" (dict.f only) and kmqzznry cfc15cf4 "Reach the DATA of retired records". Census 184 refused / 8 reused unchanged; FIRST-RECORD-XT and CHECKER-VERIFY-PKG-DEPTH ordered runs gone (12 files now stop at run-host-cast PTR@/XREF, 4 at mapped-compile-time-read PF-A-P/TFAM, 7 at CHUNK/REQUIRE-REG); executed-closure-unknown ordered runs 25 -> 2. Commit 2 lands only after an engine built from commit 1 is installed. Left out: REC-DEFER-CELL still refuses retired records at tools/native-unit-dependency.f:1693, tools/native-unit-object.f:2411, tools/native-file-reference.f:1194.

Commit 1 landed 2e5a9591 (duplicate of 35dd5e29; review LAND). Commit 2 under fold-in (retired-defer test, two comments), lands after the next install, which is built from 2e5a9591 or later.

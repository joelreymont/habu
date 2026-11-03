---
title: Undo a rolled-back does> patch in the checker and the engine alike
status: open
priority: 2
issue-type: task
created-at: "2026-11-03T16:49:56.735591+02:00"
---

Problem: a does> patch rolled back by a failed evaluate leaves the engine running the clause while the checker types the record as before the patch. Reproducer (both the installed hb and the does> lane candidate): `variable PV s" : PD ( -- ) does> ( -- n ) drop 9 ; PD 1 0 / drop" TEST-EVAL:RC drop s" PV @ drop" TEST-EVAL:RC` SIGSEGVs, rc 139. After the rollback `PV NULL-PTR - .` prints 9: the engine still runs the clause, whose code sits in space the rollback gave back, while the checker rewound PV to `-- ptr a`. RECORD-LINK-REWIND-EFFECT (src/core/checker.f:16495-16518 at the does> lane) also zeroes PV's link, so a later `: PB ( -- ) does> ( -- n ) drop 7 ; PB` falls back to the spelling and retypes PB: `: PT ( -- n ) PB ;` is admitted and `PV @` crashes again. A patch run inside CHECKER-SCOPE-START-NEUTRAL ends the same way (`QQV @` admitted, rc 139). Found by the review of the does> lane (habu-keep-a-does-64053af9). Acceptance: reduce it and fix the responsible layer (the engine's frame rollback of a patch to a record below the cut, with the checker's link rewind) so the engine's code for the record and the checker's type for it agree after any rollback, on arm64 and x86-64, tiers 0 and 1; a row in test/certify-does-definer.f holds the reproducer. Files: per the root cause (src/habu/interpret.f frame rollback, src/habu/habu2.f, src/habu/kernel-x64.f, src/core/checker.f). Verify: the reproducer on a rebuilt hb, test/certify-does-definer.f, test/outer-interpret.f, the full suite, the x86 row on pulsar. Depends: habu-keep-a-does-64053af9. Ownership: carl. Claim: unassigned.

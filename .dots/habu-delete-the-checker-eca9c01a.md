---
title: Delete the checker-owner slots installed but never read
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T11:30:37.374945+02:00"
---

Problem (dead-word deletion review 2026-11-11, found by name and number search only, not traced through every route): 13 checker-owner slots have an install and no reader in src, tools or test: UNIT-EXPORT-REFS, UNIT-PREPARE-REFS, UNIT-PERSIST-STORES, UNIT-STORAGE-CELLS ($430; its xt is CHECKER-STORAGE-CELLS, which has no other caller), UNIT-SCRATCH-RELEASE, UNIT-SYM, PRIMITIVE-STATE, RECORD-FIXED-READ, PENDING-MARK, PENDING-EXPORT-REFS, PENDING-PREPARE, PENDING-INSTALL, PENDING-IDLE; the contract offsets FIXED-KIND-OFF and FIXED-VALUE-OFF (mirrored as RECORD-FIXED-*-OFF) have no reader either. src/habu/proc-maps.f is required only by its own test (test/proc-maps.f, suite proc-maps); src/core/util.f:72 cites its MAPS-PATH-MAX. Acceptance: each slot traced (rg -i --no-ignore, the three consumers included); those with no reader deleted with their installs and the words only they called; proc-maps.f deleted with its test if nothing else needs it, or its user named. Same shape as dot habu-trace-and-delete-43f52767. Files: src/core/checker.f, checker-owner-*.f, src/habu/proc-maps.f, test/proc-maps.f. Verify: engine twice, map names unchanged but for the deleted words, suite. Depends: none. Ownership: unassigned. Claim: unassigned.

---
title: definers have no code-room check; exhaustion reports a seal violation
status: open
priority: 2
issue-type: task
created-at: "2026-08-22T22:47:07.222721+02:00"
---

Problem: EMIT-CREATE (habu2.f:3132-3156) and C-CONSTANT (3176-3195) emit ~28 bytes each with no 'REGION $4000 -' test (EM-INTERPRET-COLON 6109, C-TRUSTED 3230, C-CAST 3347, C-DEFER-ROOM 3491 all have one); C-STORE-NAME (2692) checks long names only; the backstop GUARD-CODE-WORD in LCEMIT (habu1.f:355-364) exits ENGINE-ERROR:SEAL-VIOLATION (83) with no message when CP reaches REGION-4 (~585 consecutive variables). Also C-DEFER-CELL (habu2.f:3465-3467, mirror forth.fs:4805-4807) stores the 8-byte cell before DP-CHECK, clobbering the profiler counter band when DP is within 8 bytes of the limit. Acceptance: the shared room check in all four definers with the 'code space full at:' diagnostic; DP-CHECK before the store; tests that exhaust each. Files: src/habu/habu2.f, bootstrap/cg/forth.fs, test/. Verify: the tests. Depends: none. Ownership: reader. Claim: unassigned.

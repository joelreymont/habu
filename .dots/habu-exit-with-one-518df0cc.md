---
title: Exit with one status for a duplicate definition in every check mode
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T14:19:34.177504+02:00"
---

Problem (arena 2a1 worker, carl-arena2 ecc02443): a duplicate definition (': Y ( -- ) ; : Y ( -- ) ;', or a taken ARENA cast name) gives the same E-DUPLICATE-DEFINITION packet in every mode, but tools/check.f exits 78 in plain and --all-errors and 70 under --verify-only; test/diag-position-test.f TEST-ARENA's taken-cast case pins it with CHECK-EXIT and '78 constant DUP-RC' instead of REFUSED-MODES. Acceptance: one exit status in every mode, the load's (measure bin/hb --load's rc first); the TEST-ARENA case moves to REFUSED-MODES and DUP-RC goes. Files: tools/check-core.f CHK-PREVERIFY-DUP / CHK-PREVERIFY-STOPPED, tools/check-verify-core.f, test/diag-position-test.f. Verify: the three modes on the two-definition file. Depends: none. Ownership: check's exit status. Claim: unassigned.

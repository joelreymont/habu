---
title: Drop the empty ncomp line after a duplicate
status: open
priority: 2
issue-type: task
created-at: "2026-10-31T21:33:20.778682+02:00"
---

Problem: after the checker hook refuses a duplicate definition, the native compiler prints a stray `ncomp: cannot compile ` line with no name. src/core/checker.f:29313 REFUSE-DUPLICATE (called from src/core/check-hook.f:244) writes the whole `duplicate definition: NAME at FILE:LINE` line, then throws DUP-RC ($4E, checker.f:29283) before the unit is named, so no name is kept; src/compiler/native/definition.f:531 SETTLE-UNIT treats rc $4E as unreported and REPORT-REFUSAL (:507) calls REPORT-FAILURE with an empty name and length 0. A replayed PRODUCT ckdrep then an evaluated `: CKDREP:MAKE ( n -- n ) 1 + ;` prints `duplicate definition: CKDREP:MAKE` then `ncomp: cannot compile ` alone, rc 78. TEST-REPLAY-DUP-PLACE (test/load-reject-diag-test.f:663) checks with CONTAINS?, so nothing pins the stray line. This is an acceptance case of its parent: fix it in the code that stays (named above), never in the machinery the parent replaces. Acceptance: SETTLE-UNIT rethrows the hook's already-reported duplicate code without REPORT-REFUSAL, reading the code from its owner (DUP-RC), so the refusal is the duplicate line alone; the test asserts the whole stderr exactly and fails first on the installed hb. Depends: none. Claim: unassigned.

---
title: Drop the empty ncomp line after a duplicate
status: open
priority: 2
issue-type: task
created-at: "2026-10-31T21:33:20.778682+02:00"
---

Problem: after the checker hook refuses a duplicate definition, the native compiler prints a stray `ncomp: cannot compile ` line with no name. src/core/check-hook.f HOOK writes the whole `duplicate definition: NAME at FILE:LINE` line, then throws DUP-RC ($4E) before the unit is named, so src/compiler/native/compiler.f SCAN keeps no name; src/compiler/native/definition.f SETTLE-UNIT treats rc $4E as unreported and REPORT-REFUSAL calls REPORT-FAILURE with an empty name and length 0 (sealed review; /tmp/jerry-dots-probe/dup2.f, a replayed PRODUCT ckdrep then an evaluated `: CKDREP:MAKE ( n -- n ) 1 + ;`, prints `duplicate definition: CKDREP:MAKE` then `ncomp: cannot compile ` alone, rc 78, on the public hb). TEST-REPLAY-DUP-PLACE checks with CONTAINS?, so nothing pins the stray line. Acceptance: SETTLE-UNIT rethrows the hook's already-reported duplicate code without REPORT-REFUSAL, reading the code from its owner (check-hook.f DUP-RC), so the refusal is the duplicate line alone; the row asserts the whole stderr exactly and fails first on the installed hb. Files: src/compiler/native/definition.f (SETTLE-UNIT), the row's test file. Verify: the reproducer at tier 1 and tier 0; the row. Depends: none. Claim: unassigned.

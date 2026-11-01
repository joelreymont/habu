---
title: Name the error when a body drops a linear value
status: open
priority: 2
issue-type: task
created-at: "2026-11-01T16:09:01.273799+02:00"
---

Problem: a checked body that drops a linear value is refused with no error code or reason. Program: lin.f = 'package LIN / DEFLINEAR LIN:tok / LINEAR: MINT ( n -- LIN:tok ) / : P ( LIN:tok -- ) drop ; / ;package', loaded from an entry: public hb (master ed087c68) prints 'habu: in p: at 'drop'' then the hook and ncomp lines, rc 70; tools/check.f --all-errors prints only 'habu: in p: at 'drop'', rc 70. Copying one gives 'exclusive value copied' and casts give E-CAST-LINEAR, so this path alone lacks its code. Acceptance: the refusal carries a code (an existing E-LINEAR-* if one means 'a linear value discarded', else a new one), message and repair hint through the same renderer, in both the engine and check.f; a row in the linear refusal suite, which fails on master. Files: src/core/checker.f (the linear consume check), its test suite. Verify: engine build; that row; check.f on the program. Depends: none. Ownership: checker.f linear drop path. Claim: unassigned.

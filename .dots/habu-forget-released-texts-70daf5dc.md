---
title: "Forget released texts on x86-64's negative allot"
status: open
priority: 2
issue-type: task
created-at: "2026-11-07T01:42:26.844770+02:00"
---

Problem: a negative `allot` that releases a facts text must drop it from the string table, or INTERN:TEXT answers the released place again (C2c2-table review N2) and data laid over its length cell can hang the table's next growth. arm64 does it: habu1.f BALLOT calls the checker word at CHECKER-OWNER-ABI:GRID-REVOKE-OFF, checker.f DP-CUT, which runs INTERN:KEEP-BELOW (C2c2-table). x86-64 does not: kernel-x64.f ALLOT-BODY raises DPHW-CELL and calls no checked word, so neither record grids nor texts are cut there. A machine asymmetry (CLAUDE.md: a split to remove). Acceptance: one rule on both machines, from the shared layer once x86-64 moves onto the one typed interpreter and linker, not an x86 patch: after `n allot` with n negative, INTERN:FIND of a text laid above the new DP answers 0. Files: src/habu/kernel-x64.f ALLOT-BODY (or its replacement). Verify: a checked probe on x86-64 as test/address-cell-rollback.f's failed-load text case does on arm64. Depends: x86-64 work resumes only after arm64-to-arm64 works (DECISIONS 2026-10-29 'arm64 first'). Ownership: unassigned. Claim: unassigned.

---
title: Fix the fixture NZCV false promises
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T15:41:15.594411+02:00"
---

test/compiler/native-chain-fixture.f LEAF-OF/LEAF-FRAMED/LEAF-ABI (lines 64/146/167) still declare NZCV:UNTOUCHED over routines that emit CMP/FCMP — the same false-promise class CG-14 removed from production (da8cd820), surviving in test-owned C-ABI contracts. State CLOBBERED there too; if any fixture assertion depended on UNTOUCHED, that dependency was already meaningless.

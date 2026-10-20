---
title: Supply native trap literal origins
status: active
priority: 2
issue-type: task
created-at: "2026-10-19T19:10:55.534810+03:00"
---

Supply hir.origin = -1 in native-trap.f's hand-built integer literals; preserve
every trap assertion and compiler rule. With the c0e617fe/a3297544 donor pair,
the uncorrected suite exits 67 after the mixed-path, trap-first and all-trap
assertions fail and E-A64EMIT-PLACE (-8556) escapes; the corrected suite exits
0 with test: ok, including child execution. Landing depends on the HIR origin
schema and HIR:KEY-ORIGIN introduced in a8eb05379c4a (Implement pending
definition object): master b4c8fdf5 has neither. The donor bakes that compiler
work, so its green run does not qualify a master-built engine. Retain the
fixture-only change in carl-trap-fix until that prerequisite lands; then rebase,
rerun the suite with the matching product, close this dot and push master.

Reviewed fixturee876f1c1 is integrated at366dd667 with the HIR-origin prerequisite. It passes unchanged on matching release-source35e00c85 product4653470c. The completed workspace, its stale-tree backup and temporary products are retired after exact source comparison. Combined fixed-point native registry and landing remain open.

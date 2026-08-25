---
title: "Reach the memory order's per-path clause from a module"
status: open
priority: 3
issue-type: task
created-at: "2026-08-01T14:36:34.268426+02:00"
---

src/compiler/native/regalloc-verify.f ORDER-CK has three clauses: an order is read at least once, no block reads one twice, and no two readers are on a common path that does not redefine it. The first two are falsified by mutating the selector. The third is not reachable from any mutation of the chain - a module with two readers on one path is refused first by IR-VERIFY's dominance rule or by A64RA's edge rule - so it is documented as a backstop and is not tested. Wanted: a hand-built machine module in test/compiler/native-regalloc.f whose order is read in two blocks on one path with dominance intact, and the ACCEPT refusal it earns, so every clause of the rule is falsified by a case rather than by an argument.

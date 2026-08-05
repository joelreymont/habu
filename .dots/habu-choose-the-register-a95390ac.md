---
title: Choose the register budget from the routine, not the caller
status: open
priority: 2
issue-type: task
created-at: "2026-08-01T18:47:55.819141+02:00"
---

Every caller of the native chain states how many scratch registers the routine may use: 4 for a straight-line word, 8 for one with a loop, chosen by hand and copied between tools/codegen-compare-migrated.f, test/compiler/native-chain.f and src/compiler/native/migrate.f's DEFINE arguments. It is a budget, not a fact about the program, and a wrong one is either a refusal (E-A64RA-PRESSURE, the frame cannot absorb the spills) or wasted registers. The allocator already knows how many values are live at once, and the routine contract already says which registers a Habu word may destroy, so the budget can be derived: give the chain a default pool that is the whole set A64EFF admits for the convention, and let the spill path handle a routine that still does not fit. Then NMIGRATE:DEFINE loses its regs argument and the two constants in the comparison harness go away.

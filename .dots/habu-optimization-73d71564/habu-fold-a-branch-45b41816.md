---
title: Fold a branch on a constant predicate in tier 1
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T13:13:33.086209+03:00"
---

Problem: a constant predicate such as NATIVE-CELLS? already folds to a literal in tier 1 (a constant word is a fixed row), but the brz that tests it does not: the emitted code carries mov plus cbz and a dead arm, seen in src/compiler/ir/arena.f RD@ and CELL-AT (the hottest words of the self-build, 15.6 percent of samples) whose dead arm still contains a CDIGEST:SLOT@ call. Turning a two-successor brz on a known constant into a single-successor br is a CFG rewrite with block-argument reconciliation and unreachable-block elimination in src/compiler/native/select.f and combine.f, not a lowering-table entry (finding of the inline-prims lane, 2026-09-16). Acceptance: a brz whose operand is a compile-time constant lowers to the taken successor only, the unreachable block and its arguments are dropped, a fixture compiles a word with a constant predicate at tier 1 and asserts no cbz/cbnz and no code from the dead arm in its span while both truth values still compute correctly, the existing compiler fixtures pass, byte fixpoint, self-build time before/after. Files: src/compiler/native/select.f, src/compiler/native/combine.f, a test under test/compiler/. Verify: fixtures on a rebuilt engine. Depends: habu-inline-trivial-engine-922133ca (land that first), habu-qualify-habu-for-9ccd0432 (after release). Ownership: hazel line. Claim: unassigned.

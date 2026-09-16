---
title: Elide the three tied identity spill pairs
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T12:51:17.714719+03:00"
---

Problem: after the identity-copy elision (MB-IDENTITY-COPY? in src/compiler/native/regalloc.f), three adjacent same-slot ldr/str pairs remain in the baked engine, inside PSIG, ROW-WIDE? and RESOLVE in src/core/checker.f (large BEGIN WHILE REPEAT words), each shaped cmp / b.cond / ldr x,[sp,#16] / str x,[sp,#16] / b. Leading hypothesis from the worker who removed the other 43,195: MB-TIES unions a tied operand and result into one class the way MB-COALESCE does, but the predicate only recognises MB-COPY? ops; widening it needs a decision about which tied forms compute nothing. Acceptance: one instrumented build that names the operation behind the three pairs, the predicate widened only for forms proven to move nothing, test/compiler/native-identity-spill.f extended with a reproducer for that shape, zero pairs in the baked region, byte fixpoint. Files: src/compiler/native/regalloc.f, src/compiler/native/spill.f, test/compiler/native-identity-spill.f. Verify: the fixture on a rebuilt engine and the pair scan (/tmp/hazel-prof regiondump method). Depends: none. Ownership: hazel line. Claim: unassigned.

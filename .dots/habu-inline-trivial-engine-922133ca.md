---
title: Inline trivial engine primitives in tier 1
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T11:59:50.343962+03:00"
---

Problem: tier-1 code calls engine primitives whose bodies are a few instructions: cell-view is one ret (an identity address view) and its calls cost 1.3 percent of the self-build; ptr-field is 9 instructions at 9.3 percent, cell+ 6 instructions at 3.6 percent, then max, mod, !, tuck, byte-view; together 22 percent of the 138 s build is spent inside primitive bodies entered by BL, plus the caller-side push, BL, ret, pop charged to the caller. cells and + are already folded (madd appears inline), so the lowering table simply does not cover these ops. FPRIM-L records every primitive body span in ENGINE-PRIMS, so the material for inlining exists. Also seen: NATIVE-CELLS? materialized as mov x1,#-1 then tested with a runtime cbz instead of folded, and cold throw paths laid out inline in the middle of hot bodies. Acceptance: the tier-1 lowering table covers ptr-field, cell+, cell-view, max, mod, tuck, byte-view (an identity view lowers to nothing), constant predicates fold, the existing compiler fixtures pass, a fixture pins that the T4 reproducer from the dead-pair dot contains no BL to those primitives, self-build time before and after, byte fixpoint, full gate. Files: src/compiler/native/lower*.f (find the table), src/compiler/native/elaborate.f, a test under test/compiler/. Verify: fixtures through bin/hb, tools/native-build.f twice and cmp. Depends: habu-elide-same-slot (the dead pairs), so the two wins measure separately. Ownership: hazel line. Claim: unassigned.

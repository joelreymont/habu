---
title: Declare quotation-typed persisted cells where their kind is decided
status: open
priority: 2
issue-type: task
created-at: "2026-09-13T09:23:14.720422+03:00"
---

Problem: a checked quotation store declares its persisted DATA cell to the relocation table only on the optimizing tier (src/compiler/native/elaborate.f DO-QUOTATION-STORE routes it through QUOTATION-STORAGE:STORE, which calls xt!); tier 0 lowers the same store as a plain ! and declares nothing, so an image built with tier-0 application code keeps the builder's JIT address and dies SIGILL below the live region base (measured 2026-09-12 on test/app-image.f; closed for the app-image.f path by selecting tier 1 at that file's tail). Any other image path that compiles user source on tier 0 still has the hole. Acceptance: a TYPED-VARIABLE / TYPED-BUFFER whose target type is a quotation registers its cell at declaration the way PERSISTED-PTR-VARIABLE does with ptr-cell-mark, so relocation is tier-independent; that needs a mark-only XT counterpart to ptr-cell-mark (today only xt! reaches LMARK and must store a token), a new engine primitive with its bootstrap/cg/forth.fs mirror and a seed refresh before core may use it (two-stage landing as with align); the checker owns the primitive's effect; a regression builds an image with one such cell at tier 0 and runs it; docs/native-applications.md states the rule. Files: src/habu/habu1.f, bootstrap/cg/forth.fs, src/core/checker.f, lib/ (the typed definers), test/app-image*.f, docs/native-applications.md. Verify: the regression at both tiers, the chain, test/run.f. Depends: habu-build-the-compiler-c348eab0. Ownership: hazel. Claim: unassigned.

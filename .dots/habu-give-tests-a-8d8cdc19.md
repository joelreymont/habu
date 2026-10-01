---
title: Give tests a typed boundary for evaluating source
status: open
priority: 2
issue-type: task
blocks:
  - habu-add-the-evaluate-12ffa000
created-at: "2026-09-16T16:57:00.779496+03:00"
---

Problem: 81 TRUSTED: sites under test/ wrap evaluate of a source string (and 16 more use depth-driven stack cleanup or raw memory), because docs/forth.md forbids an honest axiom for arbitrary evaluate; every whitebox and checker suite that compiles a subject string therefore carries an unchecked body (sweep lane, 2026-09-16). Acceptance: one checked word in lib/test (or the checker) that evaluates a source string under a declared effect and refuses when the evaluated program does not have it (the checker already certifies the string, so the runtime check is the stack-depth and type contract at the boundary), with the failure named; the evaluate sites convert to it; depth-driven cleanups become explicit; case counts unchanged. Files: lib/test/*.f, src/core/checker.f, the test files. Verify: the affected suites; test/run.f. Depends: none. Ownership: test library. Claim: unassigned. Parent: habu-trusted-dies-prim-4fd12d60.

Design (Fable Plan, 2026-10-01; ~/.cache/tmp/heron-arm64/design-evaluate-closed.md): the typed boundary is the engine primitive evaluate-closed (dot 12ffa000). This dot owns two leaves after it.
- lib/test/eval.f, package TEST-EVAL, required by lib/test.f: N ( ptr u8 n -- n ) runs the caller's text inside a constant closed text that stores exactly one cell; FLAG ( ptr u8 n -- bool ); RC ( ptr u8 n -- n ) returns the throw code, 0 when the text loaded. Test lib/test/eval-test.f (registered beside subject-test): 40 2 + gives 42; definitions then a value; 1 2 gives E-EVAL-RESIDUE; an empty text gives 70; a refused definition gives 70; FLAG; nested N.
- The sites, after lib/test/eval.f, split for parallel workers: test/compiler/* (26 sites in 17 files); the checker, type and decl suites and their children (65); lib/test/subject.f, test/gate-common-lib.f and test/runtime-regression-test.f:589. Recipe in the design file. Per file, the TRUSTED: count drops to its non-evaluate count, the owning suite stays green and its T-REPORT case count is unchanged. At the end rg -n 'TRUSTED: .*evaluate' test lib finds nothing.

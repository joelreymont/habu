---
title: Give tests a typed boundary for evaluating source
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T16:57:00.779496+03:00"
---

Problem: 81 TRUSTED: sites under test/ wrap evaluate of a source string (and 16 more use depth-driven stack cleanup or raw memory), because docs/forth.md forbids an honest axiom for arbitrary evaluate; every whitebox and checker suite that compiles a subject string therefore carries an unchecked body (sweep lane, 2026-09-16). Acceptance: one checked word in lib/test (or the checker) that evaluates a source string under a declared effect and refuses when the evaluated program does not have it (the checker already certifies the string, so the runtime check is the stack-depth and type contract at the boundary), with the failure named; the evaluate sites convert to it; depth-driven cleanups become explicit; case counts unchanged. Files: lib/test/*.f, src/core/checker.f, the test files. Verify: the affected suites; test/run.f. Depends: none. Ownership: test library. Claim: unassigned. Parent: habu-trusted-dies-prim-4fd12d60.

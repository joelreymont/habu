---
title: Reserve the ptr tail in ENUM and STRUCTURE
status: closed
priority: 2
issue-type: task
created-at: "2026-10-01T11:36:34.868754+02:00"
closed-at: "2026-10-14T19:26:18.128429+03:00"
close-reason: "Implemented in 99c8345d: ENUM and STRUCTURE reject ptr as a reserved family name through SIG-PTR-TOK; scope, declaration and check.f cases passed with converged images."
---

Problem: src/core/enum-decl.f:208 and src/core/structure-decl.f:185 (NAME-RESERVED?) admit ptr inside a package: 'package P ENUM ptr aa bb ;ENUM' loads rc 0, then ': F ( ptr -- ptr ) ;' in P fails rc 70 "'ptr' needs an element type" (measured by r4-deftype on its base engine). r4-deftype commit c1c06544 made DEFTYPE, NEWTYPE, SUMTYPE and PRODUCT ask SIG-PTR-TOK?, the parser's own predicate. Acceptance: ENUM and STRUCTURE refuse the same tail through SIG-PTR-TOK? with the loader's reserved-name refusal and check.f's located E-BAD-NOMINAL-TYPE; cases in test/deftype-suite.f or the enum and structure suites and tools/check-test-lib.f seen to fail first; engine rebuilt. Files: src/core/enum-decl.f, src/core/structure-decl.f.

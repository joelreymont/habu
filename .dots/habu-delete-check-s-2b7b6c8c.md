---
title: "Delete check's reserved-name lint"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T14:27:50.792820+02:00"
blocks:
  - habu-refuse-match-as-06192e22
  - habu-judge-unsafe-body-50d84a9b
  - habu-one-rule-for-15bf4fc2
  - habu-pre-pass-reads-19446126
---

Problem: tools/reserved-name-lint-core.f, run only by tools/check-core.f CHK-RUN-RESERVED-NAMES, restates rules the definers own (control words, number-shaped names: interpreter.f DEF-WALL) and overreaches the language (char, ', type, :} and others the load admits and calls reach; the dot-digit rule RNL-DOT-DIGIT? 279 refuses U.0 and A.1B, which load and are callable), so plain check refuses test/engine-suite.f 550-551 where the load passes (hunt 633076da). docs/forth.md 76-88 and docs/forth-card.md 77-83 state the lint's list as the language's rule; forth.md 84 lists 4 of the 6 target predicates. Acceptance: the lint and its check step are gone; every rule it held that the language needs lives in its owner (the ;match, unsafe-spelling, interpreter-keyword and target-predicate dots); the docs state the language's rule. Files: tools/reserved-name-lint-core.f, tools/reserved-name-lint.f, tools/check-core.f, docs/forth.md, docs/forth-card.md, comments at tools/zed-run-lib.f 173, verify-source.f 765, tools/public-signatures-core.f 326, src/lib/source-lex.f 254. Verify: plain check of test/engine-suite.f rc 0; rg -i --no-ignore finds no caller. Depends: the four owner dots. Ownership: the lint files. Claim: unassigned.

---
title: typed-local-diff-lint honours its pragma by substring
status: open
priority: 2
issue-type: task
created-at: "2026-08-22T22:47:07.181640+02:00"
---

Problem: tools/typed-local-diff-lint-core.f:74-75,99-105 reads 'typed-local-lint: allow-bare-local' by raw substring on the added line (a string literal carrying it silences the group) and never consults LINT-LEX:ERROR? while every sibling fails closed on a lexer diagnostic (error-code-lint-core.f:314-328, package-diff-lint-core.f:960-970, schedule-lint.f:753-757); the test's only pragma case (:131) is a comment. Acceptance: the pragma read as a comment token via CONTENT; ERROR? checked; a string-decoy fixture red. Files: tools/typed-local-diff-lint-core.f, its test. Verify: the test. Depends: none. Ownership: lints. Claim: unassigned.

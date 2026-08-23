---
title: three lints still match text instead of structure
status: active
priority: 1
issue-type: task
created-at: "2026-08-22T22:47:07.130464+02:00"
---

Problem: tools/namespace-lint-core.f:103-109,159-168 NL-QUOTES-ODD? toggles an in-string flag by counting quote bytes per token - the heuristic tools/error-code-lint-core.f:16-22 records as having failed in both directions (one bare '[char] "' blinds the rest of the file, and it still prints 0 findings); tools/stdin-closure-lint.f:51-52 is CONTAINS? over whole files (a comment naming SDC-INCLUDE$ satisfies it; its only self-test :109-114 proves the detector answers both ways); tools/maki-dep-lint-core.f:116 matches 'maki/' inside TOKENIZE tokens. No fixture in namespace-lint-test.f:66-69 or anywhere for stdin-closure-lint is built to fool them. AGENTS.md Test Integrity is BLOCKING. Acceptance: all three consume LINT-LEX (tools/lint/source-lex.f) as error-code-lint-core.f:254-328 does; stdin-closure requires the accessor as a WORD token in the lexed build files and reads SRC_COMMON=(...) as a list; fool-fixtures ([char] ", comment decoys, string decoys) red before, green after. Files: the three lints and their tests. Verify: each lint's test with the fixtures. Depends: none. Ownership: lints. Claim: agent=lint-text workspace=.jj-ws/habu-three-lints-still-eb2aceee

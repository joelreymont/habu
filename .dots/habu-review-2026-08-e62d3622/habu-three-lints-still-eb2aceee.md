---
title: three lints still match text instead of structure
status: closed
priority: 1
issue-type: task
created-at: "2026-08-22T22:47:07.130464+02:00"
closed-at: "2026-08-23T12:30:26.705892+02:00"
close-reason: implemented, reviewed, merged, gates green: namespace-lint and maki-dep-lint decide by LINT-LEX kinds and payloads (a6108a77; nine fool fixtures each measured wrong on the old lint and right on the new; namespace-lint sealed as NAMESPACE-LINT), the launcher parse extracted into package BOOTSTRAP-SRC with a driver-aware PARSE and bootstrap-mirror-lint migrated onto it byte-identically (-174 lines), stdin-closure lexed with the two tree-mutation probes as fixtures (ad21261f); landed on master; eleven suites, lint-tools and lint-libs slices, maki, both diff lints, error-code-lint, schedule-lint green.
---

Problem: tools/namespace-lint-core.f:103-109,159-168 NL-QUOTES-ODD? toggles an in-string flag by counting quote bytes per token - the heuristic tools/error-code-lint-core.f:16-22 records as having failed in both directions (one bare '[char] "' blinds the rest of the file, and it still prints 0 findings); tools/stdin-closure-lint.f:51-52 is CONTAINS? over whole files (a comment naming SDC-INCLUDE$ satisfies it; its only self-test :109-114 proves the detector answers both ways); tools/maki-dep-lint-core.f:116 matches 'maki/' inside TOKENIZE tokens. No fixture in namespace-lint-test.f:66-69 or anywhere for stdin-closure-lint is built to fool them. AGENTS.md Test Integrity is BLOCKING. Acceptance: all three consume LINT-LEX (tools/lint/source-lex.f) as error-code-lint-core.f:254-328 does; stdin-closure requires the accessor as a WORD token in the lexed build files and reads SRC_COMMON=(...) as a list; fool-fixtures ([char] ", comment decoys, string decoys) red before, green after. Files: the three lints and their tests. Verify: each lint's test with the fixtures. Depends: none. Ownership: lints. Claim: closed (landed on master ad21261f).

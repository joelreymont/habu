---
title: six private source lexers beside the shared one
status: open
priority: 2
issue-type: task
created-at: "2026-08-22T22:47:07.176572+02:00"
---

Problem: tools/lint/token.f TOKENIZE, tools/lint/lib.f:91-97 PAT-*, tools/checked-boundary-lint-core.f:180-262 UB-*, tools/duplicate-definition-lint-core.f:205-267 DDL-*, tools/repl-lint-core.f:205-267 R-*, plus private skip-comment/skip-string loops in source-discovery.f, diag-origin-core.f, public-signatures-core.f, build-fixpoint.f:690-712 (11 files); UB-SKIP-IGNORED:209 and DDL-NEXT-TOKEN?:264 treat any '('-initial byte as a comment opener against engine parity (source-lex.f:254-258; (CMP) at habu1.f:1417 is a word); none knows PRIM: rows or .( . Acceptance: every consumer lexes through LINT-LEX; the private lexers deleted; a fixture with (CMP) and a PRIM: row passes each lint. Files: as listed. Verify: the lints' tests. Depends: the token.f dot. Ownership: lints. Claim: unassigned.

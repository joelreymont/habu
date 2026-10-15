---
title: Lint tools use source-lex and string.f
status: open
priority: 2
issue-type: task
created-at: "2026-10-15T17:16:31.146951+03:00"
---

Problem: tools carry private tokenizers: source-discovery.f:569-585, 664-719; public-signatures-core.f:445-530; diag-origin-core.f:153-230 (which already requires lint/token.f). tools/lint/text.f redefines string.f words (LINT-STR= used in 17 files, LINT-STR=CI in 14) and reimplements fs.f READ-ALL as READ-FILE. Acceptance: these tools tokenize through source-lex.f or lint/token.f and use string.f and fs.f directly; the private copies are deleted; lint outputs unchanged. Files: those named, plus the users of lint/text.f. Verify: lint fixture rows. Depends: carl-final-core chain landed on master (it edits these files); start from master after it lands. (source-discovery.f is in it.) Ownership: listed files. Claim: unassigned.

---
title: Fix signature comment fixture
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-27T19:25:25.402687+02:00\""
---

Invariant: GOOD$ emits one backslash byte before the comment text, so LINT-LEX classifies the suffix as a line comment and signature lint sees no fake definition. Cause: tools/signature-lint-test-lib.f used plain s-string text with two backslash bytes; the corrected lexer therefore treated the second byte as a WORD and reported a false COMMENTED definition. Fix: define the byte value once and append exactly one byte before the comment text; production semantics do not change. File: tools/signature-lint-test-lib.f. Acceptance: the old form contains two byte-92 values and reproduces the COMMENTED failure; GOOD$ contains exactly one; the signature suite and exact diff lints pass. Depends: none. Blocks: habu-make-lint-lex-0edc045e. Claim: agent=parallel_rule_fix2 workspace=.jj-ws/gate-size-entry.

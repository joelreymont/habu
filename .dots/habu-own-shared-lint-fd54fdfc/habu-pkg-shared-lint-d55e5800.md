---
title: Package shared lint lexer
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T17:33:51.885178+02:00"
blocks:
  - habu-pkg-lint-text-ffbb9ba5
  - habu-pkg-signature-lint-5bf3505c
  - habu-pkg-aot-lint-04bedf7b
  - habu-pkg-reserved-name-65e4395f
  - habu-pkg-bootstrap-mirror-663e20b8
  - habu-cut-over-check-ac1b7cdf
  - habu-pkg-all-errors-86fd1b4a
---

Files: tools/lint/source-lex.f and qualified call-site edits in exactly these direct consumers: tools/typed-local-diff-lint-core.f, tools/signature-lint-core.f, tools/check-core.f, tools/bootstrap-mirror-lint.f, tools/check-all-errors-core.f, tools/bootstrap-codegen-test.f, tools/check-test-lib.f, tools/lint/ptx-emitter-lint.f, tools/lint/shadow-lint.f, tools/reserved-name-lint-core.f, tools/lint/shadow-lint-test.f, tools/lint/text-foundation-test.f, tools/aot-lint-core.f, and tools/package-diff-lint-core.f. All package-less consumers must land their owner dots first. Define package LINT-LEX; keep storage and scanners private; publish exactly the kinds, scanner, token accessors, and generic diagnostic accessors in the parent contract. Remove all global LEX-/L* API, quote-specific diagnostic cells, and mutable exports atomically; no aliases. Acceptance: exact tokens, byte offsets, line and column spans, comments, strings, escaped quotes, generic lexical diagnostics, large inputs, and reuse remain correct for every consumer. Verify: text-foundation, every direct consumer focused test, lint-tools, checker tests, typed-local-diff-lint, package-diff mutation, host-lint, filemap-lint.

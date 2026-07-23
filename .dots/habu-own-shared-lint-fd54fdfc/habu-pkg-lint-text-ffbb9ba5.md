---
title: Package lint text tests
status: closed
priority: 1
issue-type: task
created-at: "2026-07-22T17:32:17.231696+02:00"
closed-at: "2026-07-23T02:05:41.065000+02:00"
close-reason: Landed and remotely verified at ad47ef725011; independent destruction review and exact owning/master gates green.
---

Files: tools/lint/text-foundation-test.f only. Preserve the narrow CAD-NUM reopen used for checked numeric conversion, then put all remaining fixture storage, helpers, and execution in package LINT-TEXT-TEST with private short tails and a private RUN invoked before leaving the package. Do not change source-lex.f or its global API in this leaf. Acceptance: no text-test definition or storage remains global; string, scanner, signature, lexer, unterminated quote, tokenizer, large-input, and lint-source cases all still run; no alias. Verify: bin/hb --load tools/lint/text-foundation-test.f, typed-local-diff-lint, package-diff focused mutation, host-lint, filemap-lint.

Claim: agent=lint_text_test workspace=.jj-ws/habu-pkg-lint-text-ffbb9ba5.

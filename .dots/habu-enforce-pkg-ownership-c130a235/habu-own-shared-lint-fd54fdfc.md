---
title: Own shared lint lexer
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T17:30:51.005816+02:00"
---

Problem: tools/lint/source-lex.f is a package-less mutable singleton used by fourteen lint, checker, and test modules. The package-ownership gate cannot correctly handle primitive registry rows until this shared lexical boundary has one owner and every consumer uses its public API. Design: migrate the dependency closure from the leaves inward, then define package LINT-LEX with private storage and short public tails. The public surface is WORD, COMMENT, REGISTRY, UNTERMINATED-QUOTE, and MALFORMED-REGISTRY kind constants; SOURCE ( ptr u8 n -- ); COUNT ( -- n ); TOKEN and CONTENT ( n -- ptr u8 n ); KIND@, BYTE@, LINE@, and COL@ ( n -- n ); ERROR? ( -- bool ); and ERROR-KIND@, ERROR-BYTE@, ERROR-LINE@, and ERROR-COL@ ( -- n ). SOURCE clears prior state before every scan. Malformed input sets one immutable diagnostic record; consumers that require valid source must reject when ERROR? is true. No compatibility aliases, exported mutable cells, duplicate lexer, prepass, or path exemption. Ownership: child dots own disjoint consumer migrations; the final provider leaf owns tools/lint/source-lex.f and the qualified call-site cutover. Acceptance: no source-lexer state or LEX-/L* API remains global; every direct consumer loads through its exact checked path; focused lexer and consumer tests preserve spans, comments, strings, escaped quotes, large inputs, generic lexical diagnostics, and reuse after failure. Verify: child focused gates, tools/lint/text-foundation-test.f, lint-tools, check tests, host-lint, filemap-lint, typed-local-diff-lint, and dot-dep-lint.

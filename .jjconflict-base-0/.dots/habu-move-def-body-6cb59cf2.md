---
title: Move definition body reader into LINT-DEF
status: open
priority: 3
issue-type: task
created-at: "2026-07-28T13:23:17.899684+02:00"
---

Full context: test/compiler/ir-id-source.f needs the token body of a named definition, to freeze the exact bodies of SERIAL-NEXT, TRY-SERIAL, CHECK-N and the other identity words. package LINT-DEF (tools/lint/def.f) has no public word mapping a definition name to its body token range, so about ten lines composing DIRECT-KIND, NAME-I and CLOSE? live in package COMPILER-ID-SRC instead. That composition reads no source bytes of its own - it goes through the shared lexer in tools/lint/source-lex.f - so it is not a duplicate tokenizer, but its right home is LINT-DEF where every other lint can use it. Required result: LINT-DEF exposes a public name-to-body-token-range word; COMPILER-ID-SRC calls it and drops its local walk. Acceptance: the parity gate stays green with identical behaviour; the new word has its own focused test including hostile fixtures (name in a comment, in a string, duplicated definition, definition of a name that is also a constant); package-diff-lint and typed-local-diff-lint pass.

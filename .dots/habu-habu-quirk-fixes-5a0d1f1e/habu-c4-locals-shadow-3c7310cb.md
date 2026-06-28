---
title: "C4: locals shadow words in body"
status: closed
priority: 2
issue-type: task
created-at: "\"2026-06-27T13:15:58.135709+02:00\""
closed-at: "2026-06-28T15:12:41.451253+02:00"
close-reason: "Locals already shadow ordinary words/builtins on the C3 gate-green engine (dup->5, code->7, over->8 via test/c4-shadow-test.f); reserved-name handling covers it, no engine change needed. Structural loop-index shadowing inside a do-loop is B2's domain (still open)."
blocks:
  - habu-b2-local-shadows-ae2492da
---

Principled fix for reserved i/j/k/code/dup as locals: resolve locals in a scope that shadows dictionary words within the word body (name i -> your local, not the loop word). Standard lexical scoping; pairs with C1 (block-scoped locals). Supersedes the B2 diagnostic once landed. src/habu locals lookup.

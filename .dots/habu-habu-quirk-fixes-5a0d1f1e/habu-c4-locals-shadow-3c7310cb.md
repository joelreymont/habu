---
title: "C4: locals shadow words in body"
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T13:15:58.135709+02:00"
blocks:
  - habu-b2-local-shadows-ae2492da
---

Principled fix for reserved i/j/k/code/dup as locals: resolve locals in a scope that shadows dictionary words within the word body (name i -> your local, not the loop word). Standard lexical scoping; pairs with C1 (block-scoped locals). Supersedes the B2 diagnostic once landed. src/habu locals lookup.

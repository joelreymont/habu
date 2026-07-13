---
title: "STRUCTURE: generate MAKE UNMAKE"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T17:13:28.696041+02:00"
blocks:
  - habu-structure-parse-typed-c5a01e1f
---

Own STRUCTURE constructor generation and focused ctor tests. Generate sealed FAMILY:MAKE and FAMILY:UNMAKE checked effects from declaration-order field schemas, preserve generic substitutions and exact layout width, and publish atomically only after declaration validation. Add positive round trips and arity/type/rollback negatives.

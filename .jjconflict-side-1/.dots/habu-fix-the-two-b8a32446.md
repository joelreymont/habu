---
title: Fix the two production catch-drop error masks
status: open
priority: 2
issue-type: task
created-at: "2026-08-13T16:46:06.592794+02:00"
---

The exceptions design probe (6ceb7667) found exactly two production catch sites that mask the rc with drop, violating docs/forth.md section Errors: lib/content-key.f:610 and src/core/type-family.f:2803. Every other production shape binds the rc and conditionally rethrows. Fix both to handle or rethrow; add the missing test where handling is the right answer. Files: lib/content-key.f, src/core/type-family.f. Depends: none.

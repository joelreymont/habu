---
title: Ignore literals in shadow lint
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-16T11:49:09.395820+02:00\""
---

Context: tools/lint/shadow-lint.f tokenizes whole source and treats a ':' inside a diagnostic string followed by a primitive name as a real definition; src/core/render.f's E-LINEAR-FORK text triggered a false 'fork hides a prim'. Fix: classify definition tokens only in executable source, excluding strings and comments via the shared source lexer; add focused false-positive strings/comments plus true definition regressions. Acceptance: shadow-lint focused test and repo lint pass.

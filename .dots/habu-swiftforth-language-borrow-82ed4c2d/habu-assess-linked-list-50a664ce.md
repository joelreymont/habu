---
title: Assess linked-list defining words
status: open
priority: 3
issue-type: task
created-at: "2026-06-28T08:10:11.882225+02:00"
---

Files: docs/forth.md, docs/stdlib.md, possible lib/list.f, tests. SwiftForth @REL/!REL/,REL/>LINK/<LINK/CALLS support relocatable compile-time linked lists. Habu snapshots/AOT have different relocation and checker constraints. Fix: decide whether a checked linked-list DSL is needed after switch/execution-vector design; if implemented, model link pointer types and traversal effects, forbid unchecked relative-address arithmetic, and validate with xref/dictionary tests.

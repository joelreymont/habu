---
title: Add DEFER-LAYOUT-BUFFER to the checker unsafe-token seal lists
status: open
priority: 3
issue-type: task
created-at: "2026-07-20T17:33:02.246748+02:00"
---

Parity gap flagged at the stage-2 landing (4f45b5d1): DEFER-LAYOUT-BUFFER is a source-evaluating opener like LAYOUT-BUFFER but is not yet in checker.f's UNSAFE-TOK?/UNSAFE-SET-SEAL lists, so it lacks the body-reject + EXPORT-alias parity its sibling has. Harmless today (maki uses it only at top level; no engine file uses it) but the seal discipline should not depend on usage discipline. Add it to both lists mirroring the LAYOUT-BUFFER entries, red-first (a body/EXPORT misuse must reject after, and demonstrably slip through before if it does - prove which). Engine change: CODELEN rows same-commit if text moves, fixpoint x2. Territory: src/core/checker.f seal lists + a seal-negative test.

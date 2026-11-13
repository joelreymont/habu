---
title: Check the code ceiling before the checker makes a namespace
status: open
priority: 2
issue-type: task
created-at: "2026-11-03T13:32:02.095910+02:00"
---

Problem (from tim via Maki, 2026-11-03, read from source, not probed): src/core/checker.f:12542-12549 NS-ENSURE calls ROOM, which checks NDICT, then namespace-record; tim reports that the code ceiling is not checked, so a replay near the ceiling would exit 83 instead of throwing. Acceptance: first a reproducer that reaches the ceiling through NS-ENSURE (or proof that ROOM covers it, then close); then NS-ENSURE refuses with a thrown code before namespace-record. Files: src/core/checker.f NS-ENSURE, ROOM. Verify: the reproducer. Depends: none. Ownership: unassigned. Claim: unassigned.

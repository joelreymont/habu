---
title: Trail or justify RVK-INFERRED! marks
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T12:06:02.883097+01:00"
---

Problem: src/core/checker.f RVK-INFERRED! writes the kind table untrailed, while the comment above the permanent marks limits untrailed marks to build-time contexts and says unification uses the trailed TVK-RAISE-TO; it runs from QUOT-TAIL-MARK at a literal's ;] (CF-SEMIQ through QUOT-ROWS-GENERALIZE). If a ;] runs inside a trial that is undone, the mark outlives the undo. Acceptance: a reproducer where an undone trial keeps the mark, fixed by trailing the write, or the reason no trial spans a ;] stated beside RVK-INFERRED!. Depends: none. Claim: unassigned.

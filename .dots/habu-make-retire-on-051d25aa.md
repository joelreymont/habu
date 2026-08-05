---
title: Make retire-on-throw a checker capability
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T10:36:19.513667+02:00"
---

Review suggestion: four independent instances this round wrote registry-mutating words that clean up only on normal return (IR context teardown, migrate RUN cleanup, TFX/SVX rollback, arena builder construction). A checker rule — a word that mutates registry state must retire it on every exit path, or a with-combinator that structurally owns the retirement — makes the whole class unwritable. Per Checker-Miss RCA discipline (docs/forth.md): this is the answer to 'why didn't the checker catch CG-07/08/23?'. Design the capability, don't scatter catch frames. Reconcile with habu-linear-ownership-for-1d7e0b63 before starting.

---
title: Six zero-arity calling routines emit a needless data-stack access
status: open
priority: 2
issue-type: task
created-at: "2026-08-09T23:10:26.155829+02:00"
---

Measured 2026-08-09: admitting ( -- )-with-call routines turns 6 definitions red with E-A64RAV-DKEEP (-8611, 'a data-stack access the emission had no reason to make'). Either the call-site builder emits a redundant store/load for this shape or the DKEEP clause is over-strict for it; not yet diagnosed. Reduce one, name the emitting site or the over-strict clause, fix at the root with a failing-then-passing test. Files: src/compiler/native/{select,regalloc-verify}.f. Depends: habu-declare-a-routine-0c14617b (the admission that exposes them).

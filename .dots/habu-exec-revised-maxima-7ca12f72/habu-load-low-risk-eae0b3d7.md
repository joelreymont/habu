---
title: Load low-risk missing modules unblocking obvious tests
status: open
priority: 2
issue-type: task
created-at: "2026-03-07T19:20:07.544473+01:00"
blocks:
  - habu-extend-maxima-post-2f5b7fb5
---

lib/maxima-loader.lisp plus ../maxima/src/mtrace.lisp, polynomialp.lisp, float-properties.lisp, hayat.lisp. Root cause: avoidable module absences still block trace, polynomial predicates, and Taylor entry points. Fix: load the low-risk subset or reclassify with concrete blockers. Why: raises coverage quickly before harder autoload/defint work.

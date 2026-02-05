---
title: CL spec parity
status: open
priority: 1
issue-type: task
created-at: "2026-02-05T12:10:38.483985+01:00"
---

Context: docs/cl-symbols.md shows 13 missing + 10 partial ANSI CL external/audit symbols; docs/PROGRESS.md and docs/cl-spec-status.md are inconsistent. Goal: reconcile trackers, implement missing symbols (equalp, integerp, realp, fdefinition, get-setf-expansion, function-lambda-expression, copy-structure, method-combination helpers, load-logical-pathname-translations), tighten partial (&environment/&whole/&allow-other-keys, OPTIMIZE qualities), add tests, commit+push per dot.

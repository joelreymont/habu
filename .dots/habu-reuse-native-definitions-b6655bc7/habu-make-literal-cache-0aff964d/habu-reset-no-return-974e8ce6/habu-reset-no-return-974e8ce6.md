---
title: Reset no-return query scratch
status: closed
priority: 2
issue-type: task
created-at: "2026-10-19T13:02:17.810842+03:00"
closed-at: "2026-10-20T18:19:58.664370+03:00"
close-reason: "Integrated reviewed implementation passes the final native registry: 685/685, exit 0, on a30a9b64 engine and 7557c59b map. Existing engine early/middle/late edit E2E passes cached/cold executable and map equality, runtime and rejection checks; five c5 generations match, and the final master-reconciled a30 product matches its ordinary self-build with cold-prefix boot passing. Final native log: /Users/joel/.cache/tmp/carl-observed-runtime.THrB7k/final-a30/native.stdout. This closes the implemented correctness fix; it does not claim subsecond edits, a five-minute suite, or unfinished optional optimizations."
---

Decoded cold/warm application images have identical code and dictionary but differ in exactly two DATA cells: NORET-CTL is0 versus8 and NORET-FMEND is0 versus927800. Both are checker query temporaries, not semantic state. Classify them at the existing compiler-work boundary, preserving live query/recursive-save behavior. Existing literal and body E2E image equality is the regression; require matching native replay equality and independent review.

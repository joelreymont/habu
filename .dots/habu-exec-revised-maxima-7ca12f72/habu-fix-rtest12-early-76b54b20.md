---
title: Fix rtest12 early abort in Poisson/ratsimp path
status: open
priority: 1
issue-type: task
created-at: "2026-03-07T19:32:55.754066+01:00"
blocks:
  - habu-restore-share-backed-5d0eae40
---

lib/maxima-loader.lisp:70-90; ../maxima/src/pois2.lisp; ../maxima/src/pois3.lisp; canonical test path from PLAN 3.5. Root cause: after pois2/pois3 loading, rtest12 still aborts around the expected-value read/eval path near the current T76 failure. Fix: convert the early abort into normal pass/fail accounting and then close the underlying evaluator/read-time issue. Why: this is a named Stage-3 blocker in the reviewed plan.

---
title: Fix Maxima partition/simplification hang family
status: open
priority: 1
issue-type: task
created-at: "2026-03-07T19:32:55.777729+01:00"
blocks:
  - habu-adopt-canonical-test-a8a0cbe4
---

../maxima/src/simp.lisp and related simplification callers. Root cause: partition(2*a*x*f(x),x) and adjacent simplifier paths still hang. Fix: add a focused repro, stop the non-terminating loop, and reopen rtest5-class paths. Why: another main Stage-3 hang blocker.

---
title: Fix Maxima limit hang family
status: open
priority: 1
issue-type: task
created-at: "2026-03-07T19:32:55.771464+01:00"
blocks:
  - habu-audit-and-harden-5576b7ee
---

../maxima/src/limit.lisp:119-260 and ../maxima/src/tlimit.lisp. Root cause: limit(e^x/x, x, inf) and related cases still hang in recursive simplification/limit evaluation. Fix: add a focused repro, stop the non-terminating path, and reopen rtest9. Why: one of the main Stage-3 hang blockers.

---
title: "Add :count/:key to substitute and nsubstitute for Maxima paths"
status: open
priority: 3
issue-type: task
created-at: "2026-03-07T19:34:58.953862+01:00"
blocks:
  - habu-decompose-remaining-per-0c9e465d
---

lib/stdlib.habu substitute/nsubstitute implementation. Root cause: CL sequence keyword coverage is incomplete and Maxima may depend on :count/:key behavior in correctness paths. Fix: implement the missing keyword semantics and add focused regressions. Why: late-stage correctness closure under the single Maxima tree.

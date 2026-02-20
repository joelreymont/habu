---
title: Create cross-runtime GC benchmark pack
status: open
priority: 1
issue-type: task
created-at: "2026-02-20T08:55:19.495574+01:00"
blocks:
  - habu-reduce-barrier-and-254725b9
---

File: bench/gc.zig:1, bench/sbcl_gc.lisp:1, tools/gc-compare:1; cause: current scenarios are limited; fix: add mixed real workloads (lists, vectors, symbols, closures, maxima-like churn) with normalized configs; why: trustworthy parity signal.

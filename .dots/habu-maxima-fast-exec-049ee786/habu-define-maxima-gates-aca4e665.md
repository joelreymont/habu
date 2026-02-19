---
title: Define maxima gates
status: closed
priority: 1
issue-type: task
created-at: "\"2026-02-17T22:23:04.155200+01:00\""
closed-at: "2026-02-20T00:12:24.713631+01:00"
close-reason: machine-checkable gates exist in integration (maxima core subset loader + keyword controls) and lib/maxima-loader.lisp required-binding checks
---

src/tests/integration.zig:1, lib/maxima-loader.lisp:1. Cause: no single pass/fail gate for load+CAS entrypoints+perf. Fix: add machine-checkable gates and acceptance scripts.

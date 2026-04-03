---
title: Phase 0 remove false progress
status: closed
priority: 1
issue-type: task
created-at: "\"2026-04-01T22:06:02.009376+02:00\""
closed-at: "2026-04-03T23:53:13.896219+02:00"
close-reason: "done: phase-0 false-progress leaves completed and closed across patches, stubs, fallback lookup, fake fasl, eval wrappers, and masking"
---

Problem: remove patch, stub, fallback, fake-FASL, wrapper, and masking layers so Maxima failures expose Habu root causes. Acceptance: Phase 0 leaves cover every false-progress layer in PLAN.md section 0.x. Files: PLAN.md:45-177, lib/maxima-*.lisp, src/interp/{repl,vm}.zig, src/runtime/heap.zig, src/compiler/compile.zig. Verify: dot tree habu-exec-converged-clean-536008dd shows Phase 0 leaves with causal blockers. Blockers: none.

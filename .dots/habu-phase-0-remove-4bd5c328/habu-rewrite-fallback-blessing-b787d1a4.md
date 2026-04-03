---
title: Rewrite fallback-blessing tests
status: closed
priority: 2
issue-type: task
created-at: "\"2026-04-01T22:06:02.059330+02:00\""
closed-at: "2026-04-03T23:49:18.321162+02:00"
close-reason: "done: lookup/package cutover regressions now assert exact CL-USER aliasing and exact per-package lookup without fallback drift"
blocks:
  - habu-make-lookup-apis-32266bf5
---

Problem: tests still encode old fallback behavior. Acceptance: package/lookup tests assert cutover semantics and fail if fallback paths reappear. Files: src/runtime/heap.zig:4684-4698, src/runtime/primitives/package.zig, src/interp/repl.zig, src/interp/vm.zig tests. Verify: focused zig tests for package and lookup suites. Blockers: habu-make-lookup-apis-32266bf5.

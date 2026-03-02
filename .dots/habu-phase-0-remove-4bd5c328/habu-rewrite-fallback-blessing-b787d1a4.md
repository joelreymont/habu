---
title: Rewrite fallback-blessing tests
status: open
priority: 2
issue-type: task
created-at: "2026-04-01T22:06:02.059330+02:00"
blocks:
  - habu-make-lookup-apis-32266bf5
---

Problem: tests still encode old fallback behavior. Acceptance: package/lookup tests assert cutover semantics and fail if fallback paths reappear. Files: src/runtime/heap.zig:4684-4698, src/runtime/primitives/package.zig, src/interp/repl.zig, src/interp/vm.zig tests. Verify: focused zig tests for package and lookup suites. Blockers: habu-make-lookup-apis-32266bf5.

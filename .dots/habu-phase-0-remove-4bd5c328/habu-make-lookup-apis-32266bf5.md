---
title: Make lookup APIs read-only
status: closed
priority: 1
issue-type: task
created-at: "\"2026-04-01T22:06:02.053669+02:00\""
closed-at: "2026-04-02T16:45:00.122895+02:00"
close-reason: done; package lookup is read-only, zig build ok, zig build test unchanged 5-error baseline
blocks:
  - habu-remove-legacy-lookup-e81bb093
---

Problem: package lookup mutates intern tables during read/resolve flows. Acceptance: lookup does not intern or repair package tables; only explicit intern/import/export mutates. Files: src/interp/repl.zig:1512-1541, src/runtime/heap.zig:3351-3372, src/runtime/primitives/package.zig:567-611,665-692. Verify: package lookup regressions prove no-mutation invariants. Blockers: habu-remove-legacy-lookup-e81bb093.

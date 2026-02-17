---
title: Adapt Habu to hoist API
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-02-17T11:21:56.797541+01:00\\\"\""
closed-at: "2026-02-17T11:37:31.106798+01:00"
close-reason: completed
blocks:
  - habu-audit-hoist-api-6ace8084
---

Implement required call-site/API updates in Habu for hoist changes before JIT crash work. Files: src/jit/backend.zig, src/interp/repl.zig, build.zig, build.zig.zon, tests touching hoist symbols/types. Add compile coverage.

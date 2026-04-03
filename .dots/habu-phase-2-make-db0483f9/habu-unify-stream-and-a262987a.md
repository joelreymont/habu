---
title: Unify stream and path primitive surface
status: closed
priority: 1
issue-type: task
created-at: "\"2026-04-01T22:06:02.211997+02:00\""
closed-at: "2026-04-03T10:17:58.251179+02:00"
close-reason: done (zig build ok; zig build test unchanged 5-error baseline)
blocks:
  - habu-canonicalize-loader-specials-4fcbd54f
---

Problem: duplicate stream or pathname entrypoints bypass canonical pathname-aware logic. Acceptance: VM opcodes and primitives route through one canonical surface. Files: src/runtime/primitives/io.zig, src/interp/vm.zig:1834-1843,5342-5348, lib/stdlib.habu pathname helpers. Verify: rg for duplicate bypass entrypoints and focused open/path tests. Blockers: habu-canonicalize-loader-specials-4fcbd54f.

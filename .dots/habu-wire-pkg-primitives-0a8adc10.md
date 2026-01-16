---
title: Wire package primitives to heap registry
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T11:01:07.390150+02:00"
---

src/runtime/primitives/package.zig:29 (findPackage): Currently returns error.NotImplemented. Change to call heap.findPackage(name).

Files: src/runtime/primitives/package.zig:29-35
Expected: findPackage returns heap Package value
Actual: returns error causing type mismatch
Fix: Replace stub with: return heap.findPackage(pkg_name_str) orelse Value.nil;
Verify: (find-package "HABU-USER") should return package object not error

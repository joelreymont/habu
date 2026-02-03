---
title: Fix GC scan CLOS
status: active
priority: 1
issue-type: task
created-at: "\"2026-02-03T10:39:56.553262+01:00\""
---

src/runtime/gc.zig:466-542: GC.scanObject(.class) misses cls.metaclass; GC.scanObject(.generic_function) misses gf.dispatcher. Fix: copyValue those fields. Add tests in src/runtime/gc.zig ensuring metaclass/dispatcher survive + relocate across 2 GCs. Verification: zig build test.

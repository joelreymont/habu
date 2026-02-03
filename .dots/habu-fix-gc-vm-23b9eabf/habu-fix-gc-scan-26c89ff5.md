---
title: Fix GC scan CLOS
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-02-03T10:39:56.553262+01:00\\\"\""
closed-at: "2026-02-03T10:47:20.655972+01:00"
close-reason: Scan cls.metaclass and gf.dispatcher; add GC regression tests
---

src/runtime/gc.zig:466-542: GC.scanObject(.class) misses cls.metaclass; GC.scanObject(.generic_function) misses gf.dispatcher. Fix: copyValue those fields. Add tests in src/runtime/gc.zig ensuring metaclass/dispatcher survive + relocate across 2 GCs. Verification: zig build test.

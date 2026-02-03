---
title: Fix GC/VM correctness
status: active
priority: 1
issue-type: task
created-at: "\"2026-02-03T10:39:50.215458+01:00\""
---

Context: src/interp/vm.zig:662; src/runtime/gc.zig:466; src/interp/repl.zig:516; src/compiler/compile.zig:1272. Goal: make GC sound (roots + pointer updates) across VM/REPL/compiler; add regression tests; keep perf.

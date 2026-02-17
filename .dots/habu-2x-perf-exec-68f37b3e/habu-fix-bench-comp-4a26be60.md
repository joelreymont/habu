---
title: Fix bench-comp JIT crash
status: closed
priority: 2
issue-type: task
created-at: "\"2026-02-17T11:08:27.553201+01:00\""
closed-at: "2026-02-17T11:58:12.352415+01:00"
close-reason: completed
blocks:
  - habu-rebaseline-perf-post-b340b0e2
---

Reproduce and fix bus error in comprehensive bench gcd path. Evidence: src/interp/vm.zig:718 (tryCallJit), src/jit/backend.zig:401-420 (CompiledFn.callFromValues), src/interp/repl.zig:2225-2239 (cross-call patching). Add regression test + rerun bench-comp.

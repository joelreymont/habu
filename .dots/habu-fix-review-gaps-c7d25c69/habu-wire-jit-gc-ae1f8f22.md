---
title: Wire JIT GC use
status: open
priority: 2
issue-type: task
created-at: "2026-02-01T22:30:05.964170+01:00"
blocks:
  - habu-add-jit-gc-d1af96a2
---

Context: src/jit/rt.zig:10-54, src/jit/jit.zig; cause: JIT helpers allocate without GC roots; fix: route allocs through GC-aware API and pass JIT roots; deps: habu-add-jit-gc-d1af96a2; verification: GC stress test

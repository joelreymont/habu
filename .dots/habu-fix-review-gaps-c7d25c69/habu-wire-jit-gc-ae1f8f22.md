---
title: Wire JIT GC use
status: closed
priority: 2
issue-type: task
created-at: "\"2026-02-01T22:30:05.964170+01:00\""
closed-at: "2026-02-01T22:44:31.033723+01:00"
close-reason: Use GC-aware add/sub/mul/div wrappers
blocks:
  - habu-add-jit-gc-d1af96a2
---

Context: src/jit/rt.zig:10-54, src/jit/jit.zig; cause: JIT helpers allocate without GC roots; fix: route allocs through GC-aware API and pass JIT roots; deps: habu-add-jit-gc-d1af96a2; verification: GC stress test

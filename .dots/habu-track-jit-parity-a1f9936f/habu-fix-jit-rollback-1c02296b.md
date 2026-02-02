---
title: Fix JIT rollback
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-02-02T22:28:12.030269+01:00\\\"\""
closed-at: "2026-02-02T22:43:25.424631+01:00"
close-reason: Rollback on JIT compile error + test
---

Context: src/jit/jit.zig:83-112; cause: compile errors leave code_buffer pos/writable state dirty; fix: snapshot pos/state + errdefer rollback + restore W^X + clear state; deps: none; verification: zig build test

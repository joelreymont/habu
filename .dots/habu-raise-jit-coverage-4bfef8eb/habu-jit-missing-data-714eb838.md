---
title: JIT missing data ops
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-02-17T22:23:16.862648+01:00\\\"\""
closed-at: "2026-02-22T08:43:02.863116+01:00"
close-reason: Implemented generic vec/hash/string/array helper lowering and backend regressions; full zig build test blocked by /Users/joel/Work/hoist/src/context.zig syntax error
---

src/jit/translate.zig op coverage. Cause: maxima hotspots use unlowered vector/hash/string ops. Fix: add IR/op lowering and parity tests.

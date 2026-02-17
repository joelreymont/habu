---
title: JIT missing data ops
status: open
priority: 1
issue-type: task
created-at: "2026-02-17T22:23:16.862648+01:00"
---

src/jit/translate.zig op coverage. Cause: maxima hotspots use unlowered vector/hash/string ops. Fix: add IR/op lowering and parity tests.

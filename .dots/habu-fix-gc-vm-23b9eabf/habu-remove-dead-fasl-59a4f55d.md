---
title: Remove dead fasl
status: closed
priority: 3
issue-type: task
created-at: "\"\\\"2026-02-03T10:40:52.793214+01:00\\\"\""
closed-at: "2026-02-03T16:39:48.888210+01:00"
close-reason: Remove unused fasl and legacy chunk type
blocks:
  - habu-unify-stdlib-paths-b99d770e
---

src/compiler/fasl.zig + src/bytecode/opcodes.zig Chunk struct: appears unused and mismatched with runtime.objects.Chunk; causes conceptual drift. Fix: either delete dead modules or wire into build with correct Chunk type. Verification: rg import audit + zig build test.

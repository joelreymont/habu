---
title: Remove dead fasl
status: open
priority: 3
issue-type: task
created-at: "2026-02-03T10:40:52.793214+01:00"
blocks:
  - habu-unify-stdlib-paths-b99d770e
---

src/compiler/fasl.zig + src/bytecode/opcodes.zig Chunk struct: appears unused and mismatched with runtime.objects.Chunk; causes conceptual drift. Fix: either delete dead modules or wire into build with correct Chunk type. Verification: rg import audit + zig build test.

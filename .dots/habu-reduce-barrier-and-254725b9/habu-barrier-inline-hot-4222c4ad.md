---
title: "Barrier: inline hot fast paths"
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-02-20T08:58:56.412987+01:00\\\"\""
closed-at: "2026-02-20T15:35:18.734872+01:00"
close-reason: Inline barrier fast-path guards on VM/JIT stores
blocks:
  - habu-barrier-profile-mutator-812522db
---

src/interp/vm.zig, src/jit: inline cheap barrier checks and reduce branches on clean stores.

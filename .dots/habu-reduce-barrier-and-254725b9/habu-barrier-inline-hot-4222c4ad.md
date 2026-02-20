---
title: "Barrier: inline hot fast paths"
status: open
priority: 1
issue-type: task
created-at: "2026-02-20T08:58:56.412987+01:00"
blocks:
  - habu-barrier-profile-mutator-812522db
---

src/interp/vm.zig, src/jit: inline cheap barrier checks and reduce branches on clean stores.

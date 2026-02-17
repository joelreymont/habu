---
title: RCA indirect-call paths
status: open
priority: 1
issue-type: task
created-at: "2026-02-17T22:23:16.848940+01:00"
---

src/interp/repl.zig: hoist JIT dispatch and src/interp/vm.zig call op paths. Cause: indirect-call/JIT path crashes under complex workloads. Fix: direct root-cause fix, no fallback masking.

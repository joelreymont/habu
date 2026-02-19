---
title: Cut VM+GC overhead
status: closed
priority: 1
issue-type: task
created-at: "\"2026-02-17T22:23:04.193427+01:00\""
closed-at: "2026-02-20T00:07:59.599364+01:00"
close-reason: sub-dots completed (GC root reduction, architecture upgrade, transient alloc shrink) with bench-check validation
blocks:
  - habu-raise-jit-coverage-4bfef8eb
---

src/interp/vm.zig and src/runtime/gc.zig. Cause: root-set assembly and allocation churn dominate long CAS runs. Fix: reduce roots overhead, avoid transient allocs, improve cache locality.

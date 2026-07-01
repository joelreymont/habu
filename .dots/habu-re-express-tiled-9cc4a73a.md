---
title: "Re-express tiled GEMM as a checked KERNEL: body"
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T18:10:55.997993+02:00"
blocks:
  - habu-checker-capability-typed-e0c76a02
  - habu-make-ptx-device-c0eb12a3
  - habu-add-ptx-planner-30b93e8c
---

File: PLAN.md:356. Gap: `lib/ptx/cg-matmul.f` still relies on raw emit
islands for the core tiled GEMM body. Fix: after typed loops, shared-memory
tiles, accumulators, and planner metadata exist, rewrite `EMIT-MATMUL` as a
checked `KERNEL:` body composed from typed tile words and remove the unchecked
boundary. Verify: checker certifies the body, emitted PTX remains equivalent for
the covered shapes, and the generic Orin matmul device test passes.

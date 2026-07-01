---
title: Add PTX planner DSL
status: open
priority: 1
issue-type: task
created-at: "2026-07-01T22:30:13.134216+02:00"
blocks:
  - habu-add-ptx-target-ba119d76
---

File: PLAN.md:192; cause: kernel lowering currently happens without a generic roofline/resource planner, so fusion, GEMM, attention, and autotune decisions become ad hoc; fix: add lib/ptx/plan.f plus tests and FILEMAP/gate wiring for elementwise, reduction, GEMM/GEMV, attention, dtype, alignment, resource, and overflow planning; deps: habu-add-ptx-target-ba119d76; verification: bin/hb --load lib/ptx/plan-test.f plus PTX stdlib lint slices classify memory-bound and compute-bound cases and reject unsupported shapes without CUDA.

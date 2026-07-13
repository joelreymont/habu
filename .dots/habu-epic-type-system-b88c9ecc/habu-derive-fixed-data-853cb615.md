---
title: Derive fixed DATA layout from typed schema
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T11:44:22.279603+02:00"
blocks:
  - habu-add-bounded-host-b40b048f
---

Context: src/habu/layout.f and native, recovery, regalloc, debug, task, FFI, and snapshot code claim fixed DATA cells and ranges with scattered numeric constants. Prior overlaps between evaluator frames, register tables, debugger state, and FFI scratch were found only at runtime. Cause: no single typed interval owner proves disjoint placement and native or bootstrap parity. Fix: create a DATA-LAYOUT package and checked schema that allocates named cells and ranges, derives offsets and typed accessors, proves bounds, alignment, adjacency, non-overlap, lifetime class, and protected-band membership, and emits or verifies both native and recovery constants from one authority. Acceptance: adjacent ranges pass; a one-byte overlap, out-of-band range, wrong alignment, duplicate owner, and native or recovery drift reject; all current DATA claims are enumerated; layout, fixpoint, snapshot, debugger, FFI, and task gates are green.

---
title: "Libraries: migrate PTX IR record"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T17:17:13.275171+02:00"
blocks:
  - habu-lowering-hash-unified-586f7881
---

Own lib/ptx/ir.f declaration/consumers and its focused PTX tests. Replace PRODUCT/raw structure declarations with typed STRUCTURE, preserving field order, widths, offsets, package API, emitted PTX semantics, and zero-allocation compiler paths. Validate ptx-stdlib and PTX toolchain slices.

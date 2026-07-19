---
title: Structure image dump dictionary rows
status: open
priority: 2
issue-type: task
blocks:
  - habu-lowering-hash-unified-586f7881
created-at: "2026-07-19T21:39:39.924253+02:00"
---

Evidence: tools/imgdump.f:40-43,268-275,324-384 decodes each dictionary entry into four parallel arrays for name pointer/length, start, and code length, exposes eight manual field accessors, then recombines columns for rendering and comparison. Same-width name length, start, and length swaps are checker-valid and can falsely report dictionary equality or attribute a program counter to the wrong range. Define one package-owned STRUCTURE dictionary-entry with named name span, start, and length, stored in LAYOUT-BUFFER; keep the snapshot wire format explicitly encoded and decoded only at the boundary. Preserve exact dump/compare bytes and snapshot/AOT behavior. Prove compile-negative cross-field swaps, exact dump and compare goldens, corrupt-entry rejection, PC-at-start/end boundaries, count/capacity/canaries, and current snapshot fixtures. Measure source/accessor definitions, JIT/DATA bytes, row storage, and dump/compare throughput before and after. Coordinate package ownership with habu-pkg-reusable-tools-84cca130; snapshot wire versioning remains with habu-snapshot-format-ver-f33c796f.

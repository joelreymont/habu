---
title: "Lowering: consume unified fields"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T17:14:57.178594+02:00"
blocks:
  - habu-enum-infer-canonical-f07a77c2
---

Own native metadata consumers for STRUCTURE layout and ENUM construct/MATCH in src/habu/habu2.f plus focused lowering tests. Replace PF/positional payload reads with shared named field ranges while preserving emitted cell order, widths, tags, generic substitutions, and allocation-free hot paths.

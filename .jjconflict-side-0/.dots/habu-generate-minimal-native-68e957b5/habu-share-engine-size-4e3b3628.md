---
title: Share engine size row structure
status: open
priority: 2
issue-type: task
blocks:
  - habu-lowering-hash-unified-586f7881
created-at: "2026-07-19T21:36:10.195572+02:00"
---

Evidence: src/habu/engine-size.f:9-83 and tools/size-report.f:36-68,119-128 implement the same logical name-span plus size/end row twice as three parallel arrays, with duplicated SLOT, VALIDATE, accessors, and append plumbing. The producer stores cumulative end and derives bytes; the consumer stores bytes. Define one canonical checked STRUCTURE size-row/schema with a named name span and one unambiguous byte-count/end representation, store it in LAYOUT-BUFFER, and share validation and parsing contracts without coupling engine emission to report rendering. Preserve the exact emitted map, every total, duplicate/unknown-row behavior, capacity errors, page/container classification, and public API. Prove byte-exact map/report goldens, all totals, malformed parse rollback, max-capacity canaries, and compile-negative name/count/end swaps. Measure source definitions, JIT/DATA/CODELEN, row storage, and report time before and after. Serialize tools/size-report.f edits after habu-fix-size-report-1f35ca64; size ratchet dots retain ownership of measured bounds and attribution.

---
title: Structure standard library manifest rows
status: open
priority: 1
issue-type: task
blocks:
  - habu-lowering-hash-unified-586f7881
created-at: "2026-07-19T21:36:26.080502+02:00"
---

Evidence: tools/stdlib-manifest-test.f:76-96,182-224,299-390 maintains 18 parallel offset/length columns for manifest words, public words, module files, library files, and temporary fields, plus eight handwritten span accessors and four manual append paths. Each word row is three indistinguishable spans for file, word, and effect, so a stack or target-array swap is checker-valid and can falsely report manifest parity. Define a checked STRUCTURE span plus named manifest-word, public-word, and file rows stored in LAYOUT-BUFFER; make row decoding transactional and return option from lookup. Preserve exact manifest/public-signature parity and diagnostics. Prove compile-negative adjacent span swaps, malformed 11-field rows leave every count/arena unchanged, duplicate/missing/extra coverage classifications, exact diagnostics, current manifest parity, capacity/canaries, and full-row behavior. Measure definitions, source/JIT/DATA bytes, table storage, and runtime before and after.

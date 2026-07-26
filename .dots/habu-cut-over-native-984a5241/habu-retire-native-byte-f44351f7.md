---
title: Retire native byte scans
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T23:00:00.488489+02:00"
blocks:
  - habu-cache-and-tree-5c4a1a24
---

Full context: complete Wave 7 by routing current AOT and REPL build paths through HBOBJ 2 and deleting BL/literal-stencil decoding and direct address inference. Acceptance: inventory gates find no production byte-scan owner; current AOT/REPL/object tests and cross-target gates pass from explicit relocations.

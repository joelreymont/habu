---
title: Structure suite coverage rows
status: open
priority: 1
issue-type: task
blocks:
  - habu-lowering-hash-unified-586f7881
created-at: "2026-07-19T21:36:43.040780+02:00"
---

Evidence: tools/suite-coverage-lint-core.f:48-54,85-129,276-288 stores each suite member as four parallel pointer/length columns for label and file, and each in-process PTX entry as raw offset/length columns. A label/file or pointer/length store swap is checker-valid and can assign a member to the wrong suite or hide an orphan. Define checked STRUCTURE suite-member with label span and file span, a checked arena-span PTX entry, and LAYOUT-BUFFER storage; make every append transactional. Preserve exact scheduled/manual/spawn-only classification and diagnostics. Prove compile-negative span/field swaps, every orphan/missing/extra classification, malformed parse leaves rows and arenas unchanged, exact diagnostics, current coverage inventory, capacity/canaries, and full-table behavior. Measure definitions, source/JIT/DATA bytes, storage, and scan time before and after.

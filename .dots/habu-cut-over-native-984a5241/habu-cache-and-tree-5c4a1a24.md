---
title: Cache and tree-shake HBOBJ 2
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T23:00:00.475207+02:00"
blocks:
  - habu-link-hbobj-2-8e1b9ba1
---

Full context: Wave 7 requires AOT tree shaking and object caches over structured symbols/relocations. Keys bind source, checker, compiler, target, numeric policy, schema, and pass pipeline. Acceptance: any key field changes identity; closure walks symbolic references only; current cache/relink/AOT behavior passes without scanning machine bytes.

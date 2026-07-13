---
title: "Lowering: order unified declarers"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T17:15:19.078879+02:00"
blocks:
  - habu-lowering-consume-unified-3265d046
---

Own native source/load ordering, manifests, and focused closure tests for the new declaration files. Load STRUCTURE/ENUM only after checker/type-family/shared fields, keep encoder/target closures deterministic, and prove no declaration parser is silently absent in stage, stdin, snapshot, or build modes.

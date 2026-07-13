---
title: "Core records: remove schema boot DSL"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T17:15:42.353492+02:00"
blocks:
  - habu-lowering-hash-unified-586f7881
---

Own raw record declarations in src/core/type-schema.f and src/core/type-family.f. Break the pre-checker bootstrap cycle by replacing BEGIN-STRUCTURE layouts with named offset/size/alignment constants and compile-time layout assertions, not a second definer. Preserve every ABI and focused family/schema test.

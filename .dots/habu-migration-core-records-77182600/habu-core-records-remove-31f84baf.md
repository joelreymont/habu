---
title: "Core records: remove schema boot DSL"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T17:15:42.353492+02:00"
---

Own the seven private record declarations in src/core/type-schema.f and
src/core/type-family.f. Replace BEGIN-STRUCTURE layouts with named cell/byte
offsets, named strides, ordinary accessors, and load-time offset, size,
alignment, and pointer-role assertions. Preserve every ABI and focused
family/schema test. Add no parser, definer, descriptor, adoption, family,
reflection, snapshot, or AOT surface.

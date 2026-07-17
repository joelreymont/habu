---
title: Generalize field layout policy checks
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-17T10:43:41.856273+02:00\""
---

Replace stack/CELL-only PF layout validation with policy-specific STACK, PACKED, NICHE, BOXED, and CUSTOM validation in src/core/type-family.f. Add positive and negative focused cases for every policy in test/type-family-suite.f. Preserve the single PF arena and explicit layout metadata contract. Parent review finding for habu-fields-add-shared-6b063c62.

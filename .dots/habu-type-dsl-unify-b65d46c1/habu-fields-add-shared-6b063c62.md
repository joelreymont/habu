---
title: "Fields: add shared schema arena"
status: active
priority: 1
issue-type: task
created-at: "2026-07-13T17:12:36.155644+02:00"
blocks:
  - habu-migration-core-records-77182600
---

Own the in-place PF evolution in src/core/type-family.f and its focused suite. The existing PF arena is the sole field truth: key each committed row by owner family, optional variant, and TF-INTERN canonical name; store schema root, explicit cell slot/count, byte offset/size, alignment, and flags. Reuse TF-INTERN, TF-OFF$, and the existing type-family snapshot/rollback high-water integration. Provide one linear transaction carrying PF-row and interned-string watermarks, one atomic validated ADD, COMMIT/ROLLBACK with commit-only field IDs, and sealed typed FIND/EACH/reflection APIs.

Validate schema trees recursively against owner-family arity, including APP family argument cardinality and argument-index ranges. Validate explicit layout metadata without stack-only or CELL hardcoding; cover stack, packed, niche, boxed, and custom policies. Preserve existing PF consumers or migrate them atomically to the evolved record so no parallel field arena or name pool remains. Remove src/core/type-field.f and its parallel arena/name pool, SOURCE/provenance, per-field visibility, parser entrypoints, contiguity rule, staged draft/state machinery, redundant LAYOUT resubmission, declaration-seam surgery, and associated roles/TRUSTED/tests.

Tests must cover duplicate and reserved names; invalid schema owner, arity, and APP shapes; negative layout cases for every policy; nested transaction rollback restoring PF and string high-water marks; commit-only IDs; optional-variant ownership; and sealing/reflection. Validate focused loads plus type-family consumers, typed-local-diff-lint, trust inventory/lint, shadow lint, host-lint, and filemap-lint. Claim: agent=habu-fields-schema-v2 workspace=.jj-ws/habu-fields-schema-v2.

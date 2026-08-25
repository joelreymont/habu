---
title: Delete legacy variant surfaces
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T17:19:28.809111+02:00"
blocks:
  - habu-delete-legacy-record-ab976a39
  - habu-migration-core-variants-af8e09b4
---

Hard-delete TYPEFAMILY, SUMTYPE, ;SUMTYPE, positional VARIANT payload parsing,
separate payloadless legacy branches, ENUM+, ENUM4+, src/core/enums.f, duplicate
family/variant registries, checker/native/recovery events, generated-source
emitters including test/export-package.f, effects, TRUST rows, snapshot/AOT
rows, and aliases. Keep internal TK-SUM/TK-ENUM algebra only, precise
error-only tombstones, and explicitly allowlisted non-executable rejection data.

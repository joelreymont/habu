---
title: Record field visibility and source provenance
status: open
priority: 1
issue-type: task
created-at: "2026-07-18T14:16:55.285454+02:00"
blocks:
  - habu-make-field-rollback-ecb282e9
---

Full context: src/core/type-family.f PF row storage/reflection currently records owner, name, schema, offsets, width, alignment, and flags but omits the unified DSL contract's exact package visibility and declaration source span/provenance. src/core/sumtype.f declaration parsing and later STRUCTURE/ENUM lowering therefore cannot use one authoritative field row for private/public access control or precise diagnostics. Extend the shared PF schema with package identity plus public/private visibility and stable source file/line/span provenance; populate it transactionally at declaration parse time; expose only typed safe reflection; include the metadata in validation, rollback, hashing, snapshot, AOT, replay, and fixpoint identity. Add checked tests for private/public fields, package reopen/qualification, rollback leaks, malformed declarations reporting the exact field span, snapshot/AOT/replay stability, and generated STRUCTURE/ENUM consumers. Verify type-family/type-decl/package/diagnostic suites, typed-local diff lint, snapshot/AOT/fixpoint, and full native gate. Ownership: shared field-row metadata and reflection only; package-wide nominal type scoping remains habu-scope-declared-types-984c5202.

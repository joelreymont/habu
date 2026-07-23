---
title: "ENUM: expose provisional payload"
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-23T14:41:36.124396+02:00\""
---

Why: constructor generation runs before declaration publication, but committed SUMV-PAY-* readers cannot see the current field transaction. Calling them early is wrong; copying rows would create a second authority.

Owner and interface: package TYPE-FIELD-OWNER adds only TX-CELLS-FOR (field-token family field-id -- cells), implemented by calling the existing PF-TX-SCHEMA-FOR validator before reading PF.CELLS. Package DECL-EVENT owns the public declaration-token view: PAYLOAD-N (decl-token family variant-id -- n), PAYLOAD-SCHEMA@ (decl-token family variant-id index -- schema), PAYLOAD-WIDTH@ (decl-token family variant-id index -- n), and PAYLOAD-CELLS (decl-token family variant-id -- n). No pointer, raw row, field token, registry cursor, or mutable authority escapes.

Behavior: every call requires the exact live top declaration token and its bound family; the variant must be a variant event owned by that frame; index is declaration-order among that variants field events. Schema and width come from the sole provisional TYPE-FIELD row selected by the event. PAYLOAD-CELLS is the checked sum of widths. Stale, nested-foreign, wrong-family, wrong-variant, non-field, negative, and one-past indices reject before reading an unauthorized row. Published-only or rolled-back rows are never visible. Preserve empty-variant count and zero width. No copied descriptor, positional fallback, public cast, trust site, legacy SUMTYPE/PRODUCT edit, constructor generation, reflection, or caller migration.

Acceptance: red-first tests call these exact public words through the real ENUM-DECL declaration transaction before publication; two variants with distinct ordered named fields prove order, schemas, per-field widths, totals, and empty payload. Hostile stale, nested, wrong-family/variant/index, post-publish, and post-rollback calls reject with the existing declaration/field owner codes. Mutating event order, field family, field id, or width makes the focused suite fail. Run decl-event, enum-decl, generated-declaration transaction, type-family rollback, exact typed-local and package diff lints, strict trust, candidate validation, and native fixpoint. Files: src/core/type-family.f, src/core/decl-event.f, their focused suites, and inventories only if required. Smallest owning path: bin/hb --load test/enum-decl-suite.f.

Claim: agent=enum_provisional workspace=.jj-ws/habu-enum-expose-provisional-bcb7c765.

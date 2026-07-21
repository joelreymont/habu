---
title: Record field visibility and source provenance
status: open
priority: 1
issue-type: task
created-at: "2026-07-18T14:16:55.285454+02:00"
blocks:
  - habu-exhaust-field-reserved-9f0b6bcf
  - habu-stable-source-origin-frame-9d4b2a61
---

Full context: `src/core/type-family.f` PF row storage/reflection currently
records owner, name, schema, offsets, width, alignment, and flags but omits the
unified DSL contract's exact package visibility and declaration source
provenance. `src/core/sumtype.f` declaration parsing and later STRUCTURE/ENUM
lowering therefore cannot use one authoritative field row for private/public
access control or precise diagnostics.

Acceptance: after the field-schema and reserved-name leaves land, extend the
shared PF schema with package identity, public/private visibility, and a stable
source-origin identifier supplied by `habu-stable-source-origin-frame-9d4b2a61`.
Populate all fields in the declaration transaction before publishing the row.
Expose only typed read-only reflection. Package visibility participates in
semantic field/type identity. Source provenance is validated and serialized for
evidence and diagnostics but excluded from semantic family, field, constructor,
and layout hashes; moving identical source must not change its type. Prove
private/public fields, package reopen and qualification,
rollback without leaked rows, nested include/evaluate declarations retaining
their original file/line/span, snapshot/replay stability, and generated
STRUCTURE/ENUM consumers.

Files: `src/core/type-family.f`, the declaration producer that commits PF rows,
and focused type-family/declaration/provenance tests. Verify the type-family,
type-declaration, package, diagnostic, snapshot, ahead-of-time, replay, and
fixpoint suites plus typed-local, package, host, filemap, and dot lints.
Ownership: shared field-row visibility/provenance storage and typed reflection
only. Source-frame capture belongs to `habu-stable-source-origin-frame-9d4b2a61`;
package-wide nominal type scoping remains `habu-scope-declared-types-984c5202`.

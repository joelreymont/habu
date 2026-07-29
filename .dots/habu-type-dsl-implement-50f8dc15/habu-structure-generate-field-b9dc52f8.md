---
title: "STRUCTURE: generate field accessors"
status: open
priority: 1
issue-type: task
created-at: "\"\\\"2026-07-13T17:13:45.006634+02:00\\\"\""
blocks:
  - habu-structure-generate-make-872a6e75
  - habu-checker-type-structure-d996215b
  - habu-record-field-visibility-7bb1f8a7
---

Own typed field-accessor generation after field visibility/provenance and the
shared publication transaction land. Generate sealed `FAMILY:FIELD` words with
effect `ptr family<args> -- ptr field-type`, using committed shared byte offsets,
generic schema instantiation, and the landed schema-aware checker projection.
Reject value/pointer role confusion and prove nested, byte, pointer, alignment,
package visibility, source diagnostic, rollback, snapshot, and ahead-of-time
cases. Rewire the normal `;STRUCTURE` path so `DECL-EVENT` publication, the
already-landed `FAMILY:MAKE`/`FAMILY:UNMAKE` pair, and the complete accessor set
are one shared `habu-atomic-generated-declaration-4c1e8b7a` transaction. A
failure after MAKE/UNMAKE or any accessor leaves no declaration/event, word,
signature, seal, package, provenance, or registry residue. Checking only the
first field, publishing one accessor at a time, or making only the accessor
subset atomic is forbidden.

Claim: RELEASED 2026-07-21. The `genfield` workspace is preserved as evidence,
but its implementation is obsolete and must never merge: it introduced a
pre-trust `defer` at a forbidden load point, was not wired into the declaration
path, omitted synchronized baked-file owners, and lacked set-wide atomicity.
The earlier lane's useful design evidence remains valid only after the now-landed
`habu-checker-type-structure-d996215b` projection and the new prerequisites above.

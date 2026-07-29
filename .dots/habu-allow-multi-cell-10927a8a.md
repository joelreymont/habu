---
title: Allow multi-cell structures as typed locals
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T10:15:47.693411+02:00"
---

Full context: a multi-cell STRUCTURE type cannot be used as a typed local. Writing {: sn:IR-SOURCE:span :} is rejected with 'unknown type in signature' even though the same type name works in a stack effect, so callers must thread multi-cell values through the stack instead of naming them — found while writing test/compiler/ir-op.f, which passes source spans on the stack for this reason. Determine whether this is an intended restriction of the typed-locals implementation or an omission, then either support multi-cell structures as locals or produce a diagnostic that names the real limitation instead of claiming the type is unknown. Acceptance: either a typed local of a multi-cell STRUCTURE compiles and round-trips its fields, or the rejection carries an accurate named diagnostic and docs/forth.md records the restriction.

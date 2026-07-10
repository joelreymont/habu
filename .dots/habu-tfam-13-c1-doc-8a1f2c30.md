---
title: "TFAM 13 C1: document ptr-dup vs n-reserved declaration divergence"
status: open
priority: 4
issue-type: task
created-at: "2026-07-10T12:00:00.000000+02:00"
---

Destruction-review finding C1 from dot habu-tfaam-13-adt-5d3288f0 (S2 audit,
2026-07-10). `TYPEFAMILY ptr 0` rejects as E-TFAM-DUP (duplicate family, 7102)
while `TYPEFAMILY n 0` rejects as E-TDECL-NAME (reserved name, 7110). This is
NOT a bug: `ptr` genuinely IS a registered parametric family (so duplicate is
the correct classification), whereas `n` is a concrete cell type gated by
TDECL-RESERVED? (src/core/sumtype.f). Do NOT reclassify `ptr` -- that would be
wrong. Document the divergence in docs/type-families.md §24 (or §1 name-gate
prose): a reserved TYPE token that is also a live family name reports the dup,
and a reserved concrete-cell token reports reserved-name. Low priority; purely
a doc clarification, no code change.

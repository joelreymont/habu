---
title: Make ENUM field registration linear
status: closed
priority: 2
issue-type: task
created-at: "\"2026-10-01T05:47:41.051711+02:00\""
closed-at: "2026-10-01T18:00:16.824201+02:00"
close-reason: Fixed by mwksxnws f934f391 (review 139, fold review 202 ACCEPT)
---

Problem: after habu-make-enum-registration-fbc60cf8, an ENUM with one field per variant still registers in roughly quadratic time (100/650/2600/5200 variants: 0.04/0.21/1.61/5.45 s, measured by the r4-enum lane): in src/core/type-family.f PF-DUP? and PF-OVERLAP? walk every field row on each field added, and SUMV-PAY-N, SUMV-NAMED-FIELD and SUMV-PAYCELLS@ walk the family's fields on every call; the name preflight's XREF-FIND-TARGET-INDEX walks the whole dictionary for each generated name (about 0.5 s at 2600 variants). Acceptance: attribute the remaining growth by measurement; make ENUM-with-fields registration linear (or n log n) in variants, using a (family, variant) to field-row index built like the fbc60cf8 variant index and the existing indexed lookup for names; identical declarations and diagnostics (the fbc60cf8 dump fixture cmp-identical, the rejected-declaration set unchanged); times for 325 to 5200 variants before and after. Files: src/core/type-family.f, src/core/sumtype.f if needed, the name preflight. Verify: enum and declaration suites (whitebox rows run the way the gate runs them), tools/check-test.f, native build convergence, two-generation build. Depends: habu-make-enum-registration-fbc60cf8. Ownership: ENUM field registration cost.

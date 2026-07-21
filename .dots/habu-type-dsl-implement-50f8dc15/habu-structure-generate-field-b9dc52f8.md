---
title: "STRUCTURE: generate field accessors"
status: open
priority: 1
issue-type: task
created-at: "\"2026-07-13T17:13:45.006634+02:00\""
blocks:
  - habu-structure-generate-make-872a6e75
  - habu-checker-type-structure-d996215b
---

Own typed field accessor generation and focused tests. Generate sealed FAMILY:FIELD words with effect ptr family<args> -- ptr field-type, using shared byte offsets and generic schema instantiation. Reject value/pointer role confusion and prove nested, byte, pointer, alignment, and package-visibility cases.

Claim: RELEASED 2026-07-21 (agent=genfield stopped with decisive structural evidence, NO edits): a checked FAMILY:FIELD accessor ( ptr family<args> -- ptr field-type ) cannot certify on the landed checker - pointer arithmetic on layout pointers is fail-closed (LAYOUT-BLOCK gate, pointee-preserving + arm), no ptr-to-ptr cast exists (CAST refuses T-PTR), and the three armed windows (LAYOUT-INTRO, CTOR-PEND, CAST-PEND) structurally exclude the field-projection shape. The missing capability - a sealed schema-aware field projection minting ptr field-type from ptr family + committed byte offset under a generator-keyed armed window - is habu-checker-type-structure-d996215b (checker-certify epic). Full generator design recorded in the lane report: src/core/structure-field.f, STRUCTURE-FIELD:GENERATE ( fam -- ), SM-idiom validation (E-SF-FAM/E-SF-EMPTY/E-PF-ID/E-SF-DUP) then one infallible SF-EMIT reading TYPE-FIELD:BYTE-OFF@/SCHEMA@, rendered accessors driven through the generative eval crossing under the new armed window. Re-dispatch AFTER d996215b lands.

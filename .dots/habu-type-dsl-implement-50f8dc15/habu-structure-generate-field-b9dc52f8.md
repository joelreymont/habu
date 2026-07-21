---
title: "STRUCTURE: generate field accessors"
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-13T17:13:45.006634+02:00\""
blocks:
  - habu-structure-generate-make-872a6e75
---

Own typed field accessor generation and focused tests. Generate sealed FAMILY:FIELD words with effect ptr family<args> -- ptr field-type, using shared byte offsets and generic schema instantiation. Reject value/pointer role confusion and prove nested, byte, pointer, alignment, and package-visibility cases.

Claim: agent=genfield workspace=.jj-ws/habu-structure-generate-field-b9dc52f8 (Mac; EARLY START against the landed TYPE-FIELD/decl-event/structure-make contracts: builds the sealed FAMILY:FIELD accessor generator in a NEW src/core file with decl-event-driven fixtures. The blocker edge on generate-make governs CLOSURE: assembly rows and any front-end wiring land in the reconciliation after the generate-make reconcile lane merges. Write set: the new module + its suite + FILEMAP/TRUSTED/candidate-validation rows only - disjoint from the reconcile lane (structure-decl.f, structure-make.f, assembly paths).)

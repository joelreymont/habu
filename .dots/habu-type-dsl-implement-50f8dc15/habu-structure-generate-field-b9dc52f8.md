---
title: "STRUCTURE: generate field accessors"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T17:13:45.006634+02:00"
blocks:
  - habu-structure-generate-make-872a6e75
---

Own typed field accessor generation and focused tests. Generate sealed FAMILY:FIELD words with effect ptr family<args> -- ptr field-type, using shared byte offsets and generic schema instantiation. Reject value/pointer role confusion and prove nested, byte, pointer, alignment, and package-visibility cases.

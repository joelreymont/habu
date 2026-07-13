---
title: "EPIC: one STRUCTURE and ENUM type DSL"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T16:45:07.581630+02:00"
---

Hard-cutover program. Expose exactly one record declaration surface, STRUCTURE ... ;STRUCTURE, and one variant declaration surface, ENUM ... ;ENUM. STRUCTURE owns typed named fields, generics, checked MAKE/UNMAKE, reflection, and layout policy while lowering to the internal product representation. ENUM owns payloadless and payload-bearing named variants, uses the same field schema, and lets the compiler derive tag-only versus tagged-union representation. Remove PRODUCT, SUMTYPE, VALUE-RECORD, BEGIN-STRUCTURE, END-STRUCTURE, and every compatibility alias after tree-wide migration. No compatibility mode, fallback, or deprecated parser path. Parent: habu-epic-model-cad-70b629a9.

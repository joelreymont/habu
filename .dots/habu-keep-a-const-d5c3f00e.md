---
title: Keep a constant annotation named like a restriction
status: open
priority: 2
issue-type: task
created-at: "2026-11-27T14:57:29.845534+01:00"
---

Problem: src/lib/odata/metadata.f IN-ANNOTATIONS picks the restriction path from the term's name alone, before its namespace or target kind is known, and BLOCK drops restrictions for a type or property target; so an <Annotations> Annotation whose term is named FilterRestrictions (and, with ODATA:SORTABLE?, SortRestrictions) in any namespace with a constant value attribute is dropped, though src/lib/odata.f:11-15 says such an annotation is kept (probe: Term="N.SortRestrictions" String="x" on type N.A, TYPE-ANNOTATION$ answers nothing). Acceptance: such an annotation is kept and TYPE-ANNOTATION$ answers its value; a Record-valued Capabilities restriction is read as before; a test in src/lib/odata-test.f fails before. Depends: none.

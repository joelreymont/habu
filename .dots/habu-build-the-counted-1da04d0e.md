---
title: Build the counted loop inside a quotation body
status: open
priority: 2
issue-type: task
created-at: "2026-08-13T21:45:23.394952+02:00"
---

Found by the model-j lane, pre-existing: a counted loop inside a quotation refuses E-IR-VERIFY-DOM (-8091) single, E-IR-VERIFY-SUCCARG (-8088) nested; even [: 7 ;] execute in its simplest form is E-NELAB-QUOT (-8651). Sibling of the quotation ceilings family (fc37262a control-in-body, 7578eaaa calling-under-locals - one root may own several, the quot-scope lane is diagnosing now); reconcile with its findings before implementing. Files: src/compiler/native (quotation build path). Depends: sequencing with 7578eaaa/fc37262a.

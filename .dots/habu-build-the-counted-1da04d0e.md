---
title: Build the counted loop inside a quotation body
status: open
priority: 2
issue-type: task
created-at: "2026-08-13T21:45:23.394952+02:00"
---

Found by the model-j lane, pre-existing: a counted loop inside a quotation refuses E-IR-VERIFY-DOM (-8091) single, E-IR-VERIFY-SUCCARG (-8088) nested; even [: 7 ;] execute in its simplest form is E-NELAB-QUOT (-8651). Sibling of the quotation ceilings family (fc37262a control-in-body, 7578eaaa calling-under-locals - one root may own several, the quot-scope lane is diagnosing now); reconcile with its findings before implementing. Files: src/compiler/native (quotation build path). Depends: sequencing with 7578eaaa/fc37262a.

LIKELY SAME ROOT as habu-let-a-quotation-fc37262a (audit
2026-08-13): the refusal codes here (-8091 DOM single loop, -8088
SUCCARG nested) are the block-ordinal signature the quot-scope
diagnosis explained - a body's successor naming a block of the
enclosing routine, surfacing as whatever check its collision
happened to trip. The succ-ord lane's landing should convert both;
re-measure this leaf's reproducers on that landing before doing any
work here, and close as duplicate if they compile or move to one
uniform named refusal.

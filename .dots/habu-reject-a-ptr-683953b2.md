---
title: Reject a pointer to a view at its pointee
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T04:56:07.142605+01:00"
---

Problem: 'STRUCTURE prv<p,q> FIELD x ^read-view<p,q,n> ;STRUCTURE' (/tmp/jerry-kinds/prv.f) is rejected at the field name x by DECL-EVENT:FIELD's E-PF-SCHEMA throw (src/core/decl-event.f:176) with 'type parameter has incompatible kinds', though no parameter is in two kinds: the cause is type-family.f PF-NODE-KIND?'s pointer case, which refuses a pointee family with a scope or region parameter. Acceptance: the record is placed at the pointee family token, its label says a pointer cannot point at a family with a scope or region parameter, its help names a form that loads; the same for ENUM and nested pointees; the same for a pointer to the family being declared, which a later field gives a scope or region parameter: 'STRUCTURE node31<q> FIELD next ^node31<n> FIELD w read-view<q,q,n> ;STRUCTURE', 'ENUM node33<q> VARIANT one FIELD next ^node33<n> FIELD w read-view<q,q,n> ;VARIANT ;ENUM' and 'STRUCTURE node<a,q> FIELD next ^node<a,q> FIELD w read-view<q,q,a> ;STRUCTURE' (today rejected at the family name by DECL-EVENT:REQUIRE-KINDS at close, the last with a label whose help never loads); once no kind conflict reaches FIELD or the close, FIELD's E-PF-SCHEMA throw, REQUIRE-KINDS, their constant and text are deleted. Depends: none. Claim: unassigned.

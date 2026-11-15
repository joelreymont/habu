---
title: Help NEWTYPE and DEFTYPE wrap bases by their own forms
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T23:18:47.653702+01:00"
---

Problem: the NEWTYPE/DEFTYPE wrap reuses STRUCTURE's field records: 'DEFTYPE PTR' with no base goes through CHECKER-DEFTYPE and prints 'bad newtype declaration' (test/deftype-suite.f pins it); 'DEFTYPE path path' gets FIELD-FAM?'s 'recursively name its owner' with a ^ help; 'DEFTYPE x a' and 'NEWTYPE x a' get SD-PARAM-FIX$ ('list the parameter in the header, as in STRUCTURE box<a>'); the DEFTYPE family-base label reads as a field (src/core/structure-decl.f). Acceptance: each such record names its own keyword and a help the loader accepts for it; deftype-suite pins one per case. Depends: none. Claim: unassigned.

---
title: Read the committed payload directly
status: closed
priority: 2
issue-type: task
created-at: "2026-11-14T19:20:19.371218+01:00"
closed-at: "2026-11-14T20:44:51.431177+01:00"
---

Problem: src/core/sumtype.f's payload provider (TDECL-SUMV-PROVIDER, TDPV-CAPTURE, the TDPV snapshot and its validation, :1620-1760) exists for 'a front end whose declaration is not yet published', but every caller passes TDECL-SUMV-PROVIDER (sumtype.f:2143, :2332, :2374; generated-declaration.f:422), whose answers are the committed SUMV rows (SUMV-PAY-N, SUMV-PAY-ROOT, SUMV-PAYCELLS@ in type-family.f). The three quotations, context cell, copy and checks have one implementation. Acceptance: the derive walkers and the remaining generators read SUMV-PAY-N, SUMV-PAY-ROOT and SUMV-PAYCELLS@ directly; the provider, TDPV snapshot storage, capture and validation are deleted; type-decl, enum-decl, structure-decl and type-linear pass. Depends: habu-build-a-constructor-c436b87a, habu-make-make-unmake-3a5c9b8a. Claim: unassigned.

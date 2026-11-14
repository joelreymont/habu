---
title: Name the generated word in a declaration refusal
status: open
priority: 2
issue-type: task
created-at: "2026-11-14T15:49:01.789775+01:00"
---

Problem: a STRUCTURE declaration refused because a generated name is taken (E-TDECL-NAME via TDECL-THROW, src/core/sumtype.f) renders 'name is reserved or already taken at 'po'': the family token, without the generated name or the reason TDECL-THROW was given. Acceptance: the refusal names the generated word that clashes and says it is already defined. Depends: none. Claim: unassigned.

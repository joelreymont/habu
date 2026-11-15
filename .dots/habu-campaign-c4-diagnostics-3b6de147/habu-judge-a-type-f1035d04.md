---
title: "Judge a type argument's kind where it is written"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-16T02:46:16.194366+01:00"
closed-at: "2026-11-16T04:07:35.790886+01:00"
---

Problem: a concrete type where a family's parameter takes a scope or region (read-view<n,n,n>) is judged only at SD-SEAL (src/core/structure-decl.f), whose record says a type parameter has incompatible kinds, with a help that fits no parameterless declaration; for a NEWTYPE or DEFTYPE the load places it at the base with '= word: base' and check.f at the keyword. Acceptance: the record stands at the type, names the kind the parameter takes, is the same from the load and check.f, and its help's edit loads. Depends: none. Claim: agent=W161 workspace=.jj-ws.noindex/jerry-w161

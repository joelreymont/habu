---
title: Let a declared type point at a quotation
status: active
priority: 2
issue-type: task
created-at: "2026-11-16T07:43:06.151972+01:00"
---

Problem: STRUCTURE fields, ENUM payloads and NEWTYPE bases reject `^[ n -- n ]` at `[` as an unknown type with a header-parameter help, though it is a type: stack effects and SUMTYPE payloads take it (docs/forth-card.md:167, docs/forth.md:879; /tmp/jerry-w188/rr/c/s1.f and p7.f load). src/core/structure-decl.f reads a type as one text span (SD-ATOM) and a quotation spans tokens; sumtype.f TDECL-VPAY-ELEM reads one. Acceptance: `STRUCTURE s FIELD f ^[ n -- n ] ;STRUCTURE`, an ENUM payload and a NEWTYPE base `^[ n -- n ]` load and type as the SUMTYPE payload does, from the load and check.f; a test pins each. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-w202.

---
title: Build a constructor from items
status: closed
priority: 2
issue-type: task
created-at: "2026-11-14T19:06:02.591462+01:00"
closed-at: "2026-11-14T20:44:51.371077+01:00"
---

Problem: SUMTYPE and ENUM constructors are written as source text and evaluated: src/core/sumtype.f TDECL-CTOR-WORD (TDGEN-NAME, -PAYLOAD, -OUT-TYPE, -CONSTRUCT-BODY) lays TDPLAN-WORD+ text rows, which TDECL-GEN-EVAL-BODY runs through TDECL-EVAL-XT. Acceptance: each constructor is built by define-word from items (the construct keyword, the family's type item, the variant operand) as a TDPLAN kind beside TAG, EQ and HASH, with the effect built from the schema as TDECL-DRV-EFFECT builds one; no constructor text row remains; the type-decl and enum-decl suites pass. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-ctor.

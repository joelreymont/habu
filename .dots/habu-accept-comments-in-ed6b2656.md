---
title: Accept comments in declaration bodies
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T00:11:28.383166+02:00"
---

Problem: a \ or ( ... ) comment inside a STRUCTURE or ENUM body is refused: 'ENUM kind VARIANT first ;VARIANT ( c ) ;ENUM' dies rc 70 with 'unexpected token in enum declaration at (' (E-TDECL-SYNTAX 7107), a \ line dies rc 70 'name must be a lowercase tail', and STRUCTURE refuses both the same way (probes on the stack's hb, 2026-11-06). So a variant's or field's meaning must fit the two-line comment above the opener: tools/check-verify-core.f's verdict ENUM carries six variants' meanings in two lines. tools/check.f writes E-BAD-DECLARATION for \ but no packet for (, only the load's message. FSM:DEFINE tables refuse both too (docs/forth.md, FSM:E-SYNTAX). Acceptance: Joel decides whether declaration bodies accept \ and ( ... ) comments (a language change); if so, the definers skip them at load and in tools/check.f's model, the refused-program tests become accepted ones, and docs/forth.md ('parsed by its definer') and the card's section 3 row say so. Files: the STRUCTURE and ENUM parsers (src/core/type-family.f, src/core/sumtype.f), tools/check.f's declaration model, docs/forth.md, docs/forth-card.md. Verify: the probe forms load and check clean; the type-decl, enum-decl and structure-decl tests. Depends: Joel's decision. Ownership: declaration parsing. Claim: unassigned.

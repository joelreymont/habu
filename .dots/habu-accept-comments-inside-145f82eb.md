---
title: Accept comments inside type declaration bodies
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T11:40:47.454460+02:00"
---

Problem: a '\' or '( ... )' comment on a FIELD or VARIANT line inside an ENUM or STRUCTURE body is refused. 'ENUM kind VARIANT first ;VARIANT ( c ) ;ENUM' dies rc 70 'unexpected token in enum declaration at (' (E-TDECL-SYNTAX 7107, src/core/enum-decl.f:296); a '\' line in an ENUM body dies rc 70 'name must be a lowercase tail'; STRUCTURE refuses both the same way, and tools/check.f reports either as E-BAD-DECLARATION (docs/forth.md:798). FSM:DEFINE tables refuse both too. So a field's or variant's meaning must fit the comment above the opener. Settled by the lead, 2026-11-06: bodies accept '\' and '( ... )' as the outer compiler does, through the shared declaration token reader of the unified ENUM and STRUCTURE front ends (src/core/enum-decl.f, src/core/structure-decl.f); the legacy definers do not need it.

Acceptance: positive fixtures with commented FIELD and VARIANT lines through both unified front ends; hostile fixtures refused: a comment token never becomes a field name, a variant name or an arity, a comment holding the text FIELD x n declares nothing, and an unterminated paren comment inside a body is refused with a diagnostic naming the comment, not a bare 7107; tools/check.f's declaration model, docs/forth.md:798 and the card's section 3 row say what bodies accept; enum-decl and structure suites green. Depends: none.

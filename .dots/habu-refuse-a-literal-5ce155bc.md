---
title: Refuse a literal 0 a word divides by
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T06:32:22.580001+01:00"
---

Problem: the checker refuses a literal 0 divisor only at the engine's own /, mod and /mod (src/core/checker.f ZERO-REASON and ZERO-OPERAND?, which need TOK-CORE-OP?), so a word that divides by its input admits one: '7 0 FMATH:FLOOR/ .' prints 0 rc 0, as does 'package HVZ public : HALVE ( n n -- n ) / ; ;package 7 0 HVZ:HALVE .' (docs/forth.md 'A literal 0 divisor is refused'). NUM:positive-divisor covers a divisor that is positive by type only. Acceptance: a literal 0 that reaches a divisor through a word's input, unchanged, is E-DIV-ZERO at the call, for FMATH:FLOOR/ and CEIL/ and a user's word alike, in every build; docs/forth.md and the card state the rule. Depends: none. Claim: unassigned.

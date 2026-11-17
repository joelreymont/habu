---
title: "Let a generic word reach every instance's width"
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T07:23:10.723659+01:00"
---

Problem: a Habu word is compiled once, so a generic word whose body calls a width-reading accessor or AT on an open receiver (: G ( ^held<b> -- ^bool ) HELD-SET ;) takes only one-cell b; a wide or structure instance is rejected at the call of G, a one-cell structure too though its code would be right (the held design, Fable 2026-11-16, its 'what stays rejected'). Acceptance: such a word takes every instance, its accessor numbers reaching it from the call site as hidden layout inputs (CALL-EXTRAS already pushes instance numbers for WITH-FIELD). Depends: a design. Claim: unassigned.

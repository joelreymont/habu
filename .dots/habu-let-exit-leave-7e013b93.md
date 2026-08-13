---
title: Let exit leave from anywhere in a body
status: open
priority: 2
issue-type: task
created-at: "2026-08-01T14:31:02.379285+02:00"
---

The native elaborator now compiles `exit`, but only as the last word of an `if` arm: the words after it would be unreachable, and the elaborator has no way to build a block with no predecessor and no values for the arms after it to hand on. src/compiler/native/elaborate.f DO-EXIT and AFTER-EXIT-CK refuse anything else with E-NELAB-CTRL. Wanted: an unreachable region after an exit - inside a begin/until or a ?do body as well as an if arm - elaborated without inventing values, so `exit` compiles wherever Forth allows it.

NOTE (succ-ord lane 2026-08-13): exit inside a QUOTATION BODY's arm
reports E-NELAB-ARITY (-8303) - the elaborator counting the body's
outputs with no rule for the early leave; the code is misleading for
the shape. Pre-existing; when this leaf lands, cover the quotation-
body case and give it the honest code.

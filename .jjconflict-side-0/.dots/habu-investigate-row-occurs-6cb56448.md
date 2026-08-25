---
title: Investigate row occurs check family-argument gap
status: open
priority: 1
issue-type: task
created-at: "2026-07-28T15:42:48.616627+02:00"
---

Full context: found while widening the Rocq model to arity-n families. The checker's two occurs checks disagree: TY-OCC? (src/core/checker.f:1278) descends into type-family arguments, but ROW-OCC? (checker.f:963-977) walks the row spine and the pointer/quotation chain and never enters a T-PARAM. So a row variable reachable only through a family argument escapes the row occurs check. SIG-PARSE-QUOT can parse a quotation as a family argument, so the shape is constructible in principle; the modelling worker found no surface fixture that reaches it because parameter-kind rules restrict layout arguments, and the Rocq model follows the code as-is with the divergence recorded at the example. Required result: determine whether any checked program can reach ROW-OCC? with a row variable hidden in a family argument. If yes, this is an occurs-check soundness hole - build the minimal fixture, fix ROW-OCC? to descend into T-PARAM argument runs the way TY-OCC? does, and add the negative regression. If no, prove why (which parameter-kind rule forbids it) and record that proof as a comment at ROW-OCC? so the asymmetry stops looking accidental. Checker-first: this is checker work, not model work; the model deliberately mirrors the current behaviour and must be updated in the same change if ROW-OCC? changes.

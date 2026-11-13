---
title: A payloadless constructor bound to a wide-payload typed local is refused
status: open
priority: 2
issue-type: task
created-at: "2026-11-05T14:12:14.815066+02:00"
---

Problem: a generated constructor takes its family's type arguments only from the definition's declared output, so its value cannot bind to a typed local or a call that names a wide payload, and two values of one family take the same arguments. `: VB ( -- n ) OPTION:NONE {: r:option<pr> :} 0 ;` (pr a 2-cell STRUCTURE) is refused at ':}' with 'actual: option<a>', while the same bind is accepted when the signature names option<pr>. Cause: CONSTRUCT-DECL-TERM (src/core/checker.f:5432) scans the declared output and CONSTRUCT-DECL-LAYOUT (:5455) feeds TFAM-CTOR-STEP? (src/core/type-family.f:5964); seeded, OPTION:NONE pushes option<pr>'s hidden cells plus a pad (TFC-CONSTRUCT-STEP-VID :5908); unseeded it is one option<a> of 2 cells, which the bind's trial unify (LOC-AS-BUNDLE? checker.f:19385, PARAM-BIND-OK? :4339, LAYOUT-BLOCK? :4195) cannot ground with the 2-cell pr. The refusal is sound: the value is 2 cells and the local wants 3.

Other shapes, seen before "Seed a constructor from its payloads" landed and to be rechecked first: `: VW ( pr -- n ) OPTION:SOME {: r:option<pr> :} 0 ;` refused at OPTION:SOME "expected: a actual: pr", likewise RESULT:OK, BOX:MAKE, MATCH arms re-wrapping and IF/ELSE joins; `OPTION:NONE TAKE` with TAKE ( option<pr> -- n ) refused at TAKE; `( -- option<one> option<pr> ) OPTION:NONE OPTION:NONE` refused, both read as option<pr>; `: VL1 ( -- option<n> ) OPTION:NONE {: r :} r ;` refused at ':}'. Seen after it: `ENUM kk chat embed ;ENUM STRUCTURE pt FIELD x n ;STRUCTURE : P16 ( kk pt -- option<kk> option<pt> ) swap OPTION:SOME swap OPTION:SOME ;` is refused at OPTION:SOME, 'expected: a pt actual: pt kk', because TFC-SEED (type-family.f:5900) and TFC-LIVE-SEED (:5883) take the first bundle of the family in the declared output.

A design (Plan, Opus) proposed checking such a definition twice, a first pass learning each constructor site's instance. Asked which approach, Joel asked "what's the best approach for long term?" and pointed the item at the one-pass compiler (Joel, 2026-11-05: "jerry is the one working on the 1-pass compiler"); the approach is that work's to settle.

Acceptance: a payloadless constructor's value binds to a typed local whose annotation names a wide payload, with the right cells at run time; P16 compiles and runs; a value whose payload does not name every parameter (RESULT:OK leaves err's free) still takes the declared output's argument for that parameter. Depends: none. Claim: unassigned, owner jerry.

---
title: "Bind a local over a generic word's own layout"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T11:54:55.893055+02:00"
---

Problem: a word generic over a, such as ( a -- n option<a> ), cannot bind its option<a> to a local, typed or untyped: the layout's width reads a variable, so src/core/checker.f LAYOUT-WIDTH-OPEN? (:4061) counts it as open, although a generic argument is always one cell (a wide value passed as a is refused). Acceptance: a layout over the word's own declared variable has its real width, so both forms bind and read all its cells, or the reason it cannot is stated in docs/forth.md 'Rules learned by refusal'; test/checker-soundness-suite.f OPEN-WIDTH-LOCALS cases CS-L2 and CS-L3 admitted, with a cell between the bind and the read. Depends: none. Claim: unassigned.

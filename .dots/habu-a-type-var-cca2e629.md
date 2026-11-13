---
title: A type variable refuses a nominal pointee and says not why
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T13:23:35.945743+02:00"
---

Problem: a type variable's refusal of a nominal pointee says not why. With `require src/lib/adt/option.f`, a word whose body binds a declared `ptr a` to a pointer to an open family prints 'expected: ptr a actual: ptr option<b>' with ROWS-SUGGEST$'s generic 'Change the body so produced types match the signature.' (src/core/render.f:1057): U-TYPE's LAYOUT-BLOCK? arm (src/core/checker.f:4729) calls STALE-NOTE U-FAIL (:4420, :2399) and sets no latch, so CELL-DIAG! (:5280) falls to plain E-MISMATCH. LAYOUT-BLOCK? (:4136) refuses through STORED-TAKES? (:4128), the rule for a declared stored variable: it admits only a closed family LAYOUT-MEM-OK? accepts. docs/forth.md:2609 states the rule (a free type variable cannot unify with a nominal enum or family) only for result's error variable. Acceptance: every such refusal names the rule, the variable and the family it would bind, and a remedy that works (naming the family in the signature, `ptr option<a>`). Depends: none. Claim: unassigned.

---
title: Compile immediates from the Habu compile loop
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T13:12:28.922296+03:00"
---

Problem: immediates in compile mode run through the assembly `CAPTURE-IMMEDIATE` (`habu2.f:7529-7545`).
Acceptance: the order of `CAPTURE-IMMEDIATE`: find -> immediate bit -> close region -> `LNEUTRAL` query -> `COMPILE-IMMEDIATE` -> reopen; its refusals preserved.
Files: `src/habu/definers.f`, `src/habu/outer.f` (compile-mode dispatch), cases beside `test/outer-interpret.f`.
Verify: spark: immediate-word cases through the Habu loop under the feature cell, refusals included; gate.
Depends: habu-capture-bodies-and-5cbd31ea (I5b).
Route: Alder (shared: src/habu/definers.f, src/habu/outer.f and the test).
Ownership: krait (Intel lane).
Claim: unassigned.

---
title: Locate an undefined-type storage refusal
status: open
priority: 2
issue-type: task
created-at: "2026-10-01T07:11:23.861697+02:00"
---

Problem: a TYPED-BUFFER (and any storage definer that sizes a type) naming an undefined type throws E-CHECKER-LAYOUT-BUFFER 7121 from src/core/checker.f with no diagnostic: plain bin/hb --load dies 'hb: uncaught throw code 7121' rc 67, and tools/check.f default mode dies the same way through CHK-PREVERIFY-FAIL (tools/check-core.f:1518). Only --all-errors reports it located, since cfdb2bdd (found by the r4-expand3 lane). Acceptance: the engine's storage-declaration check refuses an undefined or layoutless type as a located checker diagnostic naming the type, with the checker's refusal status, in plain load and every check.f mode; a reduced case for each storage definer that sizes a type, written first and seen to fail through the real load path. Files: src/core/checker.f (baked: rebuild and converge), the definers' tests, docs/forth.md if the refusal is a language rule. Verify: the new cases, tools/check-test.f, test/gate-diagnostics.f, native build g1/g2 cmp, two-generation build. Ownership: storage definer type refusal.

Scope (lead, from the lane's census): the same declaration path also throws 7121 with no diagnostic for a malformed or sealed storage name (src/core/checker.f:9404, :9407) and an out-of-range literal count (:10561, :10581); they refuse through the same word in this lane. E-CHECKER-LAYOUT-BUFFER stays only for the capacity faults (:10520, :10607). Located means: plain load gives the existing `habu: in <name>: ...` checker shape naming the declared word and the type (plain-load checker diagnostics name the definition, not file:line); check.f modes give path:line:col. Assertions that pinned 7121 for these refusals move to the refusal status.

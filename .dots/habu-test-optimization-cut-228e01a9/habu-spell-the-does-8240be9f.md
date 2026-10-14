---
title: Spell the ;does suffix once
status: closed
priority: 3
issue-type: task
created-at: "\"2026-10-02T13:40:59.040298+02:00\""
closed-at: "2026-10-14T19:30:30.902604+03:00"
close-reason: "Implemented in 26006b2d: Habu users share DOES-CLAUSE:SUFFIX$, while the earlier emitter and seed cite it; two-generation fixpoint and does-clause suites passed."
---

Problem (review 326): the clause-name suffix ';does' is spelled locally at src/habu/kernel-x64.f:2190 (SUFFIX$), src/habu/xref.f:472, src/habu/aot-capture.f:979, src/compiler/native/elaborate.f:3848/3856, and (after 8d6da143) src/core/checker.f DOES-SUFFIX$; habu2.f emits it as bytes (SUF-LEN ~:3442). checker.f loads first of these in tools/build-fixpoint.f's engine order, so xref.f, aot-capture.f and elaborate.f can use checker.f's DOES-SUFFIX$. Acceptance: one Habu word holds the spelling for every Habu-level user that loads after checker.f; the emitter's bytes and the Gforth seed mirror cite it in a comment; engine rebuilt, g1 == g2, two-generation build; does-clause rows (test/does-clause-record.f, test/certify-does-definer.f) rc 0. Files: the five above.

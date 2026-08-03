---
title: Copy a callee whose own calls were copied
status: open
priority: 2
issue-type: task
created-at: "2026-08-03T16:09:24.811383+02:00"
---

src/compiler/native/inline.f records a body only when EVERY token of it is straight-line source: a token that names a callable word disqualifies the whole body (NELAB:SPLICEABLE?, SPLICE-MEANING?). That is what makes the copying terminate today - nothing copied can contain a call to copy in its turn. It also stops one level too early. tools/codegen-compare-migrated3.f's T-GET-N is ': T-GET-N ( ptr a n -- r ) T-AT-N @ ;': its own call to T-AT-N IS copied, so the routine the chain publishes contains no call at all, and yet its recorded-body candidacy is decided on the SOURCE token T-AT-N and refused. Every kernel row of the third corpus - T-SUM, T-SGD!, T-DIST2, T-NORM2 - therefore still emits a call per element, and those rows did not move when the fourth corpus's call rows dropped by eight times. What to build: let a recorded body hold a call to a word that is ITSELF recorded, and splice it recursively under a bound that is derived rather than chosen - the natural one is the same size rule applied to the body AFTER its own copies, since the recorded bodies form a finite acyclic set (a body is recorded only after the routine it names has been published). Depth has to be bounded structurally, not by a counter: the record is written at publication and a publication cannot name a routine published later, so the relation is already acyclic and a walk terminates. The yardstick is the third table's kernel rows in tools/codegen-compare-test.f. Owners: NINL, NELAB, NMIGRATE.

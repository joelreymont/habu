---
title: Kept quotations in the engine and library state their facts
status: closed
priority: 2
issue-type: task
created-at: "2026-11-18T09:25:03.297801+01:00"
closed-at: "2026-11-18T09:50:36.276946+01:00"
---

Problem: with the c9 checker, include.f USE-INTERPRETER, vocabulary.f REQUIRE-XT, odata/key.f DECODED-LEN and memory-test.f RBT-ACTION are rejected (kept quotation facts unstated, a stale catch read), failing vocabulary, stdlib-standalone-load, odata, memory, c2-memory-source, program-diagnostics and native-multi-error-recovery. Acceptance: those suite entries pass with the c9 engine. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-c9s1.

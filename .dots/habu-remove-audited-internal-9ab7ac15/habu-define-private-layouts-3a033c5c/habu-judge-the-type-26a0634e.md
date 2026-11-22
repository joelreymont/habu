---
title: Judge the type a definer passes to EFFECT
status: active
priority: 2
issue-type: task
created-at: "2026-11-22T15:35:07.827196+01:00"
---

Problem: EFFECT:VARIABLE-OF, TYPE-ITEM and LOCAL (src/core/effect.f:75-95) pass a type straight to the schema store; a program that casts a number to EFFECT:type faults the engine (E-MEMORY-FAULT in SCHEMA-TAG@) now that SCH-REC@ checks nothing. Only TYPE-OP judges its operands (check-hook.f:351 TYPE-ARG). Acceptance: each EFFECT word that takes a type rejects a forged one E-NOT-TYPE; a test case shows it. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-typearg.

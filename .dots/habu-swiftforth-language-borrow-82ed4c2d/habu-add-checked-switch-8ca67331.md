---
title: Add checked switch DSL
status: closed
priority: 2
issue-type: task
created-at: "\"2026-06-28T08:10:11.828175+02:00\""
closed-at: "2026-06-29T01:11:54.415155+02:00"
close-reason: Implemented checked CASE/OF/ENDOF/ENDCASE as the Habu-native switch DSL, documented it, used it in gate helpers, and verified child dot habu-implement-checked-case-78a9c782 plus engine-suite.
---

Files: new lib/switch.f or src/core/switch.f, src/core/checker.f if xt effects are required, tests, docs. SwiftForth [SWITCH]/RUNS/RUN: is linked-list plus execution-vector dispatch. Fix: implement only after execution vectors have a checked effect story, or design a Habu-native table/CASE DSL whose cases have one declared effect and whose default preserves the input value. Must reject duplicate keys, missing default, and effect-mismatched case bodies before runtime.

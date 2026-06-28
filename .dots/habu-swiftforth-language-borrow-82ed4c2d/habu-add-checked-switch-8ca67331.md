---
title: Add checked switch DSL
status: open
priority: 2
issue-type: task
created-at: "2026-06-28T08:10:11.828175+02:00"
---

Files: new lib/switch.f or src/core/switch.f, src/core/checker.f if xt effects are required, tests, docs. SwiftForth [SWITCH]/RUNS/RUN: is linked-list plus execution-vector dispatch. Fix: implement only after execution vectors have a checked effect story, or design a Habu-native table/CASE DSL whose cases have one declared effect and whose default preserves the input value. Must reject duplicate keys, missing default, and effect-mismatched case bodies before runtime.

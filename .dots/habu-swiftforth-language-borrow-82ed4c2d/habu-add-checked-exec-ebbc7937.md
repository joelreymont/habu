---
title: Add checked execution vectors
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-06-28T08:10:11.790033+02:00\\\"\""
closed-at: "2026-06-28T10:10:07.916519+02:00"
close-reason: implemented checked defer/is execution vectors; focused tests and native gate pass (109821ms <= 110000ms)
---

Files: src/core/checker.f, src/core/*.f or lib/exec-vector.f, test/gate-dictionary.f, docs/forth.md, docs/stdlib.md. SwiftForth provides DEFER, IS, and @EXECUTE; Habu has execute but not a checked storage model for xt cells. Fix: design a typed execution-vector vocabulary that either carries a quotation effect or fails closed when the checker cannot know the stored xt effect; add tests for uninitialized vectors, assignment, @EXECUTE no-op-on-zero if adopted, package scope, and checker rejection of effect-mismatched vectors. Do not implement as untyped xt storage.

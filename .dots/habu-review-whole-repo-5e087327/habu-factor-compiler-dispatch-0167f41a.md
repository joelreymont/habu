---
title: Factor compiler dispatch tables
status: closed
priority: 3
issue-type: task
created-at: "\"\\\"2026-06-25T12:19:43.566545+02:00\\\"\""
closed-at: "2026-06-25T16:57:37.217465+02:00"
---

Finding F18. Evidence: docs/factorization-review.md:46; src/habu/habu2.f:1774, src/habu/habu2.f:2048, src/habu/habu2.f:2050. Root cause: table-like dispatch/data is encoded as long inline chains and section lines. Fix: split by concern or introduce a checked dispatch/list DSL. Why: large dispatch chains force manual stack and load-order reasoning. Validate with native fixpoint and full native gate.

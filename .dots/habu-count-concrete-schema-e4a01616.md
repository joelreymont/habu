---
title: Count concrete schema linears
status: open
priority: 2
issue-type: task
created-at: "2026-07-11T12:43:48.965048+02:00"
---

Critical soundness: src/core/checker.f LAYOUT-MAYBE-LINEAR? and LAYOUT-LINEAR-COUNT scan only family args while sum/product schemas may contain concrete CT-LINEAR nodes. Recursively account instantiated variant/product schemas for memory, drop, and transports; add W1/W2 sum/product negative regressions.

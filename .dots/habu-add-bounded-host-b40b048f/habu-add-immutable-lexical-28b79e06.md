---
title: Add immutable lexical MEM borrows
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T16:44:29.040763+02:00"
blocks:
  - habu-add-checked-mem-ebd95492
---

Problem: unique spans alone force exclusive access even for safe readers and cannot prove that read borrows stay lexical. Fix: add package MEM read-borrow tokens that may fan out within one owner generation, cannot coexist with a unique mutable borrow, cannot escape their scope, and rejoin before owner recovery. Acceptance: overlapping readers certify; reader+writer overlap, escaped reader, free while readers live, cross-region reader, double end, and stale-generation reader reject; runtime reads preserve sentinels. Files: lib/memory-region-borrow.f, focused test, docs/stdlib.md, FILEMAP.md. Verify: checker quotation/linear fixtures, exact test load, memory suites, typed-local diff lint, full native gate.

---
title: Parity tests
status: open
priority: 1
issue-type: task
created-at: "2026-02-03T13:28:48.533075+01:00"
blocks:
  - habu-parity-criteria-7a3bc74d
---

test/: add JIT-vs-VM differential harness (random small programs + hand-picked edge cases). Use scalar checks for primitives; use ohsnap snapshots for structured Values/prints. Gate every JIT feature with parity tests.

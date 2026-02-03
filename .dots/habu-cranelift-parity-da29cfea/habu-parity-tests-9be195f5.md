---
title: Parity tests
status: open
priority: 1
issue-type: task
created-at: "2026-02-03T13:28:48.533075+01:00"
blocks:
  - habu-med-add-jit-b402aeb1
  - habu-parity-criteria-7a3bc74d
  - habu-fix-macro-chunk-5f196625
  - habu-fix-compiler-env-aec7d63b
  - habu-ir-skeleton-d7b83105
---

test/: add JIT-vs-VM differential harness (random small programs + hand-picked edge cases). Use scalar checks for primitives; use ohsnap snapshots for structured Values/prints. Gate every JIT feature with parity tests.

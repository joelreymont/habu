---
title: Verify compiler witness parity
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T23:04:26.809427+02:00"
blocks:
  - habu-prove-staged-gpu-754492e2
  - habu-prove-a64-obj-92f2ae05
---

Full context: design section 16.6 requires identical schema digests and witness vectors across executable and Rocq validators. Run every valid/corrupt shared/native/GPU vector through both, compare decisions/diagnostic class, and generate the assumptions report. Acceptance: no drift, no Admitted, expected external axioms only.

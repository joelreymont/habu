---
title: Prove staged GPU refinement
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T23:04:26.795510+02:00"
blocks:
  - habu-promote-staged-gpu-2c5c3e97
  - habu-prove-shared-ir-1a95c6ef
---

Full context: formalize covered RIR fusion, KIR logical semantics, GIR schedules/witness validators, PTXIR2 lowering, and PTX semantics under explicit numeric policy. Acceptance: elementwise, reduction/softmax, and matrix covered slices compose; corrupted schedule/resource witnesses reject in Habu and Rocq; external PTX axioms are explicit.

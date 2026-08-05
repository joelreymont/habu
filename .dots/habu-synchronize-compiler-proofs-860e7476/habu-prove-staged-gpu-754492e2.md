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

Full context: formalize covered RIR fusion, KIR logical semantics, GIR schedules/witness validators, PTXIR2 lowering, and PTX semantics under explicit numeric policy. Consume typed heap/separation facts with distinct global/shared/local/parameter spaces and thread/block ownership. Every schedule proves disjoint writes or a declared atomic/reduction rule; barriers transfer ownership between phases only under convergence, yielding race-freedom. Acceptance: elementwise, reduction/softmax, and matrix slices compose; corrupted schedule/resource/footprint/barrier witnesses reject in Habu and Rocq; external PTX axioms are explicit.

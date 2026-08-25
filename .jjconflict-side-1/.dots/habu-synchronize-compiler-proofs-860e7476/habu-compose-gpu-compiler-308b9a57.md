---
title: Compose GPU compiler theorem
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T23:04:26.836380+02:00"
blocks:
  - habu-verify-compiler-witness-fa77799d
---

Full context: compose model/RIR/KIR/GIR/PTXIR2/PTX proofs for each covered operation/schedule slice under target and numerical policy, consuming the common heap/separation laws and staged GPU address-space, ownership-transfer, and race-freedom proofs. Acceptance: promotion consumes the exact theorem/witness manifest; covered elementwise, reduction, softmax, GEMM, and MMA slices prove disjoint or declared atomic/reduction writes, convergent barrier transfer, and no unbound memory or race gap.

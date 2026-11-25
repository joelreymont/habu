---
title: Push data-base in T0 without a call
status: open
priority: 3
issue-type: task
created-at: "2026-11-25T16:42:29.569971+01:00"
blocks:
  - habu-inline-the-views-e1cd3f5c
---

Problem: data-base is a call (83M calls in a compile on x86, 0.6 ns each) because the base register is no vreg and T0's value stack entries are a vreg or a constant (src/habu/t0.f VS, K-REG, K-CONST). Acceptance: T0 pushes the base with a move and no call on both architectures and Wasm, the compile is no slower, and no binary grows; measured on x86 before and after. Depends: habu-inline-the-views-e1cd3f5c. Claim: unassigned.

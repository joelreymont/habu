---
title: Prove LIR and A64IR lowering
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T23:04:26.768107+02:00"
blocks:
  - habu-prove-stable-sir-91b9d57d
---

Full context: formalize representation/ABI lowering, instruction selection, allocation-witness validation, call clobbers, stack homes, control flow, and native memory refinement for covered slices. Consume typed heap/separation facts and prove that target loads, stores, stack slots, spills, calls, allocation identity, bounds, alignment, lifetime, permissions, and ABI-visible addresses refine the source/IR heap actions under the target contract. Acceptance: composed refinement reaches allocated A64IR; invalid allocation/layout/memory vectors reject in both validators; no framed memory changes.

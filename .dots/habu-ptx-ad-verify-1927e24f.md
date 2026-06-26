---
title: "PTX AD: verify red.global.add.f32 on sm_87"
status: open
priority: 2
issue-type: task
created-at: "2026-06-26T23:59:59.024799+02:00"
blocks:
  - habu-ptx-m6-collectives-12cf0e2d
---

Small but BLOCKING for scatter-add. autograd.md assumes red.global.add / atom.global.add are arch-gated-available on sm_87 for accumulating adjoints (fan-in gradients). This is ASSERTED, not verified. Confirm ptxas -arch=sm_87 accepts red.global.add.f32 AND it runs on the Orin device; if unsupported, record the atom.global.add CAS-loop fallback.
- Files: a tools/ptx/ smoke (pattern after tools/ptx/ptxas-smoke.f, Orin-only); record result in docs/autograd.md.
- Verify: ptxas assembles a red.global.add.f32 kernel for sm_87 with no warnings; device run accumulates correctly. On failure, fallback documented.
- Dep: M6 (encoder) + M1d device run for the on-device half.

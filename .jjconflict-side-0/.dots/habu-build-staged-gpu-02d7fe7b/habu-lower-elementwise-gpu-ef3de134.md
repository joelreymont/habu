---
title: Lower elementwise GPU IR
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T23:00:17.470018+02:00"
blocks:
  - habu-validate-elementwise-gpu-20ac3ed4
---

Full context: design GPU Wave B builds elementwise LOAD/compute/STORE in KIR, lowers through flat GIR to PTXIR2, and ports Maki elementwise regions. Acceptance: device goldens, broadcast maps, structural address CSE/load reuse, and no-text-emitter gate pass.

---
title: Port elementwise model lowering
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T23:02:29.252772+02:00"
blocks:
  - habu-lower-elementwise-ptxir2-da39bef0
---

Full context: port the existing checked model elementwise region boundary to build KIR values without changing maki/infer while Spark work is active. Acceptance: current elementwise CPU/device goldens, broadcast cases, source maps, gradients where owned, and old/new shadow coverage pass; unsupported regions remain named.

---
title: Validate elementwise GPU slice
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T23:02:29.266232+02:00"
blocks:
  - habu-port-elementwise-model-a6d03369
---

Full context: complete GPU Wave B with independent RIR/KIR/GIR/PTXIR validation, ptxas, launch/sentinel, device goldens, resource/performance rows, and structured mutation fixtures. Acceptance: covered production entry has zero hidden fallback; performance matches the committed correctness corpus baseline or records an attributed target-specific decision.

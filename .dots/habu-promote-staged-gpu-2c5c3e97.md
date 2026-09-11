---
title: Promote staged GPU artifacts
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T23:03:42.494724+02:00"
blocks:
  - habu-validate-gpu-autotuner-66c28736
---

Full context: bind validated artifact promotion to source/model/RIR/KIR/GIR/PTXIR2, target/toolchain, numeric policy, witnesses, correctness, resources, performance, and tuner protocol. Acceptance: any stale/missing/mismatched evidence rejects; promotion is atomic and deterministic; unsupported inputs remain named capabilities.

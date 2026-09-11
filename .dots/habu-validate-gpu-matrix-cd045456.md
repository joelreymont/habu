---
title: Validate GPU matrix resources
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T23:03:22.146461+02:00"
blocks:
  - habu-v2-resource-model-985a0b0e
  - habu-schedule-gpu-matrix-aa68a57e
---

Full context: validate fragment maps, shared memory, register pressure, occupancy, launch, async pipeline, and target legality before PTXIR2 emission. Acceptance: every invalid fragment/lane/address/order/resource case rejects with structured diagnostics; estimates bind calibrated target/toolchain facts. Dependency: scheduled matrix pipeline and resource model calibration.

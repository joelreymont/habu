---
title: V2 Orin activation and rollback
status: open
priority: 1
issue-type: task
created-at: "2026-07-11T12:25:28.057920+02:00"
blocks:
  - habu-v2-evidence-promotion-f8312ebe
---

Implement MODEL-CAD-V2-PLAN.md:1754-1772 signed manifest validation, target attestation, health-gated canary activation, prior-artifact retention, telemetry, watchdog, atomic switch, and rollback by digest. Acceptance: corrupt/incompatible artifact, stale sensor, deadline miss, device loss, thermal throttle, failed canary, restart, and rollback are injected; steady-state activation performs no compilation or allocation.

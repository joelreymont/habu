---
title: Bound capture values by the admitted span
status: active
priority: 1
issue-type: task
created-at: "2026-10-15T23:34:17.112964+03:00"
---

Current native-file-auto private-primitive E2E builds the real unsealed engine and fails because its DATA value stream exceeds the fixed 4 MiB AOT-WINDOW:VAL-CAP. The admitted DATA span is 16 MiB and each cell uses at most VMAX bytes; VAL storage is already dynamic. Derive the value bound from that accepted span and the existing codec rather than a measured-headroom guess. Preserve span, bitmap, reader and aggregate section limits. Qualify the real whitebox build and its primitive-refinement consumer, plus required native/convergence checks.

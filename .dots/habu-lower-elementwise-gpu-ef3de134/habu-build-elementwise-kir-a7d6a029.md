---
title: Build elementwise KIR
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T23:02:29.208973+02:00"
blocks:
  - habu-ptx-opt-layer-325b9507
---

Full context: design Wave B makes checked LOAD, elementwise operations, broadcast index maps, and STORE build immutable GPU-KIR rather than PTX text. Acceptance: closed schema/effects/types/shapes/addresses validate; unsupported ops reject by capability; canonical elementwise fixtures bind model source and numeric policy.

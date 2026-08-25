---
title: Remove generated PTX parsing
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T23:00:47.990860+02:00"
blocks:
  - habu-compare-ptxir2-saxpy-ac4e3d0d
  - habu-port-exact-ptxir2-d2cb93fa
---

Full context: complete GPU Wave A by deleting generated-code dependence on line parsing and routing optimization/rendering only over PTXIR2. Keep an external parser solely for external modules and roundtrip tests. Acceptance: a Habu-native gate finds no generated-text parser path or string-first instruction emitter for the covered subset; SAXPY and selected device fixtures pass. Dependencies: PTXIR2 SAXPY shadow and exact PTXIR2 passes.

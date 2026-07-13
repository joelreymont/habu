---
title: Migrate training Maki count callers
status: open
priority: 2
issue-type: task
created-at: "2026-07-13T14:14:22.920760+02:00"
blocks:
  - habu-migrate-model-ir-c171bdf5
---

Full context: migrate remaining frozen B5 MIR count callers in training/import tests: maki/from-scratch-model-test.f, maki/gradcheck.f, maki/gradcheck-test.f, maki/mlp-bwd-test.f, maki/onnx/import-test.f, and maki/onnx/ort-ref-test.f plus directly associated sources only where an accessor call actually exists. Replace all old count accessors in each owned file with MIR typed counts. Acceptance: training/gradcheck/ONNX tests exact, no production import allocator overlap, per-file census empty. Depends on MIR count APIs.

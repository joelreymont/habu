---
title: "Maki: ONNX import to Habu-PTX kernels"
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T00:01:50.984137+02:00"
blocks:
  - habu-write-docs-maki-eb717147
---

D. Implement ONNX import per docs/maki/onnx.md: graph parse, the supported-op lowering table to forward kernels (M4-M6/M11), FAIL-CLOSED rejection of unsupported ops with a named diagnostic, and the dynamic-shape policy. Inference deploy needs only forward kernels, not the AD transform. Habu-native (no .py under maki/, per host-lint).
- Files: maki/onnx/ (parser, op-table, lowering - split by concern).
- Verify: import a small ONNX model, run vs onnxruntime golden; an unsupported op / dynamic rank is REJECTED loudly (negative test).
- Dep: docs/maki/onnx.md (habu-write-docs-maki-eb717147) + corrected M6 forward kernels (habu-fix-ptx-collective-997cfcce) + maki tensor types.

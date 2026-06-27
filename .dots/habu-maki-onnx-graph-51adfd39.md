---
title: "Maki: ONNX graph parser + real model import"
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T08:06:44.252706+02:00"
blocks:
  - habu-maki-onnx-import-f37c9458
---

Gap #10. maki/onnx.f is only the op-COVERAGE lowering table (Add/Mul/Relu/Softmax/Gemm -> kernel). There is no ONNX graph parser, no protobuf decode, no initializer/shape handling, no real .onnx model import. Build the graph importer: parse an ONNX model (protobuf), walk the node graph, map each op via ONNX-LOWER, handle initializers + dynamic-shape policy (fail-closed per docs/maki/onnx.md), produce a runnable maki graph.
- Files: maki/onnx/ (parser + graph builder).
- Verify: import a small real .onnx model, run vs onnxruntime golden; unsupported op / dynamic rank rejected loudly.
- Dep: maki onnx op-table (done) + maki->PTX lowering.

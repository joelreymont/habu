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

LANDED (importer core, all in package ONNX):
- maki/onnx/proto.f: protobuf wire subset decoder over (base,lo,hi) windows with
  model-absolute offsets - varint (10-byte/64-bit, fail-closed overflow), tag,
  length-delimited slice, fixed32, skip-by-wire (groups/reserved -> E-PB-WIRE).
  Codes -5220..-5223.
- maki/onnx/graph.f: ModelProto->GraphProto walkers into flat importer tables
  (interned names, nodes w/ op_type + input window + single output + recognized
  attrs alpha/beta/transA/transB/axis, initializers w/ payload span, input/output
  ValueInfos). Fail-closed dynamic shapes (dim_param/missing/non-positive ->
  E-ONNX-DYNSHAPE), rank policy (1 -> 1xC, 2 -> RxC, else E-ONNX-RANK), FLOAT-only
  (E-ONNX-DTYPE), unknown attr names E-ONNX-ATTR. Codes -5224..-5235, -5238.
- maki/onnx/import.f: ONNX:IMPORT / ONNX:IMPORT-FILE -> the SAME model IR MODEL:
  produces (maki/model-ir.f): graph inputs + initializers become MIR input slots,
  initializer payloads materialize into a float-cell arena (ONNX:BIND-INITS binds
  them to the executor), nodes walk in NodeProto order with the topo-sort
  assumption VALIDATED (unbound input -> E-ONNX-TOPO). Coverage = ONNX:LOWER
  (unsupported op -> its E-MK-ONNX). IR map: Add/Mul (strict same-shape),
  Relu, Softmax (axis last only), Gemm -> OP-MATMUL (2-in) / OP-LINEAR (3-in,
  bias 1xN), alpha=beta=1 transA=transB=0 enforced. v1 output contract: exactly
  one graph output = the last node. Proof: host executor (EX-RUN) numerics + the
  fusion planner (FP-BUILD, Gemm->Relu fuses to 1 region) over the imported IR.
- maki/onnx/encode.f: checked protobuf wire ENCODER (fixture DSL: ENC-SUB/;ENC-SUB
  nesting, varint/tag/str/f32) so all test fixtures are hand-encoded in source.
- Tests wired into maki/test.f: maki/onnx/proto-test.f (varint edges, skip walk,
  8 fail-closed decoder paths), maki/onnx/import-test.f (4 executed fixtures incl.
  raw_data + packed float_data + input-listed initializer + IMPORT-FILE round trip,
  18 fail-closed importer paths).

FOLLOW-UP LEGS (not in this change):
- Real-model golden: import a real exported .onnx (small MLP), cross-check vs an
  onnxruntime golden on the same inputs (needs a real model artifact + a host
  onnxruntime run to produce the expected tensor).
- Movement ops (Reshape/Transpose/Slice/Concat/Gather via ONNX:MOVE-KIND): need
  shape-tensor input handling (Reshape's shape input is data, not an attr);
  today they fail closed through ONNX:LOWER.
- More compute ops as ONNX:LOWER grows; numpy broadcast beyond same-shape for
  Add/Mul (map 1xC/1x1 onto the OP-BIAS/OP-SCALE classes).
- Multi-output graphs / output at a non-final node (relax the v1 contract).
- Gemm transA/transB/alpha/beta by composing transpose/scale nodes.

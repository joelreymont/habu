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

## Follow-up legs (host-side)

The graph importer landed (proto.f + graph.f + import.f + encode.f, host-executed
fixtures A-D). Remaining host-side legs, one commit each:

- **LEG A — numpy broadcast for Add/Mul. LANDED.** Add/Mul no longer require strict
  same-shape operands: the second operand is classified with maki/bcast.f BC-CLASS
  (the mapping EX-BC@ / SHP-LEGAL? already prove) and mapped onto the capture-legal
  classes — Add + full -> OP-ADD, Add + 1xC -> OP-BIAS (row broadcast); Mul + full
  -> OP-MUL, Mul + 1x1 -> OP-SCALE (scalar). Every other numpy shape (Rx1 column,
  1x1 into Add, 1xC into Mul, ragged rows) stays fail-closed E-ONNX-SHAPE.
  Fixtures E (Add bias 1x2) and F (Mul scale 1x1) host-execute vs hand-computed;
  negatives TRY-ADDCOL (2x1 column) and TRY-ADDRAGGED (3x2 vs 2x2) reject. Files:
  maki/onnx/import.f, maki/onnx/import-test.f (+ require maki/bcast.f).
- **LEG B — movement operators. LANDED (Reshape / Transpose / Concat).** graph.f
  now splits initializers by data_type: FLOAT (1) -> f32 OGI arena as before; INT64
  (7) -> a new rank-1 int-constant table (OGIC), decoded to cell ints at parse. It
  also collects the Transpose `perm` ints (AttributeProto.ints=8, packed or unpacked)
  onto the node. import.f wires three movement builders with MV-PACK'd attrs + verdict,
  host-executed on the maki/move.f references: Reshape->OP-RESHAPE (target [R,C] read
  from the INT64 shape initializer; a shape input absent from OGIC is a runtime-computed
  shape -> E-ONNX-DYNSHAPE); Transpose->OP-TRANSPOSE (2D; perm absent=reverse or exactly
  [1,0], any other/rank-3 perm -> E-ONNX-ATTR); Concat->OP-CONCAT (2 inputs, axis 0
  row-append). Fixtures RS/TR/CC host-execute vs hand-computed; negatives TRY-RSDYN
  (runtime shape), TRY-BADPERM ([0,1]), TRY-PERM3 (rank-3). Files: maki/onnx/graph.f,
  maki/onnx/import.f, maki/onnx/import-test.f.
  - **Slice / Gather / int64_data / Reshape infer: LANDED 2026-07-11 (fable
    8a0a9fe0).** int64_data (field 7, packed AND unpacked varints, negative
    two's-complement proven) decodes into OGIC alongside raw_data; both/neither
    payload fails closed E-ONNX-DATA (TRY-I64BOTH/TRY-I64NONE). Reshape target
    dims: 0 = copy input dim, exactly one -1 inferred by exact division
    (RS-DIM/RS-RESOLVE; MODEL-RSI/RS0/RS7U/RSN7 host-executed; [-1,-1] and
    non-dividing reject E-ONNX-SHAPE). Slice: starts/ends from OGIC (runtime
    operand -> E-ONNX-DYNSHAPE), axes absent-or-[0], steps absent-or-[1], ONNX
    clamp incl. negatives (SLICE-CLAMP; MODEL-SL/SLN host-executed); empty range
    (s0>=s1) fails closed per v1 policy (TRY-SLEMPTY). Gather: axis absent-or-0;
    INT64 indices resolved at import (GA-IDX: negative += rows, then strict
    range check E-ONNX-SHAPE - the destruction review caught that the naive
    int->float bridge silently folded -1 to row 0 via executor rounding; fixed
    with MODEL-GAN [-1,0] fixture + TRY-GAOOR/TRY-GANEG negatives) then bridged
    to a synthesized Kx1 f32 input slot (SYN-IVEC, SYN-CONST arena precedent) so
    the executor is untouched. Slice+Gather outputs proven through FP-BUILD
    (materialized movement output = own region). Still remaining here: Transpose
    perm honoring for rank>2 once the IR is >2D.
- **LEG C — Gemm attribute composition. LANDED.** The default affine form
  (alpha=beta=1, transA=transB=0) still lowers to the single-node OP-MATMUL /
  OP-LINEAR fast path; attributes now COMPOSE into a node chain instead of being
  rejected: transA/transB insert a TRANSPOSE on that operand; a non-unit alpha
  inserts an OP-SCALE against a synthesized 1x1 constant (SYN-CONST + BIND-INITS
  binds it alongside the initializers); beta=0 drops C, beta=1 adds C as an OP-BIAS,
  other beta scales C first. Only a non-Gemm attribute now rejects E-ONNX-ATTR.
  Fixtures GTB (Gemm transB=1 -> TRANSPOSE+MATMUL, the PyTorch Linear export shape)
  and GAB (alpha=2 beta=0 -> MATMUL+SCALE, C dropped) host-execute vs hand-computed;
  TRY-GEMMATTR (foreign axis attr) rejects. Files: maki/onnx/import.f,
  maki/onnx/import-test.f. (F32 attr bits -> f64 via lib/ptx/cg.f F32>F64.)
- **Real-model onnxruntime golden.** REMAINING (needs a host onnxruntime run;
  user-gated). The in-source hand-encoded fixtures host-execute every op vs
  hand-computed values, but a real .onnx model compared against an onnxruntime
  reference run has not been done (no onnxruntime in this environment).

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

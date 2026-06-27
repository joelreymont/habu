# Maki ONNX import — op coverage, fail-closed

`maki/onnx.f`: the op-coverage lowering table that maps each ONNX operator to the
maki / Habu-PTX kernel entry it lowers onto, with a **fail-closed** policy — an
unsupported op is rejected (`E-MK-ONNX throw`), never silently approximated. This is
the contract that decides what a model can lower to.

## Current coverage (`ONNX-LOWER`)
`Add → ADD-F`, `Mul → MUL-F`, `Relu → RELU-F`, `Softmax → SOFTMAX-ROWS`,
`Gemm → SAXPY` (affine y=a·x+b). Anything else fails closed (tested in `onnx-test.f`,
e.g. `Conv` throws).

## Design intent + roadmap
- **Subgraph, not single op.** Lowering a *chain* of elementwise ops = concatenating
  their Habu words into one fused kernel (`maki/fusion.f`) — fusion is the default in a
  concatenative DSL, not a pass. So `ONNX-LOWER` over a subgraph maps each node to its
  word(s) and concatenates (memory-bound ops fuse → fewer HBM round-trips).
- **Real GEMM/attention.** `Gemm` lowers onto the tiled GEMM (`lib/ptx/cg-matmul.f`,
  the tensor-core path on the compute roof); an attention subgraph onto the fused
  attention (`lib/ptx/cg-attention.f`). Dotted `habu-maki-onnx-graph` (real graph
  parser + model import) and `habu-small-model-end` (a small model lowered end-to-end).
- **Fail-closed is non-negotiable** — coverage is explicit; never approximate an
  unmodeled op.

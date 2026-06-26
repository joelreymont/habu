# Maki — Status

Maki is the ML framework layer on Habu + Habu-PTX. This is maki's own status doc,
kept outside the Habu trust root and self-check (the fence). It deliberately
quotes no self-check tallies, so the Habu stale-status lint stays satisfied until
the maki-skip fence fix lands (dot `habu-add-maki-skip` for stale-status-lint).

## Built

- **Tensor shape + dtype metadata** (`maki/tensor.f`, `maki/tensor-test.f`) — the
  v0 tensor type foundation: 2D shape arithmetic (element count, broadcast
  compatibility/result) and the sm_87 dtype set (f32 / f16 / bf16 / u32 / i32)
  with byte sizes. Pure checked Habu, runnable.
- **Optimizers** (`maki/optim.f`, `maki/optim-test.f`) — SGD, SGD+momentum, and
  L2 weight decay as float update rules (the per-weight math). Runnable.
- **Losses** (`maki/loss.f`, `maki/loss-test.f`) — MSE + its gradient, and L1
  (the per-element rule). Runnable.

The optimizer/loss element rules apply per-weight; the tensor-level apply (one
update / one reduction over a whole tensor) lowers onto a Habu-PTX kernel once
codegen lands. The rules themselves are exact, checked, and tested now.

## Next (see root PLAN.md + dots)

- Tensor handle over a Habu-PTX `matrix`/`span` (storage + shape + dtype together).
- Autograd orchestration: the tensor-op VJP table lowering onto the Habu primitive
  VJP table; the user-facing define-forward → checked-backward API.
- Optimizers (SGD/Adam) + losses; ONNX import; training/eval loop; eval harness.

## Underneath (Habu-PTX, in `lib/`)

The checked kernel vocabulary maki builds on: M4 tile ops (`lib/ptx-tile.f`,
checked SAXPY), M6 collectives + softmax (`lib/ptx-collective.f`, checked
SOFTMAX-ROWS), and the AD primitives + verified-gradient kernel
(`lib/ptx-autograd-test.f`, checked SOFTMAX-ROWS-BWD).

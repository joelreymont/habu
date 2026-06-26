# Maki — Status

Maki is the ML framework layer on Habu + Habu-PTX. This is maki's own status doc,
kept outside the Habu trust root and self-check (the fence). It deliberately
quotes no self-check tallies, so the Habu stale-status lint stays satisfied until
the maki-skip fence fix lands (dot `habu-add-maki-skip` for stale-status-lint).

## Built

- **Tensor shape + dtype metadata** (`maki/tensor.f`, `maki/tensor-test.f`) — the
  v0 tensor type foundation: 2D shape arithmetic (element count, broadcast
  compatibility) and the sm_87 dtype set (f32 / f16 / bf16 / u32 / i32) with byte
  sizes. Pure checked Habu, runnable, independent of the type-only PTX runtime.

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

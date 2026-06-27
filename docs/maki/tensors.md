# Maki tensors — array storage + shape/dtype

Two layers, both checked Habu, one concern each.

## `maki/array.f` — tensor-scale storage + whole-tensor ops
Real float tensors as contiguous cell buffers. Whole-tensor operations (the ops the
Habu-PTX kernels run on device, here on the host): `fill`, `sum` (reduction),
elementwise `add`, and the in-place **tensor SGD step** `w -= lr·g` over a whole
parameter array. The optimizer runs at TENSOR scale here; the same op lowers onto a
Habu-PTX kernel (SAXPY / fused) on device — see `docs/eval-triton.md` and `maki/gpu.f`.

## `maki/tensor.f` — shape + dtype metadata
The v0 tensor *type* foundation: 2-D shape arithmetic (element count, broadcast
compatibility + result shape) and the sm_87 dtype set (f32 / f16 / bf16 / u32 / i32)
with byte sizes. Pure checked Habu. Broadcast/shape errors fail closed.

## Design intent
A tensor handle = storage (`array.f`) + shape/dtype (`tensor.f`). The element/scalar
rules in `optim.f` / `loss.f` / `autograd.f` apply per element; the **tensor-level
apply** (one update/reduction/op over the whole tensor) is what lowers onto a checked
Habu-PTX kernel. Roofline: elementwise/reduction tensor ops are memory-bound (see
`docs/kernel-principles.md`), so the device path **fuses** them — never one kernel per op.

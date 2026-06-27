---
title: "Autograd: tensor/batched VJP layer (lift scalar VJPs to whole-tensor ops on PTX)"
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T23:00:02.502003+02:00"
---

PyTorch differentiates BATCHED TENSORS; maki/autograd.f VJPs are element/scalar-level (Habu floats, CPU). Lift each VJP to a whole-tensor op that lowers onto a checked Habu-PTX kernel: forward TENSOR op + backward TENSOR op, the element rule applied over the tensor via the tile DSL. Files: maki/autograd.f (tensor wrappers), maki/array.f (tensor apply), lib/ptx tile kernels. VERIFY: tensor ADD/MUL/RELU forward+backward match the element rule over a small tensor, gradchecked at tensor scale. Dep: EPIC autograd-parity; relates habu-maki-autograd-orchestration.

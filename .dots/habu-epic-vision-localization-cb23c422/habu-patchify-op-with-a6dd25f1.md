---
title: Patchify op with exact VJP
status: open
priority: 1
issue-type: task
created-at: "2026-07-20T11:19:31.439190+02:00"
---

The one op that turns an image into transformer tokens: split a CHW float tensor into non-overlapping P-by-P patches and flatten each to a row, giving (num-patches, patch-dim); the linear projection to model width then reuses the existing matmul op. Forward is a strided gather; backward is the exact scatter-add adjoint. Register it in the CAD op registry (maki/cad-kinds.f, maki/plan-vocab.f, op-registry, executor, backward) exactly like OP-LAYERNORM was added, with gradcheck coverage at several shapes including non-square and single-patch edges. SERIALIZE: the CAD registry files are frequently owned by active device-side lanes (the affine-layernorm and rank-0 accessor lanes claimed them recently); verify no active claim owns maki/cad*.f, op-registry, plan-vocab, executor, backward before dispatching, and coordinate rather than fork.

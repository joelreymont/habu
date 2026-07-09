---
title: Automatic op-fusion pass (register-resident, the bandwidth win)
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T15:43:15.234314+02:00"
---

GAP (2026-06-27): maki/onnx.f lowers one tensor/ONNX op -> one kernel (ADD-F/MUL-F/RELU-F/...), so a chain like Add->Relu is 2 kernel launches with a global round-trip between them. Intra-KERNEL: bodies already fuse register-resident by hand (SAXPY a*x+y, softmax), but there is NO automatic pass over an op SUBGRAPH. Build a fusion pass: (1) an op-graph IR for a maki/ONNX subgraph; (2) fusibility analysis - elementwise/same-shape ops (Add/Mul/Relu/Scale/Bias/EXP) fuse into one kernel; reductions, reshapes, and shape changes are fusion BARRIERS; (3) codegen that emits ONE checked KERNEL: composing the fused tile ops with intermediates in registers (never global). The checker proves the fused stack effect. This is the concrete 'beat Triton' lever (Triton materializes intermediates; we don't) and the design's stated 'big win' (docs/ptx.md 106/121). Needs the IR layer (habu-ptx-ir-opt). VERIFY: fused Add+Relu does 1 global round-trip vs 2 separate kernels, device-correct, ~2x effective GB/s; a fused elementwise chain beats the per-op Triton baseline on bandwidth.

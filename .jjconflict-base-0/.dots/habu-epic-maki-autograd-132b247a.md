---
title: "EPIC: Maki autograd to PyTorch-parity-or-better (verified-gradient framework)"
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T23:00:02.486997+02:00"
---

VISION: make maki autograd a full reverse-mode AD framework that MATCHES PyTorch on usefulness and BEATS it on correctness-by-construction. Today (honest): maki/autograd.f has 3 scalar VJP pairs (ADD/MUL/RELU) numerically gradchecked; lib/ptx/ad.f does AUTOMATIC source-to-source reverse-mode (syntactic reversal) for STRAIGHT-LINE concatenative kernels over a fixed primitive set. Gaps vs PyTorch: (a) op coverage (PyTorch ~hundreds; maki ~handful), (b) scalar/element-level not batched tensors, (c) straight-line only (no general control flow / fan-out beyond the scatter-add + multi-output dots), (d) no higher-order grads, (e) no end-to-end model-grad parity demonstrated. The WIN over PyTorch (the thesis): PyTorch backward is hand-written + TRUSTED (silently wrong until gradcheck); maki backward is automatically derived for kernels, the derived backward is itself TYPE-CHECKED by the stack-effect checker, AND numerically gradchecked - a verified-gradient target. ACCEPTANCE: the VJP table covers a full transformer block (matmul, softmax-attention, layernorm, GELU, residual, embedding) at TENSOR scale; every entry is type-checked + gradchecked (committed matrix); a small model trains end-to-end on the Orin with gradients + final loss matching a CPU/PyTorch reference within tolerance; higher-order grad demonstrated on one op. Composes the existing AD chain (habu-ad-reverse-pass, habu-ad-vjp-primitive, habu-ad-b-adjoint, habu-ad-scatter-add, habu-ad-softmax-rows, habu-ad-save-vs, habu-ad-thread-saved, habu-ad-validate-multi, habu-ptx-ad-device, habu-maki-autograd-orchestration) PLUS the sub-dots below. Dep: none (umbrella).


## OWNERSHIP (2026-07-03): FULL HANDOFF to the second agent
The SECOND AGENT owns this entire epic. Territory (exclusive): lib/ptx*.f,
maki/*, src/arch/ptx/*, docs/eval-*.md, and the whole AD/GPU/fusion dot cluster
(ad-*, vjp, softmax, saxpy, tensor-core-mma, tiled-gemm, automatic-*-fusion,
autograd-*, eval-matrix/real, m4e-*, mechanical-checker-guided). Fully DISJOINT
from the engine/checker campaign — no overlap with src/habu/*, src/core/*, or
tools/build-fixpoint* — so it runs in full parallel starting NOW.
CAVEAT (not a conflict): device gradient-parity verification needs the zed
(Linux) GPU. On the Mac, build-and-check the Habu-side codegen + autograd
construction through the maki gate (maki/README.md); mark device parity as
PENDING-ZED rather than claiming verified. The main gate's maki slice must stay
green after every change.
NOTE (2026-07-07): the zed box is unreachable for ~a week, expected back
~2026-07-14, so the PENDING-ZED device parity stays pending until then; keep
progressing the Mac-side codegen/autograd + maki gate meanwhile.

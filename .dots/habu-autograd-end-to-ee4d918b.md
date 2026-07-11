---
title: "Autograd: end-to-end model-grad parity vs CPU/PyTorch reference"
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T23:00:02.531465+02:00"
---

Prove parity: train a small MLP + a 1-layer attention block end-to-end with maki autograd + Adam, and match a CPU (and ideally PyTorch) reference on BOTH per-step gradients and final loss within tolerance. This is the autograd half of habu-small-model-end. Files: a committed maki training+grad-compare harness; reference grads computed by the element-level autograd.f (CPU) and/or an external PyTorch script (documented, external per Habu-Only). VERIFY: gradient L2 error and final-loss match within tol, committed regression; runs on the Orin. Dep: EPIC; tensor-VJP layer + transformer-block coverage + habu-maki-training-loop + habu-small-model-end.

RESIDUE SCOPED 2026-07-11 (Wave-2 sweep): from-scratch-train.f trains an MLP
with BW-BUILD + plain SGD; optim.f ADAM rules + optim-tensor.f TT-ADAM! exist
but NO training loop uses Adam; NO attention block is trained end-to-end; NO
per-step gradient compare vs the element-level autograd.f CPU reference exists.
Host scope for this dot (merges naturally with habu-maki-training-loop Wave 3):
Adam-driven training step, 1-layer attention-block end-to-end training (GC-ATTN
landed the gradcheck precedent, fable 44dde089), per-step grad-L2 + final-loss
compare vs the element-level reference within tolerance. The PyTorch reference
run is USER-GATED (external tool, Habu-Only bars a local script).

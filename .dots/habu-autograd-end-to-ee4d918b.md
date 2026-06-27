---
title: "Autograd: end-to-end model-grad parity vs CPU/PyTorch reference"
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T23:00:02.531465+02:00"
---

Prove parity: train a small MLP + a 1-layer attention block end-to-end with maki autograd + Adam, and match a CPU (and ideally PyTorch) reference on BOTH per-step gradients and final loss within tolerance. This is the autograd half of habu-small-model-end. Files: a committed maki training+grad-compare harness; reference grads computed by the element-level autograd.f (CPU) and/or an external PyTorch script (documented, external per Habu-Only). VERIFY: gradient L2 error and final-loss match within tol, committed regression; runs on the Orin. Dep: EPIC; tensor-VJP layer + transformer-block coverage + habu-maki-training-loop + habu-small-model-end.

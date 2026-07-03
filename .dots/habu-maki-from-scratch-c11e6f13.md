---
title: "Maki: from-scratch temporal model trained on GPU"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T00:40:52.797973+02:00"
---

Driving workload flagship for the training side (docs/model-cad.md Driving workload). Define a small temporal model from scratch in the MODEL: vocabulary - v1 a windowed MLP/TCN (no recurrence) mapping a feature-sequence window to prediction + log-variance. Train from random init to convergence on committed synthetic data using the Gaussian NLL loss; backward generated and device-gradchecked; optimizer step at tensor scale on GPU (maki/optim-tensor.f, maki/gpu-train.f chain); training-step profile row through the cad PROFILE path; convergence committed as a test gate (loss threshold, seeded). Proves maki develops AND trains models, not only optimizes inference. Depends: cad-0b, Gaussian NLL dot. Related: habu-maki-training-loop, habu-epic-maki-autograd, habu-autograd-tensor-batched, habu-small-model-end.

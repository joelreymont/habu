---
title: nanoGPT weight-init policy + Gaussian RNG
status: open
priority: 2
issue-type: task
created-at: "2026-07-18T15:24:38.472605+02:00"
---

PARTIAL: current init is uniform LCG only (from-scratch-model.f SC-FILL-SMALL [-0.1,0.1) via SC-UNIT; adam-train.f ATN-FILL). GPT-2 init is normal(0,0.02), residual-projection init scaled 0.02/sqrt(2*n_layer), LayerNorm gamma=1/beta=0, biases=0. Add a Gaussian RNG (Box-Muller over the existing LCG) and a per-parameter-role init policy word. Dep: LCG exists (from-scratch-model.f).

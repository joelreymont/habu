---
title: Dropout op (train/eval) + VJP
status: open
priority: 3
issue-type: task
created-at: "2026-07-18T15:24:38.477881+02:00"
---

ABSENT. nanoGPT has dropout on embeddings/attention/residual (default 0.0 for pretrain, so low priority). Add a dropout op: bernoulli mask + 1/(1-p) scale in train mode, identity in eval mode, with a train/eval switch and its adjoint (mask*dy). Needs the RNG. Note: not on the critical path since pretrain dropout=0.0. Dep: RNG (weight-init dot).

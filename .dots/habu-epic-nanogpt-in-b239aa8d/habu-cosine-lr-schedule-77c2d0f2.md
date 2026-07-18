---
title: Cosine LR schedule + linear warmup
status: open
priority: 2
issue-type: task
created-at: "2026-07-18T15:24:38.463318+02:00"
---

ABSENT: no LR schedule in maki; lr is a fixed constant (from-scratch-train.f SC-LR 0.08, adam-train.f AMT-LR/ATN-LR). GPT-2/nanoGPT uses linear warmup then cosine decay to a min lr over max_iters. Add a checked LR-schedule word (warmup_iters, lr_decay_iters, min_lr, max_lr) returning lr at step t, threaded into the trainer step. Dep: trainer exists.

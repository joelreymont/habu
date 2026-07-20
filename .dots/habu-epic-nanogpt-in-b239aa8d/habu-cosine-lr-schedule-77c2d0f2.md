---
title: Cosine LR schedule + linear warmup
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-18T15:24:38.463318+02:00\""
---

ABSENT: no LR schedule in maki; lr is a fixed constant (from-scratch-train.f SC-LR 0.08, adam-train.f AMT-LR/ATN-LR). GPT-2/nanoGPT uses linear warmup then cosine decay to a min lr over max_iters. Add a checked LR-schedule word (warmup_iters, lr_decay_iters, min_lr, max_lr) returning lr at step t, threaded into the trainer step. Dep: trainer exists.

2026-07-20 SERIALIZED behind the adamw lane (spark): shared adam-train.f/trainer footprint.

2026-07-20 serialization released (weight-init landed f12d1842).
Claim: agent=coslr workspace=.jj-ws/fable-coslr machine=spark (owns maki/adam-train.f from-scratch-train.f + existing test files; global-norm-clip and host-batch-loop stay serialized behind this lane)

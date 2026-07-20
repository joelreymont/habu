---
title: Cosine LR schedule + linear warmup
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-07-18T15:24:38.463318+02:00\\\"\""
closed-at: "2026-07-20T10:58:34.082992+02:00"
close-reason: "Landed 53504232 (+ owner-row fix): LR-SCHED implements nanoGPT get_lr exactly (warmup lmax*(t+1)/(warmup+1), cosine decay, floor at lmin) with red-first domain guards (E-LR-SCHED -5159). Cosine built from scratch (no trig in tree): degree-12 Maclaurin on [0,pi/2] + exact reflection, measured max error 6.32e-9 at pi/2 vs proven series bound 6.4e-9, 7 closed-form angle goldens. Threading is opt-in: AMT-LR-EFF returns AMT-LR exactly when disarmed - committed -2749/9 locks bit-identical, verified re-lock after a scheduled run disarms. Scheduled 60-step proof run locks -2599 mNLL deterministically. Merge composed cleanly with the weight-init landing in the same file"
---

ABSENT: no LR schedule in maki; lr is a fixed constant (from-scratch-train.f SC-LR 0.08, adam-train.f AMT-LR/ATN-LR). GPT-2/nanoGPT uses linear warmup then cosine decay to a min lr over max_iters. Add a checked LR-schedule word (warmup_iters, lr_decay_iters, min_lr, max_lr) returning lr at step t, threaded into the trainer step. Dep: trainer exists.

2026-07-20 SERIALIZED behind the adamw lane (spark): shared adam-train.f/trainer footprint.

2026-07-20 serialization released (weight-init landed f12d1842).
Claim: agent=coslr workspace=.jj-ws/fable-coslr machine=spark (owns maki/adam-train.f from-scratch-train.f + existing test files; global-norm-clip and host-batch-loop stay serialized behind this lane)

---
title: Global-norm gradient clipping
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-07-18T15:24:38.467731+02:00\\\"\""
closed-at: "2026-07-20T11:24:14.344896+02:00"
close-reason: "Landed e25762ae: GRAD-CLIP-COEF = clip/(norm+1e-6) applied per-buffer only when coef<1 (torch clip_grad_norm_ semantics; not-clipping leaves buffers bit-identical, proven), wired into AMT-APPLY before the optimizer apply behind AMT-CLIP! arming (AMT-SCHED! idiom). Exact numeric golden (grads [3,4]+[12], norm 13, clip 1 -> scaled values and post-clip norm within 1e-6), zero-grad no-divide, red-first E-GRAD-CLIP guards (0/neg/inf/NaN), armed clip=0.1 run locks -2505 deterministically, committed -2749/-2599/9 locks untouched. Adam-MLP trainer only by design - SGD/attention trainers out of scope"
---

ABSENT: no grad clipping. nanoGPT clips global grad norm to 1.0. Add a word that computes the global L2 norm across ALL parameter gradient buffers (the BW gradient nodes bound in the executor) and rescales each by min(1, clip/norm) before the optimizer apply. Dep: executor/backward gradient-node access (from-scratch-train.f SC-GRAD-AT pattern) exists.

2026-07-20 SERIALIZED behind the adamw lane (spark): shared adam-train.f/trainer footprint.

2026-07-20 serialization released (cosine-LR landed 53504232).
Claim: agent=gclip workspace=.jj-ws/fable-gclip machine=spark (owns maki/adam-train.f from-scratch-train.f + existing test files; host-batch-loop and checkpoint stay serialized behind this lane)

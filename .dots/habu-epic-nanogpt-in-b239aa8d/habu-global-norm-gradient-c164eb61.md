---
title: Global-norm gradient clipping
status: open
priority: 2
issue-type: task
created-at: "2026-07-18T15:24:38.467731+02:00"
---

ABSENT: no grad clipping. nanoGPT clips global grad norm to 1.0. Add a word that computes the global L2 norm across ALL parameter gradient buffers (the BW gradient nodes bound in the executor) and rescales each by min(1, clip/norm) before the optimizer apply. Dep: executor/backward gradient-node access (from-scratch-train.f SC-GRAD-AT pattern) exists.

2026-07-20 SERIALIZED behind the adamw lane (spark): shared adam-train.f/trainer footprint.

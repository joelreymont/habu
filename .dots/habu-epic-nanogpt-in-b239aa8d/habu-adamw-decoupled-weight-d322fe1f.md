---
title: AdamW decoupled weight decay in tensor optimizer + trainer
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-07-18T15:24:38.457452+02:00\\\"\""
closed-at: "2026-07-20T09:33:23.757211+02:00"
close-reason: "Landed c1255380: decoupled AdamW w'=w-lr*wd*w-lr*mhat/(sqrt(vhat)+eps) in TT-ADAMW! (maki/optim-tensor.f:41) + explicit WD-NONE/WD-DECAY param-group flags with checked resolver (maki/adam-train.f:49-63); golden DISTINGUISHES decoupled from coupled L2 (differs on both w and m), wd=0 bit-identical to TT-ADAM!, red-first on both guards. Residual: AMT-WD=0.0 keeps the committed convergence locks - see habu-adopt-nanogpt-weight dot"
---

GPT-2 uses AdamW (decoupled weight decay). maki has the scalar rules ADAM + WEIGHT-DECAY separately (optim.f), but tensor TT-ADAM! (optim-tensor.f) applies NO decay and adam-train.f ADAM-UPD calls TT-ADAM! without it. Add decoupled decay (w -= lr*wd*w before/with the Adam step) to the tensor apply + trainer, with the standard no-decay-on-biases/LN param-group policy. Dep: optim-tensor.f exists.

Claim: agent=adamw workspace=.jj-ws/fable-adamw machine=spark (owns maki/optim.f optim-tensor.f adam-train.f + tests; trainer-family dots cosine-lr/weight-init/checkpoint/global-norm serialize behind this lane)

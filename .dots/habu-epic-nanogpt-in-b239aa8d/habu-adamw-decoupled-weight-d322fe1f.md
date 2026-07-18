---
title: AdamW decoupled weight decay in tensor optimizer + trainer
status: open
priority: 2
issue-type: task
created-at: "2026-07-18T15:24:38.457452+02:00"
---

GPT-2 uses AdamW (decoupled weight decay). maki has the scalar rules ADAM + WEIGHT-DECAY separately (optim.f), but tensor TT-ADAM! (optim-tensor.f) applies NO decay and adam-train.f ADAM-UPD calls TT-ADAM! without it. Add decoupled decay (w -= lr*wd*w before/with the Adam step) to the tensor apply + trainer, with the standard no-decay-on-biases/LN param-group policy. Dep: optim-tensor.f exists.

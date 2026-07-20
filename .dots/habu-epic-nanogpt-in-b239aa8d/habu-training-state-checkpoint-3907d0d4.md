---
title: Training-state checkpoint save/load (resume)
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-18T15:24:38.488557+02:00\""
---

Gradient-checkpointing (activation remat) EXISTS (maki/checkpoint.f) but is a DIFFERENT thing: nanoGPT checkpointing persists model params + optimizer moments (m,v) + step/iter to resume training. Verify whether store.f/golden-artifact.f can serialize+restore parameter and optimizer buffers; if not, add a checked save/load of (params, Adam m/v, step t, best loss). Dep: store layer; trainer state (adam-train.f ADAM-T/moment buffers).

2026-07-20 SERIALIZED behind the adamw lane (spark): shared adam-train.f/trainer footprint.

2026-07-20 serialization released (batch-loop landed 24b9f3f6).
Claim: agent=ckpt workspace=.jj-ws/fable-ckpt machine=spark (owns maki/adam-train.f + store-layer read-consumers + existing test files; the maki/examples/nanogpt restructure stays serialized behind this lane - last of the trainer wave)

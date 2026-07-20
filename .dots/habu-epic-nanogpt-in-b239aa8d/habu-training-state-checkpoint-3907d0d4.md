---
title: Training-state checkpoint save/load (resume)
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-07-18T15:24:38.488557+02:00\\\"\""
closed-at: "2026-07-20T12:45:38.676486+02:00"
close-reason: "Landed 9bd7cd28: maki/train-state.f checked checkpoint codec (frame = magic/version/ncells + raw 64-bit cell payload + FNV-1a-64 digest, reusing competitive-store's integrity pattern and store.f's root discipline after verifying NO existing layer round-trips floats bit-exactly - store.f is text rows, golden-artifact is 9-decimal lossy). AMT-CKPT-SEGS registers all 4 params + 8 Adam moments + step/bias-correction cells. Proofs: resume equivalence BIT-IDENTICAL to uninterrupted N+M (fixed-LR AND schedule+clip armed), t-dropped resume proven to diverge, missing/magic/truncated/flipped-byte/wrong-shape/over-cap each a named E-TSC-* throw with atomicity (failed load leaves state untouched), save->load->save byte-identical. Red-first per guard and per schema segment (dropping any state class reds resume equivalence)"
---

Gradient-checkpointing (activation remat) EXISTS (maki/checkpoint.f) but is a DIFFERENT thing: nanoGPT checkpointing persists model params + optimizer moments (m,v) + step/iter to resume training. Verify whether store.f/golden-artifact.f can serialize+restore parameter and optimizer buffers; if not, add a checked save/load of (params, Adam m/v, step t, best loss). Dep: store layer; trainer state (adam-train.f ADAM-T/moment buffers).

2026-07-20 SERIALIZED behind the adamw lane (spark): shared adam-train.f/trainer footprint.

2026-07-20 serialization released (batch-loop landed 24b9f3f6).
Claim: agent=ckpt workspace=.jj-ws/fable-ckpt machine=spark (owns maki/adam-train.f + store-layer read-consumers + existing test files; the maki/examples/nanogpt restructure stays serialized behind this lane - last of the trainer wave)

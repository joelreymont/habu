---
title: Dropout op (train/eval) + VJP
status: closed
priority: 3
issue-type: task
created-at: "2026-07-18T15:24:38.477881+02:00"
closed-at: "2026-07-20T22:37:37.483098+02:00"
close-reason: "Landed 29602d57: dropout op (train/eval + VJP), the last missing nanoGPT model piece, additive 18 files +450/-4. Attribute-based identity per the affine-LayerNorm precedent: both modes are arity-1 nodes, mode + fixed-point p packed in the attr cell, so the train/eval switch is a re-capture never an arity change. Deterministic reproducible Bernoulli mask from an own-stream LCG seeded per node from the node table index; the VJP node reads the forward node to reseed the identical stream, so run-twice is bit-locked, dropped grads are exactly zero, and gradcheck holds the mask fixed - V-PASS proven, plus exact-kept-count statistical golden, domain guards (E-DO-P red-first), fail-closed device-lowering rejects, and a GELU-DROPOUT-GELU integration where the eval switch is bit-identical to the dropout-free reference. Full cold gate green at the merged tip"
---

ABSENT. nanoGPT has dropout on embeddings/attention/residual (default 0.0 for pretrain, so low priority). Add a dropout op: bernoulli mask + 1/(1-p) scale in train mode, identity in eval mode, with a train/eval switch and its adjoint (mask*dy). Needs the RNG. Note: not on the critical path since pretrain dropout=0.0. Dep: RNG (weight-init dot).

2026-07-20 unblocked: cad/executor/backward/op-registry all free (stack merged).
Claim: agent=dropout workspace=.jj-ws/fable-dropout machine=spark (owns the dropout op end to end: op-registry/plan-vocab/cad capture arm/executor/backward/lower reject + tests; blconv owns src/habu - disjoint)

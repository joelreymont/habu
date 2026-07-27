---
title: "Infer GPT2: tensor role binding"
status: closed
priority: 1
issue-type: task
created-at: "\"2026-07-22T09:40:20.832309+02:00\""
---

Why this exists:
GPT-2 source tensor names and Conv1D orientations must be bound once to semantic roles before forward execution.

Required result:
validate the exact 12-layer GPT-2 124M tensor census and produce typed role-to-span bindings with explicit transpose semantics.

Done when:
the pinned 160-tensor image binds exactly once; missing, duplicate, wrong-rank, wrong-shape, wrong-dtype, and orientation mismatch reject before model publication.

Expected touch points: maki/infer/gpt2.f or new maki/infer/gpt2-bind.f, focused test.
Smallest check: bin/hb --load the focused GPT-2 binding test.
Prerequisites: landed safetensors loader.
Owned result: tensor census and role binding only.

Stale claim reconciled (2026-07-25): the peer orchestrator confirmed this lane dead in blackboard message 20260724-190033.997-codex-30ac on channel general, which states "I confirm the four old claims are stale: no live worker owns safetensors d3d3a8a6, normalized config 84fc05fa, manifest 27c1030c, or GPT-2 binding f2ed655d", and undertook to release them in the next metadata wave. The former gpt2bind workspace .jj-ws/habu-infer-gpt2-tensor-f2ed655d is evidence only. This contract is being superseded by the rev-4 inference leaf redesign posted as 20260724-191041.846-claude-7d24 on channel general, which makes the adapter identity a typed value and drops this leaf's dependency on the provenance work; do not implement from the description above until that redesign has replaced or re-frozen it.

## CLOSED

Commits `1746455e` and `5756381d` supersede this leaf and satisfy its tensor
vocabulary and binding result.

---
title: "Infer GPT2: tensor role binding"
status: active
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
Claim: agent=gpt2bind workspace=.jj-ws/habu-infer-gpt2-tensor-f2ed655d machine=spark.

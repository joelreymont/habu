---
title: Decouple schedule/clip/Adam-step facilities into train-core
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-20T17:16:53.326071+02:00\""
---

Follow-up from the eval authoring-surface landing (bbeb380f), same shape as the landed init-role/AdamW-policy decouple (4ffe4da6): the framework's generic training facilities still live in the nanoGPT EXAMPLE - LR-SCHED (the degree-12 cosine + warmup, from-scratch-train.f), GRAD-CLIP-COEF/GCLIP-SCALE! (global-norm clip math), and the host Adam step/bias-correction bookkeeping (adam-train.f) - so the eval leg had to reimplement eval-local arming (ET-LR/ET-CLIP over library math) rather than grade the exact framework words, and every future consumer faces the same choice. Extract byte-identically into maki/train-core.f (the established home), example requires the library, all example locks stay bit-identical (AMT -2749, scheduled -2599, clipped -2505, batch locks, checkpoint resume), then rewire the eval leg to the extracted words and retire ET-LR/ET-CLIP. Acceptance rg: no library file references the example (the landed decouple's mechanical proof, re-run). Territory: maki/train-core.f, maki/examples/nanogpt/from-scratch-train.f + adam-train.f (extraction only), maki/eval/train.f rewire.

Claim: agent=traincore2 workspace=.jj-ws/fable-traincore2 machine=spark (owns maki/train-core.f + examples/nanogpt/from-scratch-train.f adam-train.f extraction + maki/eval/train.f rewire; the SPEC-line eval follow-up habu-eval-leg-authors-00367fe9 serializes BEHIND this lane - same eval file)

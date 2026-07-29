---
title: Use canonical checkpoint dtype
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T20:34:14.954473+02:00"
---

Problem: SAFET publishes raw numeric dtype constants while MAKI:dtype is the declared semantic authority; GPT2LOAD relies on numeric equality and a second model family would duplicate the mistake. Result: SAFET:DTYPE? returns MAKI:dtype and every checkpoint tensor is compared directly with the validated MDLCFG dtype before WSTORE publication. Delete the public SAFET numeric dtype vocabulary and every cross-package numeric comparison. Preserve safetensors wire decoding privately and reject unsupported wire codes by name. Owner: SAFET wire decode plus direct GPT2LOAD callers. Production red: swapping two equal-cell dtype domains is checker-valid today. Acceptance: checked negatives reject raw numeric and foreign dtype arguments; all GPT-2 tensors validate as F32; wrong and unsupported wire dtypes reject before model publication; SAFET, model-config, GPT2LOAD, and exact diff gates pass. Claim: unassigned.

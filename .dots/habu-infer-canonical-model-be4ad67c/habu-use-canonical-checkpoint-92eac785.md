---
title: Use canonical checkpoint dtype
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T20:34:14.954473+02:00"
---

Problem: SAFET publishes raw numeric dtype constants while MAKI:dtype is the semantic authority; checkpoint consumers compare equal-cell numbers. Result: SAFET:DTYPE? returns MAKI:dtype and the existing GPT2LOAD caller compares it directly with validated MDLCFG dtype. Delete the public SAFET numeric dtype vocabulary and every current cross-package numeric comparison. Preserve wire decoding privately and reject unsupported codes by name. Later GPT2DEV and DEVRT Qwen staging leaves own their integration with this result. Owner: SAFET dtype decode plus the existing GPT2LOAD caller only. Production red: swapping the two numeric domains is checker-valid. Acceptance: checked negatives reject raw numeric and foreign dtype arguments; focused SAFET fixtures decode F32 and BF16 to the exact MAKI arms; wrong and unsupported wire codes reject; SAFET, model-config, GPT2LOAD, and exact-diff gates pass without either future device loader. Claim: unassigned.

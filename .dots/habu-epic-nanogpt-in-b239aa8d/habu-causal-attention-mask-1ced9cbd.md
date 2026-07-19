---
title: Causal attention mask (masked softmax) + adjoint
status: closed
priority: 1
issue-type: task
created-at: "\"2026-07-18T15:24:38.452623+02:00\""
closed-at: "2026-07-19T06:49:59.395159+02:00"
---

ABSENT: no masking op anywhere; SOFTMAX-ROW (softmax.f SM-FWD, cad.f) is unmasked. GPT-2 needs causal masking (upper-triangular scores set to -inf before row softmax). Add a causal-mask word/op feeding SOFTMAX-ROW (mask attn scores to -inf on j>i) with its adjoint (masked positions get zero cotangent). Golden first over attention.f MM-NT scores. Dep: softmax.f exists.

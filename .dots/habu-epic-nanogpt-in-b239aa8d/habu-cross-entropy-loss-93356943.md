---
title: Cross-entropy loss over logits+int targets (log-softmax, tensor+seed)
status: open
priority: 1
issue-type: task
created-at: "2026-07-18T15:24:38.442271+02:00"
---

GPT-2 trains on softmax cross-entropy over logits with INTEGER token targets; maki only has one-hot pre-softmax CE golden (maki/celoss.f CE + SOFTMAX-CE-BWD=y-t) that is NOT in loss-tensor.f and NOT wired into any trainer (trainers seed NLL/MSE only, from-scratch-train.f/adam-train.f). Add: (a) numerically-stable tensor log-softmax+CE reduction over (logits RxV, targets R ints); (b) the y-t seed cotangent wired into BW-BUILD training loop (BW-SEED-SLOT). Dep: softmax.f/celoss.f goldens exist.

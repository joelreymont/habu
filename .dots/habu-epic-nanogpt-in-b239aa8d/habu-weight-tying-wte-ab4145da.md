---
title: Weight tying (wte <-> lm_head) with gradient accumulation
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-18T15:25:04.155430+02:00\""
blocks:
  - habu-gpt-2-block-a9039501
---

GPT-2 ties the token-embedding table with the LM-head weight (one shared buffer). maki IR binds each input slot to a DISTINCT buffer (executor EX-BIND); tying the same buffer to two slots yields two separate gradient nodes that must be SUMMED before the optimizer step (or the executor must accumulate). Add shared-parameter binding + gradient accumulation across the tied slots. Hard-blocked on the full-model composition dot.

2026-07-20 unblock note: the full-model composition wall fell (2efa4388 trainable MHA + 8207fd54 block + the accumulate-across-slots pattern landed in the batch trainer 24b9f3f6). Hard-block satisfied.
Claim: agent=wtie workspace=.jj-ws/fable-wtie machine=spark (owns a NEW examples/nanogpt tying test + minimal trainer glue in examples files; executor.f/backward.f are svcore-owned - READ-ONLY, the summation lands at the trainer level like the batch accumulators; cad.f is slotref-owned - read-only)

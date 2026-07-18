---
title: Weight tying (wte <-> lm_head) with gradient accumulation
status: open
priority: 2
issue-type: task
created-at: "2026-07-18T15:25:04.155430+02:00"
blocks:
  - habu-gpt-2-block-a9039501
---

GPT-2 ties the token-embedding table with the LM-head weight (one shared buffer). maki IR binds each input slot to a DISTINCT buffer (executor EX-BIND); tying the same buffer to two slots yields two separate gradient nodes that must be SUMMED before the optimizer step (or the executor must accumulate). Add shared-parameter binding + gradient accumulation across the tied slots. Hard-blocked on the full-model composition dot.

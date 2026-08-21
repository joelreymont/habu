---
title: Weight tying (wte <-> lm_head) with gradient accumulation
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-07-18T15:25:04.155430+02:00\\\"\""
closed-at: "2026-07-20T21:44:21.770128+02:00"
close-reason: "Landed 47f249c2: GPT-2 weight tying at the trainer level. Probe-verified premise: EX-BIND is pointer-only so the tie is a TRANSPOSED role - one logical (V,C) parameter, a mirrored (C,V) transpose refreshed after every step, gradients summed (head side transposed) into ONE Adam moment pair. Five proofs: FD on the shared table equals the summed slot gradients; deterministic locked training (1781->1 mCE, raw-float run-twice bit-identical); the mirror verified equal after every step; the untied baseline diverges (L2 ~5.09 - the tie genuinely changes optimization); red-first shape guard E-TIE-SHAPE"
---

GPT-2 ties the token-embedding table with the LM-head weight (one shared buffer). maki IR binds each input slot to a DISTINCT buffer (executor EX-BIND); tying the same buffer to two slots yields two separate gradient nodes that must be SUMMED before the optimizer step (or the executor must accumulate). Add shared-parameter binding + gradient accumulation across the tied slots. Hard-blocked on the full-model composition dot.

2026-07-20 unblock note: the full-model composition wall fell (2efa4388 trainable MHA + 8207fd54 block + the accumulate-across-slots pattern landed in the batch trainer 24b9f3f6). Hard-block satisfied.
Claim: agent=wtie workspace=.jj-ws/fable-wtie machine=spark (owns a NEW examples/nanogpt tying test + minimal trainer glue in examples files; executor.f/backward.f are svcore-owned - READ-ONLY, the summation lands at the trainer level like the batch accumulators; cad.f is slotref-owned - read-only)

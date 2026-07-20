---
title: Autoregressive generation (sampling) from a trained model
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-20T15:55:41.471433+02:00\""
---

Gap found 2026-07-20 answering 'what is missing to run GPT-2': the epic has NO generation/inference dot - every landed surface trains; nothing decodes. nanoGPT's sample loop: crop context to block size, forward, take last-position logits, temperature-divide, optional top-k mask, softmax, multinomial-sample (or argmax for greedy), append, repeat. Build it on the landed pieces: forward-only execution of the trained MODEL: (executor already runs forward without BW-BUILD), the tokenizer's DECODE (bounded, landed fe907212), the library RNG (train-core.f LCG - multinomial sampling from softmax probs via inverse-CDF over the row). v0 is full-context re-forward per token (honest at toy extents); an incremental KV-cache path is a SEPARATE later dot (device-side, belongs with the batched-attention plan node). Proofs: greedy decode from a deterministically-trained model is itself deterministic (locked token sequence); temperature 0-limit equals argmax; top-k masks provably exclude out-of-k tokens; sampling with a fixed seed locks the sequence; decode round-trips through the tokenizer. Territory: maki/examples/nanogpt (a generate.f + test), library additions only if a genuinely generic piece emerges (multinomial-from-row might be library).

Claim: agent=gen workspace=.jj-ws/fable-gen machine=spark (owns NEW maki/examples/nanogpt/generate.f + test + registration; executor/model machinery READ-ONLY - the derive lane owns those files; bpe lane owns bpe*)

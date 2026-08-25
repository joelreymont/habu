---
title: Decode one GPT-2 token greedily
status: closed
priority: 2
issue-type: task
created-at: "2026-08-03T15:02:57.282015+02:00"
closed-at: "2026-08-03T16:15:08.571863+02:00"
close-reason: Merged as c44f8e067fa23c32ec551f437c0ea056e508cd99; exact maki, lint-libs/ptx-stdlib, device, diff, error, dependency, and dot gates green.
---

Why: GPT2:LOGITS exposes the production row but every caller would otherwise duplicate raw-F32 selection. Result: add maki/infer/gpt2-greedy.f in package GPT2 with public GREEDY ( GPT2:model n ptr u8 CAD-NUM:byte-len -- GPT2:model result<n,n> ). GREEDY calls the existing LOGITS exactly once, returns its error unchanged, then scans the complete caller-owned F32 row; every value must be finite, the first maximum wins ties, and ok carries the next integer token id. A non-finite row returns one named numeric error after the input token has committed; callers abort or close that generation. It allocates nothing and retains nothing. Dependencies: landed GPT2:model LOGITS/RESET and F32 buffer reader. Owner: one-token greedy selection only; LOGITS remains the sole forward path. Acceptance: the mandatory standalone DGX Spark test calls GREEDY through real OPEN on token 15496 and gets GPT2-REFERENCE id 0, then feeds that id and gets id 1; RESET repeats; synthetic rows exercise first-max ties, NaN, positive infinity, and negative infinity through the same private scanner; invalid token and wrong row length preserve position; two models still coexist and close in both orders. The following CLI slice must loop GREEDY and match all 64 GPT2-REFERENCE identifiers. Forbidden: GENERATE, tokenizer, sampling config, random state, engine, sequence, cache, scheduler, runtime, descriptor, registry, batch, paging, host forward, allocation, new public type, ABI/version, compatibility, manifest, lint, suite enrollment, skip, performance assertion, or raw-logit copy. Smallest owning check: bin/hb --load maki/infer/gpt2-logits-device-test.f -- /home/joel/Work/Habu/gpt2-model. Claim: agent=codex-gpt2-greedy workspace=.jj-ws/habu-decode-one-gpt-ddf01aac.

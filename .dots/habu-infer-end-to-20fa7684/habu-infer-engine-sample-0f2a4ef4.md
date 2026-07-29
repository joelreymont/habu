---
title: Sample next tokens
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:41:52.412930+02:00"
blocks:
  - habu-infer-engine-prefill-558f9003
  - habu-own-sampling-value-6dc1a8cf
---

Why this exists:
token selection must respect the active model's valid identifier domain and ordered bounded stop set while consuming sequence-owned logits without global random state or advancement after failure.

Required result:
package INFER owns one caller-row NEXT-MANY transition over the upstream SAMPLE values and the landed MAKI:SMP-NEXT selector; it does not define either. A caller-owned next-row contains only one INFER:seq and one bounded output span. NEXT-MANY returns the exact row sum token(seq,id,off,len) | token-final(seq,id,off,len,length,prompt-n,completion-n) | finished(seq,stop,prompt-n,completion-n). It validates one or more distinct rows up to the active model batch capability, preflights every output, copies only logits [0,VALID-COUNT) into preallocated scratch, and calls MAKI:SMP-NEXT once per row using the sequence-owned config and random state. It tests the selected identifier against the active model's SAMPLE:stop-set in stored order; a match returns finished with the matching stop reason and never enters RUN-ROWS. A non-stop identifier detokenizes into sequence scratch and enters one RUN-ROWS call. After RUN-ROWS commits, token bytes, random state, accumulated output, and either token or length terminal publication are total because every span and maximum count was preflighted. NEXT-MANY never closes a sequence or reclaims a request. Any earlier refusal returns every row, output span, random state, history, logit row, and KV boundary unchanged. GPT-2 supplies `{50256}` over 50257 rows. Qwen-specific valid-count, two-stop ordering, and padded-row acceptance belong to the later Qwen model and end-to-end leaves. There is no separate public NEXT. No selector implementation, sample-type declaration, OPEN-SEQ change, per-call sampling argument, tokenizer-owned stop policy, global random stream, device sampler, output allocation, hidden retry, raw logit access, or state advance on rejection.

Done when:
GPT-2 greedy and fixed-seed top-k/top-p sequences match the landed fixtures; two sequences interleave without cross-talk; identifier 50256 finishes without a cache append; zero rows, over-capability, duplicate sequence, short output, logit copy, selector, RUN-ROWS, and detokenizer failure preserve all rows; emitted bytes and identifier agree with the GPT-2 tokenizer.

Expected touch points: maki/infer/engine.f and its focused output test only.
Smallest check: bin/hb --load the focused engine output test through one real GPT-2 step.
Prerequisites: upstream SAMPLE values, one-token prefill, RUN-ROWS, and the landed MAKI:SMP-NEXT module.
Owned result: NEXT-MANY sampling, detokenization, and output publication only.
Claim: unassigned.

---
title: Run Qwen through INFER
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:41:52.463168+02:00"
blocks:
  - habu-add-qwen-infer-3fd69d6d
  - habu-infer-dense-host-4c9152ad
  - habu-infer-engine-64-02416606
---

Why: internal Qwen operations do not prove the public engine runs a second model without a second execution path.

Result: a production-path acceptance opens the pinned Qwen root through `INFER:OPEN-QWEN`, starts the existing shared engine with one live sequence, encodes the exact pinned 201-byte chat prompt, prefills its 39 identifiers, and performs up to 64 greedy paged steps with request overrides `do_sample=false`, one beam, repetition penalty 1, EOS identifiers 151645 and 151643, and pad identifier 151643 from QWENPIN. Greedy argmax scans exactly logits `[0,151665)` and never padded head rows 151665 through 152063. It compares every emitted identifier and decoded byte with QWEN-REFERENCE and records paired checkpoints before/after generation indices 9, 25, 41, and 57, where prompt length 39 and page size 16 allocate new pages, then closes sequence, engine, and model. Run the same path twice from fresh owners.

Add no Qwen engine, host fallback, pack, plugin, callback, alternate tokenizer path, benchmark record, compatibility mode, or reference generation. Owner: real Qwen public-INFER correctness fixture only. Production red: INFER currently has no Qwen arm or end-to-end call. Acceptance: the owning operation tests satisfy their exact elementwise BF16 criteria; both public runs match all 64 pinned identifiers and decoded bytes exactly; WSTORE, SAFET, and QWENIDX are zero before model publication; completion at either stop identifier causes no further device step; injected open, prefill, step, decode, and close failures release or return every owner exactly once. Smallest owning check: one required correctness-only GB10 Qwen continuation test through the public INFER entry point. Claim: unassigned.

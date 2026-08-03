---
title: Emit GPT-2 decode attention
status: active
priority: 1
issue-type: task
created-at: "2026-08-03T09:38:58.831561+02:00"
---

Why: one-row GPT-2 decode needs the real causal multi-head attention step and durable K/V writes. Result: one checked Forth PTX entry consumes Q/K/V, contiguous per-layer K/V storage, output, position, head count, head width, and context cap; it writes current K/V once, computes scaled causal attention over positions 0..position for every head, and produces the concatenated attention row. One block owns one head; scores are bounded by the validated context. No T=1 shortcut, paging scaffold, alternate attention, descriptor, cache owner, host fallback, ABI version, or committed binary. Owner: GPT2 decode-attention PTX emission only. Production red: no device entry both records and consumes GPT-2 KV. Acceptance: real CUDA matches an independent F32 golden at positions 0, 1, page-like edges, and final context, with wrong extents refused before launch and canary-proven bounds. Smallest owning check: focused GPT-2 attention-kernel device test.

Claim: agent=codex-gpt2-attention workspace=.jj-ws/habu-emit-gpt-2-a63e5f2e

---
title: Execute GPT-2 device block
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T20:40:51.585301+02:00"
blocks:
  - habu-exec-gpt-2-29bea6c6
  - habu-exec-gpt-2-dcb9a189
  - habu-exec-gpt-2-cf349cf9
---

Problem: the independently proven GPT-2 stages are not composed into one decoder block. Result: GPT2DEV:BLOCK takes the live DEVRT owner, device weights, one immutable provisional KV descriptor, authenticated layer and position, and device activation state; invokes QKV, ATTN, then MLP exactly once; and returns launch state plus owners. It never synchronizes, retains, commits, aborts, or mutates a KV owner. RUN-ROWS alone pairs pending device work with KV:ready. Owner: GPT-2 device-block composition only. Production red: no real model call joins the three exact stages. Acceptance: selected first and last block intermediates match GPT2-REFERENCE; first token, page boundary, and capacity edge pass; stage order, wrong descriptor, and injected launch failures preserve every owner and committed length; repeated calls reuse the same addresses. Forbidden: kernel implementation, host fallback, duplicate attention, per-layer compilation or allocation, raw pointer, contiguous cache, second descriptor, second execution plan, or KV authority. Smallest owning check: bin/hb --load maki/infer/gpt2-device-block-test.f on DGX Spark. Claim: unassigned.

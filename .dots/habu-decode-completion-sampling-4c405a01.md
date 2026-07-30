---
title: Decode completion sampling options
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T07:03:53.662445+02:00"
blocks:
  - habu-decode-required-completion-f0719d07
---

Why: supported OpenAI scalar options must extend the same decoder without a second request type or parser pass. Result: extend OPENAI-COMP:DECODE to accept optional max_tokens, temperature, top_p, seed, and n in arbitrary order while retaining the required-field path. max_tokens is positive, temperature is finite in [0,2], top_p is finite in (0,1], seed is an integer, and n is omitted or exactly 1. It constructs the canonical SAMPLE:config with top-k equal to INFER:info.valid-count and retains all named defaults when omitted. Owner: supported scalar option handlers and validation only. Dependency: required completion decode. Production red: the required decoder rejects every supported override. Acceptance: minimum, maximum, default, reordered, duplicate, wrong-type, non-finite, one-outside, overflow, and n-not-one cases select exact DECODE arms without a second parse or request type. Forbidden: stream, stop, unsupported fields, top-k option, permissive coercion, unnamed default, compatibility, metric, or lint. Smallest owning check: the production DECODE option matrix through maki/serve/openai-decode-test.f.

Claim: unassigned.

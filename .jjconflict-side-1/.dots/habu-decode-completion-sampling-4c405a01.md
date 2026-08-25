---
title: Decode completion sampling options
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T07:03:53.662445+02:00"
blocks:
  - habu-decode-required-completion-f0719d07
---

Why: supported OpenAI scalar options must extend the same decoder without a second request type or parser pass. Result: extend OPENAI-COMP:DECODE to accept optional max_tokens, temperature, top_p, seed, and n in arbitrary order while retaining the required-field path. max_tokens is positive, seed is an integer, and n is omitted or exactly 1. The same request value carries raw temperature, raw top-p, raw top-k equal to INFER:info.valid-count, and seed while retaining every named default when omitted. DECODE performs field, role, representation, and request-policy checks but no sampling-domain check; INFER:OPEN-SEQ is the sole authority and validates the raw scalars once at the engine edge. Owner: supported scalar option handlers and request-policy validation only. Dependency: required completion decode. Production red: the required decoder rejects every supported override. Acceptance: minimum, maximum, default, reordered, duplicate, wrong-type, non-finite, one-outside, overflow, and n-not-one cases select the exact DECODE or engine-boundary arm without a second parse, request type, or sampling-domain check; every decoded raw scalar reaches OPEN-SEQ unchanged. Forbidden: stream, stop, unsupported fields, top-k option, permissive coercion, unnamed default, compatibility, metric, or lint. Smallest owning check: the production DECODE option matrix through maki/serve/openai-decode-test.f.

Claim: unassigned.

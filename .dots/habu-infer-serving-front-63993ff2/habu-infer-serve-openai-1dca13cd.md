---
title: Decode completion JSON
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T10:07:43.943179+02:00"
blocks:
  - habu-infer-scheduler-bounded-53574658
---

Why: HTTP syntax must not define scheduler semantics, and OpenAI field handling must be one exact supported product subset rather than an implied full API.

Result: OPENAI-COMP:DECODE accepts one bounded body, a caller-owned decoded-prompt buffer, and the active model name. It copies JSON-unescaped prompt bytes through JR:STR and returns one request value with required matching model; optional max_tokens default 16, temperature default 1.0 in [0,2], top_p default 1.0 in (0,1], integer seed default 0, and n omitted or exactly 1. It maps directly to maximum output, INFER sample-config with no top-k limit, and seed for SCHED:SUBMIT. Any stream field rejects. Duplicate, wrong type, non-finite, out-of-range, unknown, prompt array, short prompt buffer, stop, suffix, echo, logprobs, best_of, logit_bias, or user rejects before admission. Owner: OpenAI completion request decoding only. Production red: no production JSON maps to one scheduler request. Acceptance: exact and reordered canonical requests decode; escaped prompts use caller storage; duplicate keys, hostile strings, wrong roles, types, ranges, model, stream, unsupported fields, short prompt, and malformed JSON reject before SUBMIT. Forbidden: response rendering, streaming, chat, token-array prompt, multiple choices, model alias, permissive unknown field, schema, DOM, transport framing, version, compatibility, or unnamed default. Smallest owning check: bin/hb --load maki/serve/openai-decode-test.f. Claim: unassigned.

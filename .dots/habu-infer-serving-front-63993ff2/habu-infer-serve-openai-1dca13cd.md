---
title: Finish strict completion decode
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T10:07:43.943179+02:00"
blocks:
  - habu-decode-completion-sampling-4c405a01
---

Why: the required and scalar leaves build the production decoder; this leaf closes the exact supported OpenAI field set. Result: finish the same OPENAI-COMP:DECODE pass so any stream field, stop, suffix, echo, logprobs, best_of, logit_bias, user, unknown field, duplicate field, wrong role, or unsupported prompt form refuses before publication. Success returns the one request value from the earlier leaves with raw temperature, top-k, top-p, and seed; scheduler admission passes those scalars unchanged to INFER:OPEN-SEQ for the sole sampling-domain validation at the engine boundary. No public sampling value or second request representation exists. Owner: final strict-field dispatch and decoder integration only. Production red: supported scalar fields exist but the complete field policy has not been proven through one pass. Acceptance: exact and reordered canonical requests decode; hostile strings, duplicates, unsupported fields, wrong roles and types, malformed JSON, and stream true or false all select exact DECODE arms before scheduler admission. Forbidden: response rendering, streaming, chat, token-array prompt, multiple choices, model alias, permissive unknown field, schema, DOM, transport framing, version, compatibility, metric, or lint. Smallest owning check: bin/hb --load maki/serve/openai-decode-test.f. Claim: unassigned.

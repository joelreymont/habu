---
title: Decode required completion fields
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T07:03:53.505359+02:00"
blocks:
  - habu-infer-engine-owned-99a98d17
---

Why: model and prompt ownership must be correct before optional sampling fields enlarge the decoder. Result: package OPENAI-COMP defines the final immutable request value and production DECODE over one bounded JSON body, caller-owned prompt storage, and canonical INFER:info. This first cut accepts exactly one matching model string and one string prompt in either order, copies JSON-unescaped prompt bytes through JR:STR, and supplies named product defaults max_tokens 16, raw temperature 1.0, raw top-p 1.0, raw top-k equal to info.valid-count, seed 0, and n 1. The decoded request carries those raw sampling values to INFER:OPEN-SEQ, whose engine-edge validation is authoritative; request typing is unchanged and no sampling config is constructed. Missing, duplicate, wrong-type, mismatched-model, prompt-array, malformed, unknown, or short-prompt input refuses before publication. Owner: request value plus required-field decode only. Production red: no production JSON yields one typed completion request. Acceptance: exact and reordered required requests enter the real DECODE result; escaping, duplicate keys, hostile strings, wrong roles, malformed input, mismatched model, and one-short prompt storage refuse without changing caller storage; defaults are exact. Forbidden: optional-field override, response render, DOM, schema, transport, compatibility, metric, or lint. Smallest owning check: bin/hb --load maki/serve/openai-decode-test.f.

Claim: unassigned.

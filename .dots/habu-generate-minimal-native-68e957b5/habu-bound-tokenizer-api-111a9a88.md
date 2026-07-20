---
title: Bound tokenizer API
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-19T22:43:11.376143+02:00\""
---

Current master memory-safety and semantic-validation defect in new maki/tokenizer.f. Public TOK-ID accepts arbitrary n and executes TOK-INV b cells + @ before checking the loaded value, so b<0 or b>255 reads outside the 256-cell table. Before any TOK-BUILD, zero-image TOK-INV makes every byte appear to map to token 0, so TOK-ID and TOK-ENCODE silently succeed with no vocabulary. TOK-ENCODE and TOK-DECODE reject only u>cap; negative u or cap reaches raw ?do/addressing, and decode converts arbitrary float cells through f>s, accepting fractional, NaN, infinite, or out-of-range values according to conversion accident instead of exact token identity. Add refined byte, token-id, nonnegative length, and capacity boundaries; reject every invalid domain before address arithmetic or loop entry; require a ready vocabulary; decode must prove each stored value is finite, exactly integral, and within the current vocabulary before conversion/write. Add canaries and -1/0/255/256/max-cell byte cases, pre-build calls, negative/overflow lengths and capacities, fractional/NaN/infinite/huge float ids, exact-capacity buffers, and property tests that every successful lookup stays within both tables and round-trips exactly. No rejected call may read or write caller/module buffers. Preserve sorted stable byte vocabulary and valid encoded bytes. Files: maki/tokenizer.f and focused tests. Coordinate representation and package ownership with habu-own-tokenizer-state; this dot owns immediate API bounds and value validation only.

Claim: agent=toksafe workspace=.jj-ws/habu-bound-tokenizer-api-111a9a88

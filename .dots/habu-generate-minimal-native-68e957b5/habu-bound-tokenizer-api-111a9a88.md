---
title: Bound tokenizer API
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-07-19T22:43:11.376143+02:00\\\"\""
closed-at: "2026-07-20T10:47:17.512780+02:00"
close-reason: "Landed fe907212: TOK-READY gate (empty-vocab calls throw E-TOK-EMPTY, kills the zero-image every-byte-maps-to-0 hole), byte-domain check before table indexing (b<0/b>=256 rejected before address arithmetic), TOK-CELL>ID proves decode cells finite+integral+in-vocab before conversion (NaN/inf/fractional/negative all named rejects), negative length/capacity guards before loop entry. Red-first both-direction: 13 new tests fail on the unfixed base, all green after; canaries prove rejected calls write nothing; 40-corpus round-trip property. Representation/package rework stays with habu-own-tokenizer-state-d5db1943"
---

Current master memory-safety and semantic-validation defect in new maki/tokenizer.f. Public TOK-ID accepts arbitrary n and executes TOK-INV b cells + @ before checking the loaded value, so b<0 or b>255 reads outside the 256-cell table. Before any TOK-BUILD, zero-image TOK-INV makes every byte appear to map to token 0, so TOK-ID and TOK-ENCODE silently succeed with no vocabulary. TOK-ENCODE and TOK-DECODE reject only u>cap; negative u or cap reaches raw ?do/addressing, and decode converts arbitrary float cells through f>s, accepting fractional, NaN, infinite, or out-of-range values according to conversion accident instead of exact token identity. Add refined byte, token-id, nonnegative length, and capacity boundaries; reject every invalid domain before address arithmetic or loop entry; require a ready vocabulary; decode must prove each stored value is finite, exactly integral, and within the current vocabulary before conversion/write. Add canaries and -1/0/255/256/max-cell byte cases, pre-build calls, negative/overflow lengths and capacities, fractional/NaN/infinite/huge float ids, exact-capacity buffers, and property tests that every successful lookup stays within both tables and round-trips exactly. No rejected call may read or write caller/module buffers. Preserve sorted stable byte vocabulary and valid encoded bytes. Files: maki/tokenizer.f and focused tests. Coordinate representation and package ownership with habu-own-tokenizer-state; this dot owns immediate API bounds and value validation only.

Claim: agent=tokbound workspace=.jj-ws/fable-tokbound machine=spark (owns maki/tokenizer.f + its focused tests; API bounds/validation only - representation+package rework stays with habu-own-tokenizer-state-d5db1943)

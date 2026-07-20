---
title: Make tokenizer encode/decode writes atomic
status: open
priority: 2
issue-type: task
created-at: "2026-07-20T11:16:28.876789+02:00"
---

maki/tokenizer.f TOK-ENCODE and TOK-DECODE (as landed by the tokbound lane) validate each element inside the single write loop, so a rejection at element N throws after elements 0..N-1 were already stored into the caller's destination buffer. The bounds dot habu-bound-tokenizer-api-111a9a88 specified that no rejected call may write caller buffers; partial output on a failed decode also risks being consumed as if complete. Split both words into a validate-all pass (byte/id/exact-cell checks, no writes) followed by a write pass over the proven-valid elements, keeping the landed error codes and TOK-CELL>ID semantics. A reference two-pass implementation exists on the pushed bookmark recover-toksafe (commit a9750ec0, maki/tokenizer.f TOK-ENC-CHECK/TOK-ENC-WRITE and TOK-DEC-CHECK/TOK-DEC-WRITE); re-derive against the landed representation rather than raw-merging. Add regressions proving a mid-buffer invalid element leaves the destination byte-identical (canary fill before the call). Coordinate with habu-own-tokenizer-state-d5db1943 if representation changes land first.

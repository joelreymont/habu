---
title: Write private byte buffers as vectors
status: open
priority: 2
issue-type: task
created-at: "2026-11-24T23:34:33.386041+01:00"
---

Problem: Joel 2026-11-19: "no agent should be using "^u8 n" anymore! it's either str or vector". Batch lanes leave private words that write a caller's ^u8 n buffer: src/lib/net/tcp4-test.f FILL-PATTERN, src/lib/net/test-issuer-test.f PUT and DECODED, src/lib/net/model-test.f APPEND and XS. Acceptance: each writes into a VEC:vector<u8> its caller owns; no private row names a ^u8 n destination. Depends: none. Claim: unassigned.

---
title: "Take CURL's stream bytes in vectors"
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T16:54:38.980976+01:00"
---

Problem: Joel's rule (2026-11-19) "no agent should be using "^u8 n" anymore! it's either str or vector" (docs/forth-card.md:126-127). CURL:START, CURL:PERFORM and CURL:HEADERS append to a caller's vector, but the streamed path's NEXT and NEXT-ARM ( handle ^u8 len -- ... ) in src/lib/net/curl.f still take a span as ^u8 len. Acceptance: each writes into a ^VEC:vector<u8> the caller lends, on the loop task without calling the allocator, as the whole-body window does; no CURL public effect says ^u8 len; every caller in Habu and the consumers' notice in the same install. Depends: habu-buf-buf-a-48c31f14, which turns model.f's chunk and reply BUFs, the stream's only Habu caller, into vectors. Claim: unassigned.

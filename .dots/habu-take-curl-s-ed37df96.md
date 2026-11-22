---
title: "Take CURL's header and stream bytes in vectors"
status: active
priority: 2
issue-type: task
created-at: "2026-11-19T16:54:38.980976+01:00"
---

Problem: Joel's rule (2026-11-19) "no agent should be using "^u8 n" anymore! it's either str or vector" (docs/forth-card.md:126-127). CURL:START and CURL:PERFORM append a whole body to a caller's vector, but CURL:HEADERS ( handle ^u8 len -- ) and the streamed path's NEXT and NEXT-ARM ( handle ^u8 len -- ... ) in src/lib/net/curl.f still take a span as ^u8 len. Acceptance: each writes into a ^VEC:vector<u8> the caller lends (the streamed path on the loop task without calling the allocator, as the whole-body window does); no CURL public effect says ^u8 len; every caller in Habu and the consumers' notice in the same install. Depends: the streamed half (NEXT, NEXT-ARM) waits on habu-buf-buf-a-48c31f14, which turns model.f's chunk and reply BUFs, the stream's only Habu caller, into vectors. Claim: agent=worker workspace=.jj-ws.noindex/carl-curlhdr (HEADERS).

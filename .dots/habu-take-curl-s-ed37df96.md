---
title: "Take CURL's header and stream bytes in vectors"
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T16:54:38.980976+01:00"
---

Problem: Joel's rule (2026-11-19) "no agent should be using "^u8 n" anymore! it's either str or vector" (docs/forth-card.md:126-127). After the whole-body cutover (workspace .jj-ws.noindex/carl-curlvec, design /tmp/carl-curlvec-design.md), CURL:HEADERS ( handle ^u8 len -- ) and the streamed path's NEXT and NEXT-ARM ( handle ^u8 len -- ... ) in src/lib/net/curl.f still take a span as ^u8 len. Acceptance: each writes into a ^VEC:vector<u8> the caller lends (the streamed path on the loop task without calling the allocator, as the whole-body window does); no CURL public effect says ^u8 len; every caller in Habu and the consumers' notice in the same install. Depends: the whole-body cutover landing. Claim: unassigned.

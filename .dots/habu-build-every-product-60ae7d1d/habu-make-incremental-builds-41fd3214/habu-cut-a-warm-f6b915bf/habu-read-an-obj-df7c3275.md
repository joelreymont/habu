---
title: "Read an object's fields from one span"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-13T04:37:15.081241+02:00"
closed-at: "2026-11-13T04:49:22.285192+02:00"
---

Problem: a warm engine build spends about 450 ms of self time in field accessors while it checks and links its objects (674,833 WORDS and 366,735 EXTENTS rows over 74.6 MB): every 8-byte read goes src/habu/linker.f RD-U64 (:2739) through BYTES$ (:513), BUF:SPAN$'s live check and LE:U64@, and each probe step of PLACE-SLOT (:765) makes six PLACES @ calls through DYNAMIC-STORAGE:ADDRESS. Acceptance: HEADER-CK, PIECE-CK and LINK-OBJECT take OBJECT's span once and read fields at offsets in it, PLACE-SLOT reads a slot from one row address; the warm engine build's time falls by what that cost; warm bytes equal cold. Depends: none. Claim: agent=jerry worker workspace=.jj-ws.noindex/jerry-rd.

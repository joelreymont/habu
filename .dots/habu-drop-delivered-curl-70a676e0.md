---
title: Drop delivered CURL stream bytes
status: open
priority: 2
issue-type: task
created-at: "2026-10-28T17:19:55.951887+02:00"
---

Problem: src/lib/net/curl.f STREAM-START/NEXT accumulates the whole response body in an open_memstream (R.STREAM, REC-BUF-CELL; FINISH keeps it in R.BODY) and never drops bytes already delivered to the caller, so a long stream grows the heap without bound and each realloc leaves unwiped copies of delivered secrets. Found by the carl-curl lane (habu-recv-curl-answers-a10717b1). Acceptance: delivered bytes leave the loop's buffer once STREAM-NEXT copies them out (memory held is the undelivered window, not the stream so far); every buffer is wiped before free or realloc; a long-stream test shows bounded loop memory and exact delivery. Files: src/lib/net/curl.f, src/lib/net/curl-test.f. Depends: habu-recv-curl-answers-a10717b1 (shared wipe helpers). Claim: unassigned.

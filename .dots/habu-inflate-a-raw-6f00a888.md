---
title: Inflate a raw DEFLATE stream in steps
status: open
priority: 2
issue-type: task
created-at: "2026-11-26T14:09:47.526619+01:00"
---

Problem: Maki imports SolidWorks members of 794 MB raw DEFLATE that expand to 2.2 GB, and ZLIB has only one-shot inflates (src/lib/zlib.f INFLATE-RAW, INFLATE-FRAMED) on a task-local stream (tim, /tmp/tim-habu-van.md item 1). Acceptance: ZLIB:WITH-INFLATE-RAW and ZLIB:INFLATE-STEP inflate a stream fed in pieces into room given in pieces, the zlib suite shows it, and the scope ends the stream on return and on a throw. Depends: none. Claim: unassigned.

---
title: Load empty image name maps
status: closed
priority: 2
issue-type: task
created-at: "2026-10-16T11:16:54.815553+03:00"
closed-at: "2026-10-16T12:01:06.673346+03:00"
close-reason: Independent Astra CLEAR; header-only and one-row image maps load, and real AOT cache, REPL freshness and producer-cache fixtures pass with matching a118 engine/map.
---

The existing hb-build AOT cache E2E fails with7122 on a valid digest-bound header-only map: IMAGE-NAMES:INDEX-ROWS dereferences CODE-ROWS index0 before its empty sort, but no code rows allocated the dynamic buffer. Retained no-row and one-row map repros ~/.cache/tmp/aot-cache-bounds-20261016 differ only by row presence and exit67 versus0 on matching a118 engine. Skip buffer access when fewer than two code rows need sorting; keep map parsing/digest/ownership behavior. Existing header-only AOT cache E2E is the pre-code failure coverage; rerun it and the two narrow load controls, independent review.

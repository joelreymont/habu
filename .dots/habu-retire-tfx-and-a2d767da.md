---
title: Retire TFX and SVX before counters rewind
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T10:36:19.564064+02:00"
---

CG-23. src/core/sumtype.f:54-92 directly rewinds TFAM-N/SUMV-N; src/core/type-family.f:439-496,755-766 requires index retirement before those counters rewind (the outer restore does it at 2152-2164, the local declaration restore bypasses it). First CHECK:RUN correctly returns 70 for a bad declaration; a later valid declaration reusing the same tail hard-exits 76 'tfam: bad family id'. Fix: one registry-owned restore operation retires TFX/SVX then rewinds all relevant watermarks; remove direct counter restoration from the declaration layer.

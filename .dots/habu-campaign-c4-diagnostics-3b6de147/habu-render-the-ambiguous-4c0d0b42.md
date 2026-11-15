---
title: Reject a storage count out of range in the run
status: closed
priority: 2
issue-type: task
created-at: "2026-10-03T16:01:10.285236+03:00"
closed-at: "2026-11-16T04:07:34.348768+01:00"
---

Problem: `0 TYPED-BUFFER zb n` (and -1, and LAYOUT-BUFFER) ends bin/hb with "uncaught throw code 7121", rc 67: src/core/layout-buffer.f LBUF-VALIDATE and STORAGE-VALIDATE throw E-LAYOUT-BUFFER bare, where check.f writes E-BAD-STORAGE at the count token; the generated-name and signature buffers and the storage bind/defer/grid paths throw it bare too. Acceptance: all three readers print the same one record, rc 70; each other bare throw is shown unreachable and deleted, or its reproducer reported. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).

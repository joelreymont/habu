---
title: "An aio ticket kept from a file's top level is accepted in the program"
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T15:13:21.936926+02:00"
---

Problem: an aio ticket is a record index and the record's generation (src/lib/aio.f IDX>HANDLE :506-510, OWNED-CHECK :1049-1055). The process-state stack (unpushed, tip 259191a9) made the first chunk scratch (AIO-RECS :245) and deleted GEN-FLOOR, so every generation reads zero in a later file and in the program and START's RECS-CLEAR bumps each to 1 in both: a ticket kept in a TYPED-VARIABLE equals the program's first ticket over that record and AWAIT accepts it. Acceptance: a carried variable GEN-SERIAL is at least every generation a ticket holds (raised at REC-CLAIM under AIO-LOCK); REC-RESET and REC-GROW give a record a generation above it; fixture test/fixtures/file-objects/kept-aio.f, called from kept-main.f (build K of test/file-objects.f), prints 'aio refused', cold and warm, and printed otherwise before the change. curl needs no change: LOOP-START clears the fd table and TIMER-MS before the loop task runs, and LOOP-RUN stores WAKE-TICKET and TIMER-TICKET before any UNWAIT reads one (src/lib/net/curl.f:1492-1500, :1538-1541, :1667-1682). Files: src/lib/aio.f, test/fixtures/file-objects/kept-aio.f. Verify: suites aio-uring, curl-http, file-objects. Depends: none. Ownership: those files. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-idb (brief .jj-ws.noindex/jerry-wasm/.out/briefs/idb.md).

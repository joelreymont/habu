---
title: Decide which file publishers sync to disk
status: open
priority: 2
issue-type: task
created-at: "2026-11-05T13:20:20.006579+02:00"
---

Problem: since 234ddc96 every FS:ATOMIC-WRITE-FILE syncs the sibling and the directory (F_FULLFSYNC on macOS). The build cache writes its objects through it (tools/native-definition-cache.f, tools/native-unit-file.f, tools/keyed-image.f): an estimated 170 objects per cold macOS build at about 11 ms each, about 1.9 s, unmeasured; the cache already treats a damaged entry as a miss. The other way round, FS:REPLACE-STAGED (src/lib/fs-mutate.f:1120; callers tools/hb-build-lib.f, src/lib/svg-pdf.f, tools/build-fixpoint.f) and snap-lib's image publish (src/habu/snap-lib.f:559-579, RESERVE-SIBLING then RENAME-FILE) rename with no sync, so a crash can leave an installed hb or built image empty under its name (fsync review findings 5 and 6). Step 1, the lead's measurement: a cold-cache engine build (`rm -rf .out`; `bin/hb --load tools/native-build.f -- <out>/hb`) at f07bec12 and at 234ddc96 on spark, same engine both sides (DECISIONS "where timings run"), plus the count of syncs in one cold macOS build times a probe's per-call F_FULLFSYNC cost. Step 2: a design for which publishers sync, with that evidence; it changes failure recovery, so it goes to Joel before any worker. Files: per design. Verify: per design. Depends: none. Ownership: carl. Claim: unassigned.

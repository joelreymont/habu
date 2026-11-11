---
title: "test/heap.f and test/arena.f call FS:CLEANUP-TREE+ without requiring fs-mutate.f"
status: open
priority: 3
issue-type: task
created-at: "2026-11-11T10:49:39.491425+02:00"
---

Problem: test/heap.f:119 and test/arena.f:230 call FS:CLEANUP-TREE+ but neither requires src/lib/fs-mutate.f, which defines it; they load only because hb carries that file (tools/hb-build-core.f:18). Acceptance: each file requires src/lib/fs-mutate.f. Files: test/heap.f, test/arena.f. Verify: both tests rc 0. Depends: none. Ownership: the two files. Claim: agent=carl workspace=.jj-ws.noindex/carl-zip.

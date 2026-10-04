---
title: Publish DYNAMIC-BUFFER growth before unmapping
status: open
priority: 3
issue-type: task
created-at: "2026-10-03T18:10:14.142080+03:00"
---

Found by lane 449 forklocks: src/core/dynamic-storage.f RESERVE (:142) grows a buffer that already has registry capacity outside the registry lock and unmaps the old mapping before publishing the new one; a child forked inside that window (or any reader racing it) can touch unmapped memory. Acceptance: order growth so the new mapping is published before the old is unmapped (or hold the lock that lane 449 holds across fork), with a fork hammer that crashes children before and none after.

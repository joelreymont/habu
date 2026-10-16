---
title: Remove unreachable MAP tombstones
status: active
priority: 2
issue-type: task
created-at: "2026-10-16T11:17:41.919416+03:00"
---

MAP public API has no deletion and no production writer of deleted state; only private tests forge it. Delete state/counter/first-tombstone probe/reuse branches. Preserve public clear/get/set/collision/full-table/replacement/iteration behavior and existing meaningful tests. Do not remove documented MAP API.

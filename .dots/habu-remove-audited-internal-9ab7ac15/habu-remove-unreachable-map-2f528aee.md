---
title: Remove unreachable MAP tombstones
status: closed
priority: 2
issue-type: task
created-at: "2026-10-16T11:17:41.919416+03:00"
closed-at: "2026-10-16T11:30:15.153376+03:00"
close-reason: Removed unreachable tombstone state, count and probe bookkeeping while preserving public map behavior. Existing map runtime and check.f source tests passed; independent Astra reviewed every changed hunk and public operation with no material finding. Lead reviewed and retired workspace.
---

MAP public API has no deletion and no production writer of deleted state; only private tests forge it. Delete state/counter/first-tombstone probe/reuse branches. Preserve public clear/get/set/collision/full-table/replacement/iteration behavior and existing meaningful tests. Do not remove documented MAP API.

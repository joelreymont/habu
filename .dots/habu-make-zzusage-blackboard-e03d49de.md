---
title: Make zzusage, blackboard and kiba load on current Habu
status: active
priority: 2
issue-type: task
created-at: "2026-10-23T19:07:22.711208+03:00"
---

Their sources require lib/... but Habu moved libraries to src/lib/ in 6308d497213. One worker per repo in that repo's .jj-ws.noindex/jerry-load workspace; lead reviews and pushes each default branch. kiba converted to colocated jj 2026-10-23 (origin still https: the remote switch to SSH needs Joel's permission). zzusage local master carries 17 unpushed 2026-10-13 commits that ride along.

---
title: "Keep a union's branch relations only for their readers"
status: open
priority: 2
issue-type: task
created-at: "2026-11-28T13:41:59.141797+01:00"
---

Problem: every union keeps each branch's relation (ORIGIN-BRANCH via ITEM-BRANCHES) until the compile ends, though only OPTIONAL's per-branch join, per-branch grouping and walks read them; at the 512-branch union that is about 359 KB of the 1 MiB a compile may map. Acceptance: a union none of those reads keeps no branch relation past its fold, and TEST-HEAVY's union family's compile peak falls by that share with statements unchanged. Depends: none.

---
title: Share the guard and callback texts
status: active
priority: 2
issue-type: task
created-at: "2026-11-17T11:14:00.172697+01:00"
---

Problem: src/habu/habu1.f (GUARD-MSG$, B-TASK-GUARD, B-LOWER-GUARD, the CBT-DIE, sites) and src/habu/kernel-x64.f (GUARD-MSG*, MUTATION-GUARD,, TASK-GUARD,, LOWER-GUARD,, CB-DIE,, TASK-LIVE-RC) each hold the task-guard and callback messages, help texts and exit statuses ($4F, 106), so the two machines can drift; TASK-LIVE-RC's comment points at habu1.f. Acceptance: one copy, in src/habu/fault-record.f beside CRS-HELP$; both engines converge; tasking-threads and ffi-callback pass on both. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-xs1.

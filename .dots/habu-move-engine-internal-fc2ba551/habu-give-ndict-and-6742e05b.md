---
title: Give ndict! and retire-from one primitive row each
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T22:22:43.025190+01:00"
---

Problem: src/habu/prims.f states ndict! (:583-586) and retire-from in four private rows each, one per calling package; docs/architecture.md:19 says each primitive is one row. Acceptance: each has one row and the other packages import it with EXPORT. Depends: none. Claim: unassigned.

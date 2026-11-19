---
title: Give ndict! one primitive row
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T22:22:43.025190+01:00"
---

Problem: src/habu/prims.f states ndict! (:585-587) in three private rows, one per calling package; docs/architecture.md:19 says each primitive is one row. Acceptance: it has one row and the other packages import it with EXPORT. Depends: none. Claim: unassigned.

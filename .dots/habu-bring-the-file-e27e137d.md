---
title: Bring the file-objects suite under five minutes
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T09:19:21.124148+01:00"
---

Problem: test/file-objects.f alone took 348 s on spark (ownseal lane, 2026-11-17), over test/AGENTS.md's 'five at most' for the whole suite; each case runs full builds. Acceptance: file-objects runs under its share of the suite's budget with the same failures caught, its cost measured per case first. Depends: none. Claim: unassigned.

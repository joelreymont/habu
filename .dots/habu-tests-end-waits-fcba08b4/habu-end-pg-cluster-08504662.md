---
title: "End pg-cluster's waits without a clock"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-13T20:19:32.959709+01:00"
closed-at: "2026-11-13T21:29:13.687094+01:00"
---

Problem: test/db/pg-cluster.f, missing from the census, ends initdb, the server's start, each case engine and the closing stop at deadlines (TOOL-MS, CASE-MS, HB_PG_CASE_MS), and marks such a step late. Acceptance: those waits have no deadline, the late-step machinery and the case engine's exit-124 reading are gone, the signalled stop's QUIT-MS kill stays, and the pg suite passes. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-wPGC.

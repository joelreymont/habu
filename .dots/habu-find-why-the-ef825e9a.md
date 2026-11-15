---
title: Find why the low-speed dribble case fails once
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T18:14:01.606516+02:00"
---

Problem: src/lib/net/curl-test.f DRIBBLE-LOW-SPEED (:1725) failed once in about 44 whole-file runs on 2026-11-15: curl code 28 after 1000 ms, so a dribble served at 64 bytes every 100 ms (DRIBBLE-STEP, DRIBBLE-GAP-MS) was ended by a low-speed limit of 10 bytes a second over 1 second (LOW-RATE, LOW-SECONDS). Cause unknown. Acceptance: the cause found with evidence and fixed at its root; the case no longer fails. Depends: none. Claim: unassigned.

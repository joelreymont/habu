---
title: "Free the PG connection when PG:OPEN halts"
status: open
priority: 2
issue-type: task
created-at: "2026-11-02T18:22:35.323204+02:00"
---

Problem: src/lib/pg-rows.f:253-260 PG:OPEN runs OPEN-START AWAIT MATCH itself instead of CONNECT, so a halt during OPEN leaks the slot, the descriptor and the PGconn; CONNECT's halt path was fixed and tested in carl-pg (f4fb6c98, dot 7e09e8e4, reviewed ACCEPT; the reviewer confirmed this defect separately). Smallest fix (reviewer): OPEN configures and freezes, then calls CONNECT. Acceptance: a halt during PG:OPEN frees the attempt as CONNECT's does, tested beside HALT-CONNECT-CASES in the PG cluster suite; fails before, passes after. Files: src/lib/pg-rows.f, the PG cluster test. Verify: hb --load test/db/pg-cluster.f. Depends: 7e09e8e4 lands first. Ownership: unassigned. Claim: unassigned.

---
title: "Reset the checker's id maps with the ids"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-23T17:51:01.226317+01:00"
closed-at: "2026-11-24T16:34:34.617555+01:00"
---

Problem: ROWMAP and NMAP (src/core/checker.f:8657) hold variable ids NEW (:5574) gives out again, and only some callers of NEW reset them (CHECK-RESET, PE-OPEN, TFAM-CHILD-ROWS), so a map can name live variables of a later check. Acceptance: NEW clears both maps; no reset by hand follows a NEW. Depends: none. Claim: unassigned (its commit is on the parked stack lmrmpkys..xvroprym above tqsnwlwz, workspace .jj-ws.noindex/carl-infname).

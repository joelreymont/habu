---
title: "Delete E-ACTIVE's unreachable transaction check"
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T15:43:16.563205+01:00"
---

Problem: DECLARATION-TRANSACTION:OPEN's E-ACTIVE (rg in src/core) cannot fire: no caller opens a transaction while one is open. Acceptance: the check and its code are deleted and the engine builds itself. Depends: none. Claim: unassigned.

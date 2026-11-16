---
title: Delete the unread REACH clauses
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T13:49:36.930098+01:00"
---

Problem: src/habu/prims.f rows carry REACH clauses (e.g. get-current's `REACH dict`, prims.f:793) and REACH@ (prims.f:284) has no caller in src or tools. Acceptance: the clauses and REACH@ are deleted, or a reader is shown. Depends: none. Claim: unassigned.

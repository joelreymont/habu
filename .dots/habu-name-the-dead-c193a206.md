---
title: Name the dead token after a dead execute
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T10:19:40.369803+01:00"
---

Problem: E-DEAD-CODE after an execute that never returns names a stale token: checker.f RSEXEC sets DEADP without DEAD-OWNER!. Acceptance: the rejection names the execute; a checker-dead-path test fails first. Depends: none. Claim: unassigned.

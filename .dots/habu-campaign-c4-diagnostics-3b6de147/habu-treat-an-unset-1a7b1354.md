---
title: Treat an unset grid revoke as no owner
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T06:35:37.110032+01:00"
closed-at: "2026-11-16T04:07:33.838893+01:00"
---

Problem: src/habu/habu1.f BALLOT (:3150) and habu2.f's rollback (:1778) throw -8574 when the grid revoke cell is 0, which it is between checker.f:784 and check-walk.f:3116, where no grid rows exist. Acceptance: both branch to no owner, as GRID-BEFORE/GRID-AFTER do; the throw is gone. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).

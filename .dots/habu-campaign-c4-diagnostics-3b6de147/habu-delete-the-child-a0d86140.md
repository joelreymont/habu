---
title: Delete the child-shape and grid-owner checks
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T06:23:15.619366+01:00"
closed-at: "2026-11-16T04:07:36.381597+01:00"
---

Problem: src/habu/interpreter.f CHILD-BODY throws for a shape no caller passes; habu1.f and habu2.f test at run time for a grid-revoke operation the checker always installs. Acceptance: both deleted with the facts that rule them out; the engine converges. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).

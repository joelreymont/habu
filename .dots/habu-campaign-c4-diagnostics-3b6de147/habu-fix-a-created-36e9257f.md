---
title: "Fix a created word's grid past a child anchor"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T14:24:39.487545+01:00"
closed-at: "2026-11-16T04:07:36.176954+01:00"
---

Problem: in a definer 'create 1 , 2 , ARENA 3 , 4 , does>' (or HEAP) the created word gets the child's run as its storage grid: CHILD-CLOSE (src/habu/interpreter.f:2598) moves COMMA-BASE without closing the pending body, and RECORD-LINK-CREATED (src/core/checker.f:16176) re-pends at does>. Acceptance: the created word's grid is its own cells; CHECKER-STORAGE-GRID's unreachable checks are gone. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-w49grid.

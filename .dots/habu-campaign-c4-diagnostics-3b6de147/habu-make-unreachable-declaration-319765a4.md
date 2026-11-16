---
title: "Delete the replay stream's re-entry guard"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-14T18:05:37.840707+01:00"
closed-at: "2026-11-16T15:50:25.245585+01:00"
---

Problem: src/core/generated-declaration.f RP-CLAIM refuses a second replay stream with E-REPLAY-BUSY, whose EXPLAIN row says 'report this declaration as a Habu defect'; no input reaches it (a replayed body is never evaluated), and test/enum-decl-suite.f 23h reaches it only by forging an open stream. Acceptance: the guard, its code, its EXPLAIN row and the forged case are gone. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-w219.

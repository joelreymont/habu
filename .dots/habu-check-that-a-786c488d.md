---
title: "Check that a rewind's retired records stay retired"
status: active
priority: 2
issue-type: task
created-at: "2026-11-26T11:04:16.138345+01:00"
---

Problem: read, not reproduced: EMIT-DEF-OCC-OVERLAP's LAFTER (src/habu/habu1.f:4537-4556) gives an occurrence to every record LMATCH matches, which may include a record an earlier code rewind retired whose span still reaches past cp, and FRAME-ROLLBACK-TOKEN (src/habu/interpreter.f:3194) may raise cp after it was lowered past TOKEN-MARK. Either would let a record name code that later definitions overwrite. Acceptance: a reproducer shows each happen and its fix lands, or the code that rules each out is named. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-rewind.

---
title: Write one record for a does> clause clash
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T06:35:37.118826+01:00"
closed-at: "2026-11-16T04:07:33.701421+01:00"
---

Problem: checker.f:13098 CLAUSE throws a bare 78 when a does> definer's clause name is taken (--verify-only on `: DRD;does ; : DRD create , does> ;`), while interpreter.f DEF-DOES-NAME writes its own record inline; interpreter.f:1239 PKG-CLAUSE-ROOM passes the clause name as its token, so its record is unplaced. Acceptance: one writer, placed at the source token, under the three readers, rc 70. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).

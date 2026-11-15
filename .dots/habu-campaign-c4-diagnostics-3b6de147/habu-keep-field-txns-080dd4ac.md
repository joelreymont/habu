---
title: Keep field transactions from programs
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T15:00:05.745245+01:00"
closed-at: "2026-11-16T04:07:35.717866+01:00"
---

Problem: TYPE-FIELD-OWNER (src/core/type-family.f:2049-2187) is a public top-level package, so a program can call OPEN, ADD, COMMIT, FINALIZE and ROLLBACK and nest field frames; that is the only input its frame stack, tokens and TX-PARENT-REQUIRE checks guard (engine paths stay at depth 1: the load-time REGISTER-* words run in sequence, TFAM-FIND dies before ER-BODY at depth > 0, DEV-OPEN runs once per declaration). No consumer or doc calls it; test/name-length-test.f:160-168 reaches ADD's long-name rejection through it. TX-CELLS-FOR (:2156) has no caller. Acceptance: no program reaches the field transaction words; the engine holds one frame directly, with no token; TX-CELLS-FOR is deleted; the long-name case is reached through a declaration. Depends: habu-hold-one-declaration-0d896f36. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).

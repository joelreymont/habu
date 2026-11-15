---
title: "Write a cast signature's help through SGBAD-FIX"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T20:50:18.937945+01:00"
closed-at: "2026-11-16T04:07:33.812960+01:00"
---

Problem: src/core/check-walk.f CHECKER-IDENTITY:PARSE (:2003) writes its own unknown-type and arity help through CAST-ERROR:RAISE, so cast: X ( ptr -- n ) is told to declare the type, not to spell a pointer ^T, while a definition's and a stored signature's record share checker.f SGBAD-FIX. Acceptance: that cast's record helps with ^u8; one help writer per signature verdict. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).

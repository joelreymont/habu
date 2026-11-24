---
title: "INTERN-SOURCE's origin is always -1"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T08:09:25.724740+02:00"
---

Problem: INTERN:INTERN-SOURCE (src/habu/intern.f:179-180) always answers -1 as its origin, so the origin its callers file (src/habu/t0.f:1351 :1356 :1550) carries nothing. Acceptance: INTERN-SOURCE's callers call INTERN:STRING and the origin column goes if nothing reads a value other than -1 (t0.f is jerry's: advise, do not edit). Depends: none. Claim: unassigned.

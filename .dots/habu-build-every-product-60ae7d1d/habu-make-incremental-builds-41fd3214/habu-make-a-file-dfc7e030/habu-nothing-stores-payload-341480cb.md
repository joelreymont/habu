---
title: "INTERN-SOURCE's origin is always -1"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T08:09:25.724740+02:00"
---

Problem: INTERN:INTERN-SOURCE (src/habu/intern.f:180-182) always answers -1 as its origin, so the origin its callers file (src/compiler/native/elaborate.f:981 :1009 :3125 :4317, fetch.f:97, src/habu/t0.f:1252 :1257 :1454 :1481) carries nothing. Acceptance: INTERN-SOURCE's callers call INTERN:STRING and the origin column goes if nothing reads a value other than -1 (t0.f is jerry's: advise, do not edit). Depends: none. Claim: unassigned.

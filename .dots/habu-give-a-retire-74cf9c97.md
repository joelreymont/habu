---
title: Give a retire by string its fact
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T17:22:57.479355+01:00"
---

Problem: XREF:UNDEFINE-NAME (src/habu/xref.f:617, public) carries no checker fact, so the verifier does not follow it: with `: U ( -- ) s" X" XREF:UNDEFINE-NAME ;  U  : P ( -- n ) X ;` the load rejects E-UNDEFINED at X while tools/check.f --verify-only exits 0. Acceptance: the three readers give the load's record for that program; a test pins it. Depends: none. Claim: unassigned.

---
title: Reject a does> word called with no created word
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T07:46:08.878406+01:00"
closed-at: "2026-11-16T04:07:34.691730+01:00"
---

Problem: the checker accepts : X ( -- ) does> ( -- n ) @ ; X, which stops at run time with the kernel's one-line E-DOES-UNCREATED (habu2.f:667 EMIT-NONE); in an evaluated string the stop has no line. Acceptance: the checker rejects such a call with a placed record before it runs, and the run-time stop is deleted. Depends: the hunt in .jj-ws.noindex/jerry-does. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).

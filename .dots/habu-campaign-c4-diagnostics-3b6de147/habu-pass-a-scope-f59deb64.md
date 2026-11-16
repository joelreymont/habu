---
title: Pass a scope its root with its path
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T14:22:09.394607+01:00"
---

Problem: SOURCE-ROOT:WITH-BYTES and WITH-SUPPLIED (src/core/include.f) enter RESOLVED-ROOT$, the owner root whatever resolve ran last set, not one their caller passes; their caller (src/habu/verify-source.f:3200-3212 COMPOSE-OPEN) resolves separately first. A program that calls either with no resolve before it, as in `hb file.f`, enters an empty root and gets error[E-PATH-RANGE] 'a path is empty' naming no word. Acceptance: each takes the root its path resolved under from its caller (or resolves the path itself), and a program-run call with an existing file loads it under that root. Depends: none. Claim: unassigned.

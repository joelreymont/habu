---
title: Build a program that requires the XML reader
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T13:37:34.576395+01:00"
---

Problem: a stripped program that requires src/lib/xml/c2.f fails to build with E-SEALED-PACKAGE at src/lib/xml/c2-memory.f:15: the checker creates package C2-MEM (src/core/type-family.f:3425), the build seals it, and the build's APP-BODY never loads c2-memory.f, so its words land in a sealed package. hb --load of the same program runs. Acceptance: a program that requires c2.f builds, cold and warm byte-equal, and runs as under --load; a test pins the build. Depends: none. Claim: unassigned.

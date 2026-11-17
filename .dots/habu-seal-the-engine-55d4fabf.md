---
title: "Seal the engine's packages in a load as a build does"
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T09:19:21.090110+01:00"
---

Problem: a --load runs program source with the packages its engine's files made still open, while a build seals them first (docs/forth-card.md, 'A build seals every package its engine files made'). So src/lib/process-path.f's reopening of engine package PROCESS loaded in every suite and failed only in tim's --repl build of a file requiring src/lib/spawn.f (E-SEALED-PACKAGE at spawn.f:21). Acceptance: a load seals what a build seals: a library file that opens a package an engine file made is rejected in --load and tools/check.f as in a build. Depends: none. Claim: unassigned.

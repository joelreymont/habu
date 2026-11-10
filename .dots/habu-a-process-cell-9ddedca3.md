---
title: A process cell keeps its build-time value in a built program
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T11:33:21.475493+02:00"
---

Problem: IMAGE-LIFECYCLE:PROCESS-CELLS has had no caller since 1d97c3f2, so a cell declared with IMAGE-LIFECYCLE:PROCESS-CELL is no longer zeroed in a built program. Cases /Users/joel/Work/reviews/habu-10x-2026-11-10/owned/miss.f (the program prints PC=42, the value its top level stored at build time) and exit1.f (an exit action added at build time runs in the program); fs-mutate's cleanup count (src/lib/bearer.f:467) is the same kind of cell. Fix not decided: src/compiler/aot-x64.f:237 says scratch-cell-mark cannot run in a stripped program. Found by the owned-cells hunt. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: a hunt says how a process cell, one declared lazily included, is given its start value in a built program on each build path; then a built program starts with every process cell at its declared start value: miss.f prints PC=0 and exit1.f runs only the run-time action. Files: src/lib/image-lifecycle.f, the cross-build's data writer, src/compiler/aot-x64.f. Verify: both cases built and run; a test through the real build path that fails on the base. Depends: none. Ownership: IMAGE-LIFECYCLE and the build's data. Claim: agent=bob workspace=.jj-ws.noindex/habu-a-process-cell-9ddedca3 (hunt)

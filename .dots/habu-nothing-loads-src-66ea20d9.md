---
title: Nothing loads src/habu/aot-x64.f
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T12:07:44.404787+02:00"
---

Problem: the process-cell hunt found no file that loads src/habu/aot-x64.f; its comment at :237 on scratch-cell-mark describes a path that no longer runs. Read, not run. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: rg shows whether anything requires or includes the file; if nothing does it is deleted with its tests and mentions, else the dot records who loads it. Files: src/habu/aot-x64.f. Verify: rg; an engine build and the suite after the deletion. Depends: none. Ownership: the x86-64 build. Claim: unassigned.

Claim: agent=bob workspace=.jj-ws.noindex/habu-nothing-loads-src-66ea20d9

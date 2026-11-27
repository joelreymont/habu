---
title: Map no alias for a stripped x86-64 image
status: open
priority: 2
issue-type: task
created-at: "2026-11-27T13:47:56.528471+01:00"
---

Problem: a stripped arm64 image has no code alias (delta 0) and maps its region with no second view (src/habu/habu2.f EM-MMAP-CODE-REGION, habu1.f:2499 'Delta 0 = no alias'), but x86-64's CODE-REGION, (src/habu/boot-x64.f:127-137) maps the alias on every start, a stripped program's included, though x86-64 FORK, already has the delta-0 path (src/habu/kernel-x64.f:1057). Acceptance: a stripped x86-64 program starts with delta 0 and one view, as arm64 does; its fork takes the plain path. Depends: habu-start-under-any-a094243c. Claim: unassigned.

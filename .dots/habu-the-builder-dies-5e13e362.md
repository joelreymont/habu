---
title: The builder dies of SIGSEGV when a build fails after an exit action
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T11:33:21.488651+02:00"
---

Problem: building /Users/joel/Work/reviews/habu-10x-2026-11-10/owned/heap2.f or clean1.f, whose top level calls AT-EXIT:ADD or FS:CLEANUP+ and whose build then fails, ends the builder with SIGSEGV (status 139); heap1.f, the same without the action, exits 74. Cause not determined. Found by the owned-cells hunt. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: a root-cause hunt names the faulting word and why; then a failed build exits with its own status and message whatever the top level registered. Files: src/lib/at-exit.f, the build driver (tools/hb-build.f and what it loads). Verify: both cases exit as heap1.f does; a test that fails on the base. Depends: none. Ownership: the build driver's failure path. Claim: agent=bob workspace=.jj-ws.noindex/habu-the-builder-dies-5e13e362 (hunt)

---
title: "GROW and ROW-GROW at build time publish into the host's cells"
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T20:51:46.582301+01:00"
---

Problem: src/lib/ffi-callback.f:113 GROW and src/lib/task.f:870 ROW-GROW publish the new table with data-base CB-XTS/CB-ROWS atomic!; the rollback review (claim 5) says a build that loads them points the building engine's callback and task-row cells at a table in the build's dictionary. Unverified: the second review found neither runs while hb builds itself, and the linker's MOVE-IN (src/habu/linker.f:5090) may already make data-base the build space. Acceptance: a reproducer that grows each table inside a build answers whether the host's CB-XTS and CB-ROWS change; if they do, they no longer do, and the ffi-callback and tasking suites pass. Depends: none. Claim: unassigned.

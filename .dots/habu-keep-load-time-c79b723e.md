---
title: Keep load-time scratch out of program images
status: active
priority: 2
issue-type: task
created-at: "2026-11-25T15:52:54.089537+01:00"
---

Problem: a program image keeps FS's stat record (FS:STAT-BUF, the FS-ABI band) from its load, so two builds of one source differ by a file's access time. Acceptance: every per-call engine band starts zero in a built program, and the reproducer's two builds are the same. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-band.

---
title: "Give back hb's records a build retired"
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T23:22:08.077731+01:00"
---

Problem: a build that retires one of hb's own records (a program's 'undefine dup') stamps it through src/habu/xref.f RETIRE, and the linker's close (src/habu/linker.f RESTORE, FORGET-SPACE) restores hb's cells and drops the build's records but leaves that stamp, so the process that ran the build no longer finds the word afterwards (read, not run). Acceptance: after a build's close, every hb record the build retired is found again in the same process, shown by a test. Depends: none. Claim: unassigned.

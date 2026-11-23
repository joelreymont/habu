---
title: "Name Linux's EAGAIN once"
status: open
priority: 2
issue-type: task
created-at: "2026-11-23T19:47:04.094993+02:00"
---

Problem: three packages define their own private EAGAIN: src/lib/fd.f:133, src/lib/task.f:119 (TASK-EAGAIN) and, once it lands, src/lib/inotify.f (Linux's 11 on arm64 and x86-64); a library reading errno must copy the constant because none is public. Found by the review of the inotify library. Acceptance: one public EAGAIN, per target as the card's constant rules say, used by all three; no private copy remains. Depends: none. Claim: unassigned.

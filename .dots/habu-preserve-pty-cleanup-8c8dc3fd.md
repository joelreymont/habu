---
title: Preserve PTY cleanup failure
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-13T03:14:28.574394+02:00\""
---

Full context: pending lib/process-pty.f KILL-REAP drops kill status and catches/drops wait errors, can clear lifecycle state without proving reap, and loses cleanup failure behind a primary error. Represent termination/reap structurally; clear PID only after successful reap; accept ESRCH only when wait proves child reaped; preserve primary and cleanup failures. Depends on bounded PTY completion.

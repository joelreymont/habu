---
title: Supervise only the PTY target
status: active
priority: 2
issue-type: task
created-at: "2026-10-16T11:17:41.588497+03:00"
---

process-pty-io.f forks anchor and monitor solely to satisfy process-pty-handle registry shape; helpers only wait on pipes. Remove dummy processes, keepalive pipes, watches and registry fields. Preserve public handle ownership, target exit/signal/I/O, error cleanup and descendant teardown. Inspect existing process-pty registry, pipe and terminal smoke tests first; run real process lifecycle tests and independent review.

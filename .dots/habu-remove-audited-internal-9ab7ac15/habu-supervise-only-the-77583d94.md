---
title: Supervise only the PTY target
status: closed
priority: 2
issue-type: task
created-at: "2026-10-16T11:17:41.588497+03:00"
closed-at: "2026-10-16T11:36:07.575418+03:00"
close-reason: Removed helper children and keepalive plumbing; registry, pipe, terminal smoke and process-tree lifecycle tests pass. Independent adversarial review and focused follow-up found no remaining material issue.
---

process-pty-io.f forks anchor and monitor solely to satisfy process-pty-handle registry shape; helpers only wait on pipes. Remove dummy processes, keepalive pipes, watches and registry fields. Preserve public handle ownership, target exit/signal/I/O, error cleanup and descendant teardown. Inspect existing process-pty registry, pipe and terminal smoke tests first; run real process lifecycle tests and independent review.

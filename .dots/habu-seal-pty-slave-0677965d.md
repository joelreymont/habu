---
title: Seal PTY slave descriptors
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-13T03:14:28.577255+02:00\""
---

Full context: pending lib/process-pty.f sets CLOEXEC only on the master. Set FD_CLOEXEC immediately on each opened slave before spawn; dup2 stdio targets survive while the original descriptor cannot leak across exec. Add descriptor inheritance regression.

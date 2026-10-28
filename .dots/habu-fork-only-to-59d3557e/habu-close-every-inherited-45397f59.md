---
title: Close every inherited fd before exec
status: open
priority: 2
issue-type: task
created-at: "2026-10-28T19:04:35.684575+02:00"
---

tim's finding: src/lib/process-fork.f CLOSE-EXCEPT2 and src/lib/process-pty-io.f close only fds below MAXFD ($400), a fixed limit; a death-pipe write end at fd >= 1024 survives in later children and so SPAWN-REAPER never sees EOF when its owner dies (reproduce: dup the write end to an fd >= 1024, fork a child, SIGKILL the owner). Fix with the fork-to-exec work: every fd Habu opens is close-on-exec, or the child closes all fds above its keep set without a limit. Also (phil): src/lib/process-pty-io.f:173-178 IO-TARGET-CHILD reads the launch gate before IO-CLOSE-HIGH, so it holds the inherited gate write end, and a target stranded before LAUNCH becomes an orphan. Claim: unassigned.

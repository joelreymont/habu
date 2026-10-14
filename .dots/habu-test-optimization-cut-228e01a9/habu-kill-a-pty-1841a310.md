---
title: "Kill a PTY session's whole process tree"
status: closed
priority: 2
issue-type: task
created-at: "\"2026-10-02T23:12:52.709519+02:00\""
closed-at: "2026-10-14T19:29:05.577967+03:00"
close-reason: "Implemented in b55efd63: PTY harness and I/O teardown kill the child process tree, while IO-KILL-REAP remains non-throwing; sleeper-holds-pipe regressions were red before and green after, with focused PTY and process suites passing."
---

Problem (lane 361 r4-killtree, 15356c6d): captures now end early with PROC-TREE:KILL-TREE, but PTY sessions still kill only the child's pid: src/lib/pty-harness.f:284 KILL-REAP and src/lib/process-pty-io.f:103 IO-KILL-REAP, so a process the PTY child started outlives the session. IO-KILL-REAP must not throw and KILL-TREE can. Acceptance: both kill the child's tree (a non-throwing form of the walk for IO-KILL-REAP, or the throw caught and reported where it cannot propagate), a PTY test whose child spawns a sleeper holding a pipe sees EOF within 5 s after the kill, seen failing first. Files: src/lib/pty-harness.f, src/lib/process-pty-io.f, src/lib/process-tree.f.

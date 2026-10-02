---
title: "Kill a PTY session's whole process tree"
status: open
priority: 2
issue-type: task
created-at: "2026-10-02T23:12:52.709519+02:00"
---

Problem (lane 361 r4-killtree, 15356c6d): captures now end early with PROC-TREE:KILL-TREE, but PTY sessions still kill only the child's pid: lib/pty-harness.f:284 KILL-REAP and lib/process-pty-io.f:103 IO-KILL-REAP, so a process the PTY child started outlives the session. IO-KILL-REAP must not throw and KILL-TREE can. Acceptance: both kill the child's tree (a non-throwing form of the walk for IO-KILL-REAP, or the throw caught and reported where it cannot propagate), a PTY test whose child spawns a sleeper holding a pipe sees EOF within 5 s after the kill, seen failing first. Files: lib/pty-harness.f, lib/process-pty-io.f, lib/process-tree.f.

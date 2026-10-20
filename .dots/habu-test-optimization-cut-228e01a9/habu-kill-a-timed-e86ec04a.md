---
title: "Kill a timed-out capture's whole process tree"
status: closed
priority: 3
issue-type: task
created-at: "2026-10-02T09:40:27.259949+02:00"
closed-at: "2026-10-14T19:34:45.606086+03:00"
close-reason: "Implemented in e92fa897: all early capture ends kill and reap the child process tree through one path, retaining timeout and capture results; five failing-before sleeper cases and focused suites passed. Walk failures retain the capture outcome under 9866db1e."
---

Problem (r4-chkorphan lane, bd41cb21, dot 2253adf0): src/lib/process.f PROC-KILL-CAPTURE and PROC-REAP-CAPTURE-TIMEOUT (~:425) kill only the child's pid on a deadline, so a child that started processes in their own groups (check.f's run child, a test subject) leaves them running after E-PROC-TIMEOUT. bd41cb21's signal answer already kills the tree with PROC-TREE:KILL-TREE. Acceptance: a capture that times out ends the child's whole tree (same mechanism, one copy), reaps the child, and still reports E-PROC-TIMEOUT with its capture; a case through the real load path whose subject spawns a sleeper in its own group and outlives the deadline, seen failing first (test/check-signal-subject.f is the pattern). Base: after bd41cb21 lands.

Lane 317 (r4-reaperfork, 45a476b4) adds: src/lib/process.f PROC-THROW-CAPTURE, now also the path of a refused reaper arm (PROC-CAPTURE-PID!), kills only the child's pid, not its process group, like the timeout path: a grandchild forked between the spawn and the refused arm survives. One fix covers both paths.

Lane 319 (r4-chkout, 9ef2f891) adds: when check.f's run capture overflows (E-PROC-TRUNCATED), src/lib/process.f:427-431 PROC-KILL-CAPTURE kills only the direct child; a process the checked program started keeps running, unlike check.f's signal path, which kills the tree. check.f cannot kill the tree afterwards (the child is reaped). Same fix point as the timeout path.

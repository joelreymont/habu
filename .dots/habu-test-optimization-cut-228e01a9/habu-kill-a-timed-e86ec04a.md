---
title: "Kill a timed-out capture's whole process tree"
status: open
priority: 3
issue-type: task
created-at: "2026-10-02T09:40:27.259949+02:00"
---

Problem (r4-chkorphan lane, bd41cb21, dot 2253adf0): lib/process.f PROC-KILL-CAPTURE and PROC-REAP-CAPTURE-TIMEOUT (~:425) kill only the child's pid on a deadline, so a child that started processes in their own groups (check.f's run child, a test subject) leaves them running after E-PROC-TIMEOUT. bd41cb21's signal answer already kills the tree with PROC-TREE:KILL-TREE. Acceptance: a capture that times out ends the child's whole tree (same mechanism, one copy), reaps the child, and still reports E-PROC-TIMEOUT with its capture; a case through the real load path whose subject spawns a sleeper in its own group and outlives the deadline, seen failing first (test/check-signal-subject.f is the pattern). Base: after bd41cb21 lands.

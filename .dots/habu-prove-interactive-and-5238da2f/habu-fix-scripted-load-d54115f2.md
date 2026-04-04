---
title: Fix scripted load specializer trap
status: closed
priority: 1
issue-type: task
created-at: "\"2026-04-04T17:26:09.814065+02:00\""
closed-at: "2026-04-04T17:30:59.571827+02:00"
close-reason: "proved: script-mode nested maxima load makes forward progress through real files; old trap no longer reproduces after direct argv publication + defstruct slot emission fix"
---

Problem: repo-local scripted Maxima load repro .tmp/script-step2.lisp now reaches PRE-LOAD-ALL, then traps with rc=132 inside src/compiler/passes/p07c_specialize.zig:124 specializeWithEnv on a bad IR pointer. Bench load survives, so this is a script/load path compiler-state bug, not a Maxima semantic error. Fix the rooted/allocator/compiler-state issue generically in the scripted load path, then prove with ./zig-out/bin/habu .tmp/script-step2.lisp and the canonical tools/maxima-rtest.lisp entrypoint.

---
title: proc-pty flakes under pool load
status: open
priority: 2
issue-type: task
created-at: "2026-08-16T01:20:46.469752+02:00"
---

One-off red in the native engine runtime slice during a full test/run.f (pool=10) on candidate 88e31d8d: test/proc-pty.f F10, rbuf carried garbage-looking 4834891362997894251, child exit 1. NOT reproducible: same commit 3/3 green standalone, the implementing lane's full battery green, an idle-box full rerun green with RC read from the engine directly. Same class as cad-replay 8be2ba00 (load-sensitive test reds the gate only when the box is busy). Diagnose with evidence when it recurs: which assert F10 is, what writes rbuf, whether pty read timing under contention can return a partial/stale buffer. Do not bump timeouts without proof of mechanism.

---
title: Trace first ANSI TypeMismatch
status: closed
priority: 2
issue-type: task
created-at: "\"2026-02-17T20:38:12.923459+01:00\""
closed-at: "2026-02-20T21:17:53.768100+01:00"
close-reason: Initial ANSI TypeMismatch trace completed during loader stabilization
---

src/tests/integration.zig and tools/ansi/run.sh: capture first uncaught ANSI TypeMismatch with deterministic reproducer; include failing form, stack site, and condition payload; prerequisite for root patch.

---
title: Trace first ANSI TypeMismatch
status: open
priority: 2
issue-type: task
created-at: "2026-02-17T20:38:12.923459+01:00"
---

src/tests/integration.zig and tools/ansi/run.sh: capture first uncaught ANSI TypeMismatch with deterministic reproducer; include failing form, stack site, and condition payload; prerequisite for root patch.

---
title: "Engine: top-level stack underflow dies SIGABRT"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T06:03:00.279377+02:00"
---

Observed during cad-4 (worker report): a stack-underflowing top-level interpreted line crashes bin/hb with SIGABRT instead of a named diagnostic. Definitions are checker-covered; top-level interpreted lines are not, but the ENGINE should still fail with a clean E-UNDERFLOW-style diagnostic naming the line, never a signal. Investigate per docs/debugging.md (debugger evidence first), add the guard at the interpreter loop boundary, negative test in the engine suite. Repro: a top-level line consuming more cells than the stack holds in a --load file.

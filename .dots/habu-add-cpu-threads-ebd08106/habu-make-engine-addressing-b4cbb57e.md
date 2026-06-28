---
title: Make engine addressing all-x20-relative (no absolute DATA-VA) for per-task regions
status: open
priority: 1
issue-type: task
created-at: "2026-06-28T13:27:59.753160+02:00"
---

MEASURED blocker for threads Step 1: a run-in-region prototype that repoints x20 to a fresh region crashes (SIGBUS) even for an EMPTY xt, while run-in-stack (x19-only swap) works. Cause: the engine mixes absolute DATA-VA references with x20-relative ones, so x20 at a different VA desyncs them. This is codex's 'user-area base authoritative for every access' invariant. Work: audit src/habu/* + src/core/* for absolute DATA-VA / RBASE-VA refs to runtime cells, convert all to x20(base-register)-relative; verify with the empty-xt run-in-region test, then proceed to run-in-region + pthread trampoline + TASK/ACTIVATE. Evidence: docs/threads.md Build progress.

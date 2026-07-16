---
title: Frame one PTY terminal result
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-16T14:27:03.367459+02:00\""
---

Full context: lib/process-pty-io.f SUP-TARGET-DIED writes a positive outcome before SUP-RUN performs fallible descriptor cleanup. If cleanup then fails, SUP-FAIL appends primary/mask frames, but OP-READ-STATUS consumes only the first positive frame and OP-WAIT-SUP reduces the supervisor exit to E-PROC-WAIT, losing the true cleanup error and evidence. Redesign the supervisor terminal protocol so all fallible teardown completes before exactly one terminal result is published, carrying either outcome or primary error plus cleanup mask. Add deterministic injected cleanup-failure and short-frame regressions; update docs and run PTY, protocol, typed diff, trust, host, filemap, candidate phase16, and full cold gate.

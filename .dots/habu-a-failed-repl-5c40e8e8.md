---
title: A failed REPL line rolls back twice while a task is live
status: active
priority: 2
issue-type: task
created-at: "2026-11-13T17:58:10.043820+01:00"
---

Problem: src/habu/interpreter.f REPL-LINE (:2795) calls REPL-RECOVER (:2810), so FRAME-ROLLBACK-CODE (:2721), on every failed line, also on arm64 where the kernel's throw recovery has already rolled back; EVAL-FRAMED-RANGE runs its recovery only when it owns the frame. With a task live the kernel keeps NDICT, so :2727's ndict! refuses and the session exits 79 (inferred by the rollback review, not run). Acceptance: on a PTY REPL with a task live, a line that defines a word and then fails leaves the session running with the word taken back; a case in test/repl-address-cell-rollback.f holds it. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-replhunt.

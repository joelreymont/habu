---
title: Return to the REPL after Ctrl-C
status: open
priority: 1
issue-type: task
created-at: "2026-08-24T21:25:01.815610+02:00"
---

Problem: Ctrl-C cancels an edited line, but SIGINT during a running word terminates bin/hb. Acceptance: through the production PTY path, start the REPL, run a nonterminating checked word, send Ctrl-C, observe one clean prompt, then execute another word successfully; the interrupted evaluation publishes nothing and terminal state is restored. Reuse the existing evaluator rollback and terminal code; add no mode, supervisor, signal framework, or alternate REPL. Files: src/habu/repl.f, the existing OS signal/terminal seam, and test/proc-pty.f. Verify: one focused PTY regression plus the existing REPL recovery cases. Depends: none. Ownership: execution-time REPL interruption only.

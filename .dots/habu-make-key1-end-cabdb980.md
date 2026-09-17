---
title: Make KEY1 end the line on a zero-length read
status: active
priority: 2
issue-type: task
created-at: "2026-09-17T14:27:55.600923+03:00"
---

Problem: src/habu/repl.f:77 KEY1 is '0 KB 1 read drop KB c@': it drops the read count, so a zero-length read (EOF on the terminal, a hung-up pty) replays the previous key byte instead of ending the line, and a read error does the same (found by the pty-barriers lane, 2026-09-17, while tracing the ^D hang). Acceptance: KEY1 (or its caller) treats a read that returns 0 or below as end of input, the REPL leaves the line editor cleanly on it, and a regression drives the REPL over a pty, closes the master mid-line and asserts the child exits instead of re-running the last key; the raw-mode restore still happens. Files: src/habu/repl.f, a pty regression under test/. Verify: the regression; test/proc-pty.f; test/process-pty-tty-smoke.f; test/run.f (engine change). Depends: none. Ownership: REPL. Claim: agent=hazel-key1-eof workspace=.jj-ws/hazel-key1-eof.

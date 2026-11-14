---
title: Let one thread end the process while another runs exit actions
status: open
priority: 2
issue-type: task
created-at: "2026-11-14T15:05:06.353203+01:00"
blocks:
  - habu-run-exit-actions-54c95381
---

Problem: the arm64 trampoline (src/habu/habu2.f EMIT-EXITHOOK) reads the exit vector and clears it in two steps, and a thread that finds it clear calls exit_group at once. So a second thread's die or uncaught throw, while the first is running AT-EXIT actions, ends the process before those actions finish (a cleanup tree is left behind), and two threads that both read it armed run the actions at once. Acceptance: while one thread runs the exit actions, another thread's exit waits for that exit and never ends the process early; a test where a task dies while main's slow action runs shows the action finishing. Depends: habu-run-exit-actions-54c95381. Claim: unassigned.
